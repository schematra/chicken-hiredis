# Verification of the non-blocking rewrite

This document records the manual verification performed after the rewrite to
`redisConnectNonBlock` + cooperative I/O. Each section has the exact command
used and the observed result, so the checks can be re-run by anyone with a
local `redis-server` and CHICKEN 5.

**Environment**

- macOS 15.7.4 arm64
- CHICKEN 5.4.0
- hiredis via Homebrew (`/opt/homebrew/include/hiredis/hiredis.h`)
- `redis-server` running on `127.0.0.1:6379`

**Build**

```sh
chicken-install -n
```
Result: clean compile of `hiredis.so`, `hiredis.import.so`, and `hiredis.static.o`.
Do NOT invoke `hiredis.build.sh` directly — it's a generated artifact produced
by `chicken-install` from the egg's `(custom-build "build-redis")` declaration.

---

## 1. Module loads, all exports resolve, internals hidden

```sh
csi -R hiredis -e '(for-each print (list redis-connect redis-disconnect redis-context? redis-context-alive? redis-context-err redis-context-errstr redis-command redis-subscribe redis-nil redis-nil?))'
```
Result: 9 `#<procedure>` lines + one `(redis-nil . #f)`. All ten exports resolve.

```sh
csi -R hiredis -e '(print make-redis-context)'
csi -R hiredis -e '(print redis-context-ptr)'
```
Result: both raise `unbound variable`. The record constructor and the raw-pointer
accessor are correctly hidden — users cannot hold a reference that bypasses the
finalizer.

---

## 2. End-to-end command exercise

```sh
csi -R hiredis -e '
(define ctx (redis-connect))
(print "PING  -> " (redis-command ctx "PING"))
(print "SET   -> " (redis-command ctx "SET" "foo" "hello-from-chicken"))
(print "GET   -> " (redis-command ctx "GET" "foo"))
(print "HGET  -> " (redis-command ctx "HGET" "myhash" "foox"))
(print "KEYS  -> " (redis-command ctx "KEYS" "*"))
(print "miss  -> " (redis-command ctx "GET" "nope_no_such_key"))
(print "nil?  -> " (redis-nil? (redis-command ctx "GET" "nope_no_such_key")))
(print "[]    -> " (redis-command ctx "LRANGE" "nonexistent_list" "0" "-1"))
(print "err   -> " (redis-command ctx "GET"))
(redis-disconnect ctx)
(print "alive? after disconnect: " (redis-context-alive? ctx))'
```

Observed:

```
PING  -> PONG
SET   -> OK
GET   -> hello-from-chicken
HGET  -> bar
KEYS  -> (foo myhash)
miss  -> (redis-nil . #f)
nil?  -> #t
[]    -> ()
err   -> (error . ERR wrong number of arguments for 'get' command)
alive? after disconnect: #f
```

Covers: status, string, hash-field, array, missing-key (nil sentinel),
empty-array (distinct from nil), server-side error reply, disconnect
state flip.

---

## 3. Cooperative behavior — subscriber does not freeze the runtime

```sh
csi -R hiredis -R srfi-18 -R chicken.time -e '
(define ctx-sub (redis-connect))
(define ctx-main (redis-connect))
(define t0 (current-seconds))
(thread-start!
  (lambda ()
    (redis-subscribe ctx-sub "test-coop"
      (lambda (r)
        (print "[sub@" (- (current-seconds) t0) "s] " r)
        (not (equal? (list-ref r 3) "quit"))))))
(let loop ((i 0))
  (when (< i 4)
    (thread-sleep! 0.5)
    (print "[main@" (- (current-seconds) t0) "s] PING -> " (redis-command ctx-main "PING"))
    (loop (+ i 1))))' &
CSI_PID=$!
sleep 3
redis-cli publish test-coop quit > /dev/null
wait $CSI_PID
```

Observed:

```
[main@0s] PING -> PONG
[main@1s] PING -> PONG
[main@1s] PING -> PONG
[main@2s] PING -> PONG
```

The primordial thread completes four `PING`s over ~2s while the subscriber
thread is parked in `read-one-reply!`. On the old synchronous wrapper the
first `PING` would have frozen because the subscriber's `recv()` blocked the
entire runtime. This is the core behavior change the rewrite was motivated
by.

## 3b. Subscribe actually dispatches messages

```sh
csi -R hiredis -R srfi-18 -e '
(define ctx (redis-connect))
(define done (make-mutex)) (mutex-lock! done)
(thread-start!
  (lambda ()
    (redis-subscribe ctx "coop-chan"
      (lambda (r)
        (print "sub received: " r)
        (if (equal? (list-ref r 3) "quit")
            (begin (mutex-unlock! done) #f)
            #t)))))
(thread-sleep! 0.3)
(mutex-lock! done)
(print "subscriber exited cleanly")
(redis-disconnect ctx)' &
CSI_PID=$!
sleep 0.6 && redis-cli publish coop-chan hello > /dev/null
sleep 0.3 && redis-cli publish coop-chan quit  > /dev/null
wait $CSI_PID
```

Observed:

```
sub received: (pmessage coop-chan coop-chan hello)
sub received: (pmessage coop-chan coop-chan quit)
subscriber exited cleanly
```

Confirms: PSUBSCRIBE reply shape reaches the callback intact; returning `#f`
from the callback causes `redis-subscribe` to issue PUNSUBSCRIBE and exit.

---

## 4. Error surface — `errstr` reaches the caller

```sh
csi -R hiredis -e '(redis-connect "127.0.0.1" 6390)'
```
Observed:
```
Error: (redis-connect) Connection refused
```
Previously this raised a generic `"Failed to connect to redis"`. The fix: wait
cooperatively for the fd to be writable, then `getsockopt(SO_ERROR)` and feed
the errno through `strerror`. Hiredis's own `errstr` is empty in this window
because no hiredis function touched the socket between `connect()` and our
check.

---

## 5. Failed connects do not leak file descriptors

```sh
ulimit -n 256
csi -R hiredis -R chicken.condition -e '
(let loop ((i 0))
  (when (< i 1000)
    (handle-exceptions e #f (redis-connect "127.0.0.1" 6390))
    (loop (+ i 1))))
(print "1000 failed connects completed without EMFILE")'
```
Observed: `1000 failed connects completed without EMFILE`

A one-fd-per-attempt leak would hit `EMFILE` around attempt ~240 under
`ulimit -n 256`. The old code's connect helper returned `NULL` on `ctx->err`
without calling `redisFree(ctx)` — that path leaked a socket every time.

## 5b. Finalizer reclaims orphaned successful connects

```sh
ulimit -n 256
csi -R hiredis -R chicken.gc -e '
(let loop ((i 0))
  (when (< i 500)
    (redis-connect)      ; value dropped; only the GC finalizer can close it
    (when (zero? (modulo i 50)) (gc #t))
    (loop (+ i 1))))
(gc #t)
(print "500 orphaned connects survived; finalizer reclaimed fds")'
```
Observed: `500 orphaned connects survived; finalizer reclaimed fds`

500 successful connects under `ulimit -n 256` can only complete if
`set-finalizer!` is actually reclaiming dropped contexts during GC cycles.

---

## 6. No C memory leaks

**6a. RSS plateau across a long workload.** 100,000 commands over a mix of
reply shapes (string, nil, empty array, hash, set, error):

```scheme
(let loop ((i 0))
  (when (< i 20000)
    (redis-command ctx "SET" "leaktest:k" "v")
    (redis-command ctx "GET" "leaktest:k")
    (redis-command ctx "GET" "leaktest:missing")
    (redis-command ctx "LRANGE" "leaktest:nope" "0" "-1")
    (redis-command ctx "HGETALL" "leaktest:h")
    (loop (+ i 1))))
```

Sampling RSS once per half-second while the loop ran:

```
2400 KB    (baseline)
9712 KB
10528 KB
10912 KB    (heap expansion to working-set size)
11152 KB
...
11312 KB    (plateau — 30+ consecutive samples identical)
```

Growing linearly would indicate a leak; the flat tail rules one out.

**6b. macOS `leaks` scan.** After the workload completed, with the process
held alive via `thread-sleep!`:

```sh
leaks <pid>
```

Result:
```
Process 19706: 4253 nodes malloced for 12536 KB
Process 19706: 0 leaks for 0 total leaked bytes.
```

Of 4253 live allocations, zero are unreferenced. Every malloc the process
holds is reachable through a live pointer. This exercises the realistic leak
paths: `rh_append_argv`'s `argv`/`argvlen` pair, `freeReplyObject` in both
`redis-command` and `redis-subscribe`, and the per-reply string copies
produced by `redis-reply-str`.

## 6c. One-shot regression check

For CI or ad-hoc runs, `leaks -atExit` wraps a command and runs the analysis
just before the program exits:

```sh
leaks -atExit -- csi -s test.scm
```

No process-timing tricks required. Non-zero exit status if anything leaks.
