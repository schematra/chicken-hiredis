# Redis Client for CHICKEN Scheme

A Redis client library for CHICKEN Scheme on top of the hiredis C library.
Uses the non-blocking hiredis API and CHICKEN's srfi-18 scheduler so that
only the calling green thread parks on socket I/O — other threads keep
running. This makes long-lived `redis-subscribe` loops viable alongside
other concurrent work.

## Features

- Non-blocking sockets cooperating with srfi-18 green threads
- All Redis reply types parsed to Scheme values
- Pub/sub via `PSUBSCRIBE`
- Connection contexts auto-reclaimed by GC finalizer (or freed eagerly with
  `redis-disconnect`)

## Concurrency model

**One `redis-context` per srfi-18 thread.** The wrapper drives hiredis in
non-blocking mode; only the calling green thread parks on
`thread-wait-for-i/o!`. Sharing one context across threads is not supported
out of the box — if you must share, hold a mutex across the *entire*
`redis-command` call (append + flush + read), not around individual buffer
ops, otherwise pipelined replies may be routed to the wrong thread.

## Dependencies

- hiredis C library

```sh
# macOS
brew install hiredis

# Ubuntu / Debian
sudo apt-get install libhiredis-dev

# CentOS / RHEL
sudo yum install hiredis-devel
```

## Building & Installing

From the repo root:

```sh
chicken-install
```

## Usage

### Basic example

```scheme
(import hiredis)

(define ctx (redis-connect "127.0.0.1" 6379))

(redis-command ctx "SET" "mykey" "myvalue")
(redis-command ctx "GET" "mykey")
(redis-command ctx "HSET" "myhash" "field1" "value1")
(redis-command ctx "HGET" "myhash" "field1")
(redis-command ctx "KEYS" "*")

(redis-disconnect ctx)
```

### Pub/sub

```scheme
(import hiredis srfi-18)

(define ctx (redis-connect))

(thread-start!
  (lambda ()
    (redis-subscribe ctx "notifications.*"
      (lambda (reply)
        (let ((channel (list-ref reply 2))
              (message (list-ref reply 3)))
          (display (list 'got channel message)) (newline)
          (not (string=? message "quit")))))))   ; #f to unsubscribe

;; main thread keeps running while the subscriber is parked
(let loop ()
  (redis-command (redis-connect) "PING")
  (thread-sleep! 1)
  (loop))
```

## API

### `(redis-connect [hostname [port]])`

Connect to a Redis server using the non-blocking API. The TCP handshake is
awaited cooperatively — only the calling green thread parks. Returns a
`redis-context` record. A finalizer reclaims the underlying socket if the
context is dropped without `redis-disconnect`.

Defaults: `hostname` = `"localhost"`, `port` = `6379`.

Raises a Scheme error on connect failure with a message derived from
`SO_ERROR` / hiredis's `errstr` (e.g. `Connection refused`,
`No route to host`).

### `(redis-disconnect ctx)`

Close the socket and free the underlying `redisContext` immediately. Marks
the context dead; subsequent `redis-command` calls will raise. Idempotent.

### `(redis-command ctx command . args)`

Execute a Redis command. `command` and `args` are strings. Binary-safe —
strings with embedded NUL bytes are passed through correctly.

Returns a Scheme object representing the reply (see [Reply types](#reply-types)).

Raises if the context is dead, or if the underlying socket / protocol
errors out (the context is then marked dead — re-`redis-connect` to
recover).

### `(redis-subscribe ctx pattern callback)`

Issue `PSUBSCRIBE pattern` and loop over incoming pmessages. The callback
receives each decoded reply, typically of shape:

```
("pmessage" "<pattern>" "<channel>" "<message>")
```

Return `#t` from the callback to keep listening, `#f` to issue
`PUNSUBSCRIBE` and return.

While this thread is parked waiting for messages, other srfi-18 threads
continue to run.

### `(redis-context? x)` / `(redis-context-alive? ctx)`

Predicates for the context type and its liveness.

### `(redis-context-err ctx)` / `(redis-context-errstr ctx)`

Read hiredis's error code and error string off a context. Useful for
diagnosing why a context was marked dead.

### `redis-nil` / `(redis-nil? x)`

Sentinel value returned for `REDIS_REPLY_NIL` (e.g. `GET` on a missing
key). It is `eq?`-comparable, distinct from `'()` (which represents an
empty array reply). Use `redis-nil?` to test.

## Reply types

| Hiredis reply           | Scheme value                                          |
| ----------------------- | ----------------------------------------------------- |
| `REDIS_REPLY_STRING`    | string                                                |
| `REDIS_REPLY_STATUS`    | string                                                |
| `REDIS_REPLY_INTEGER`   | integer                                               |
| `REDIS_REPLY_DOUBLE`    | float                                                 |
| `REDIS_REPLY_BOOL`      | `#t` / `#f`                                           |
| `REDIS_REPLY_NIL`       | `redis-nil` sentinel (use `redis-nil?`)               |
| `REDIS_REPLY_ARRAY`     | list                                                  |
| `REDIS_REPLY_MAP`       | flat list of alternating key/value (RESP3)            |
| `REDIS_REPLY_SET`       | list (RESP3)                                          |
| `REDIS_REPLY_PUSH`      | list (RESP3 push)                                     |
| `REDIS_REPLY_BIGNUM`    | string                                                |
| `REDIS_REPLY_VERB`      | string                                                |
| `REDIS_REPLY_ERROR`     | `(cons 'error "<message>")`                           |

## Files

- `hiredis.scm` — the module
- `redis_helpers.c` — C helpers (non-blocking I/O wrappers, reply accessors)
- `test.scm` — example usage
- `TESTING.md` — verification log for the non-blocking implementation

## License

BSD 3-Clause.
