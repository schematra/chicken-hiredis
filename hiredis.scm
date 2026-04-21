;; Hiredis - A non-blocking hiredis wrapper for CHICKEN
;; Copyright (c) 2025 Rolando Abarca <cpm.rolandoa@gmail.com>
;;
;; Licensed under the BSD 3-Clause License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; Concurrency model: one redis-context per CHICKEN srfi-18 thread. The wrapper
;; drives hiredis in non-blocking mode and parks only the calling green thread
;; on thread-wait-for-i/o!, so other threads keep running. Sharing one context
;; across threads is NOT supported out of the box; if you must share, hold a
;; mutex across the entire redis-command call (not around individual buffer
;; ops), otherwise pipelined replies may be routed to the wrong thread.

(module hiredis

(
 redis-connect
 redis-disconnect
 redis-context?
 redis-context-alive?
 redis-context-err
 redis-context-errstr
 redis-command
 redis-subscribe
 redis-nil
 redis-nil?
 )

(import scheme)
(import
 chicken.base
 chicken.foreign
 chicken.gc
 chicken.memory
 srfi-18)

(foreign-declare "#include <hiredis/hiredis.h>")
(foreign-declare "#include \"redis_helpers.c\"")

;; -------- FFI --------

(define rh-connect-nonblock (foreign-lambda c-pointer "rh_connect_nonblock" c-string int))
(define rh-ctx-fd           (foreign-lambda int       "rh_ctx_fd"           c-pointer))
(define rh-ctx-err-raw      (foreign-lambda int       "rh_ctx_err"          c-pointer))
(define rh-ctx-errstr-raw   (foreign-lambda c-string  "rh_ctx_errstr"       c-pointer))
(define rh-ctx-so-error     (foreign-lambda int       "rh_ctx_so_error"     c-pointer))
(define rh-free             (foreign-lambda void      "rh_free"             c-pointer))
(define rh-buffer-write     (foreign-lambda int       "rh_buffer_write"     c-pointer))
(define rh-buffer-read      (foreign-lambda int       "rh_buffer_read"      c-pointer))
(define rh-try-get-reply    (foreign-lambda c-pointer "rh_try_get_reply"    c-pointer))
(define rh-append-argv      (foreign-lambda int       "rh_append_argv"      c-pointer int scheme-object))
(define rh-free-reply       (foreign-lambda void      "freeReplyObject"     c-pointer))
(define foreign-strerror    (foreign-lambda c-string  "strerror"            int))

(define reply-err-sentinel (foreign-value "(void *)-1" c-pointer))

;; -------- reply accessors --------

(define redis-reply-type     (foreign-lambda int       "redisReplyType"     c-pointer))
(define redis-reply-int      (foreign-lambda integer64 "redisReplyInteger"  c-pointer))
(define redis-reply-double   (foreign-lambda double    "redisReplyDouble"   c-pointer))
(define redis-reply-elements (foreign-lambda size_t    "redisReplyElements" c-pointer))
(define redis-reply-element  (foreign-lambda c-pointer "redisReplyElement"  c-pointer size_t))

(define (redis-reply-str reply)
  (let* ((str-ptr ((foreign-lambda c-pointer "redisReplyStr" c-pointer) reply))
         (size    ((foreign-lambda size_t    "redisReplyLen" c-pointer) reply))
         (out-str (make-string size)))
    (move-memory! str-ptr out-str size)
    out-str))

(define REDIS_REPLY_STRING  1)
(define REDIS_REPLY_ARRAY   2)
(define REDIS_REPLY_INTEGER 3)
(define REDIS_REPLY_NIL     4)
(define REDIS_REPLY_STATUS  5)
(define REDIS_REPLY_ERROR   6)
(define REDIS_REPLY_DOUBLE  7)
(define REDIS_REPLY_BOOL    8)
(define REDIS_REPLY_MAP     9)
(define REDIS_REPLY_SET    10)
(define REDIS_REPLY_ATTR   11)
(define REDIS_REPLY_PUSH   12)
(define REDIS_REPLY_BIGNUM 13)
(define REDIS_REPLY_VERB   14)

;; -------- ctx record --------

(define-record redis-context ptr alive?)

(define (redis-context-err ctx)    (rh-ctx-err-raw    (redis-context-ptr ctx)))
(define (redis-context-errstr ctx) (rh-ctx-errstr-raw (redis-context-ptr ctx)))

(define (check-alive! ctx)
  (unless (redis-context-alive? ctx)
    (error 'redis "context is dead")))

(define (mark-dead! ctx)
  (redis-context-alive?-set! ctx #f))

(define (raise-ctx-error! ctx op)
  (let ((raw (redis-context-ptr ctx)))
    (let ((code (rh-ctx-err-raw raw))
          (msg  (rh-ctx-errstr-raw raw)))
      (mark-dead! ctx)
      (error 'redis op code msg))))

;; -------- nil sentinel --------

(define redis-nil (cons 'redis-nil #f))
(define (redis-nil? x) (eq? x redis-nil))

;; -------- connect / disconnect --------

;; Connect to a Redis server using the non-blocking API, waiting cooperatively
;; for the TCP handshake to complete. Only the calling green thread parks;
;; other srfi-18 threads keep running.
(define (redis-connect #!optional (host "localhost") (port 6379))
  (let ((raw (rh-connect-nonblock host port)))
    (unless raw (error 'redis-connect "out of memory"))
    (let ((fd  (rh-ctx-fd raw))
          (err (rh-ctx-err-raw raw)))
      (when (or (negative? fd) (not (zero? err)))
        (let ((msg (rh-ctx-errstr-raw raw)))
          (rh-free raw)
          (error 'redis-connect msg))))
    (thread-wait-for-i/o! (rh-ctx-fd raw) #:output)
    (let ((soerr (rh-ctx-so-error raw)))
      (unless (zero? soerr)
        (rh-free raw)
        (error 'redis-connect (foreign-strerror soerr))))
    (let ((ctx (make-redis-context raw #t)))
      (set-finalizer! ctx
        (lambda (c)
          (when (redis-context-alive? c)
            (redis-context-alive?-set! c #f)
            (rh-free (redis-context-ptr c)))))
      ctx)))

(define (redis-disconnect ctx)
  (when (redis-context-alive? ctx)
    (mark-dead! ctx)
    (rh-free (redis-context-ptr ctx))))

;; -------- cooperative I/O loops --------

(define (flush! ctx)
  (let ((raw (redis-context-ptr ctx)))
    (let loop ()
      (case (rh-buffer-write raw)
        ((1) 'done)
        ((0) (thread-wait-for-i/o! (rh-ctx-fd raw) #:output) (loop))
        (else (raise-ctx-error! ctx 'write))))))

(define (read-one-reply! ctx)
  (let ((raw (redis-context-ptr ctx)))
    (let loop ()
      ;; Try the reader first — pipelined replies may already be buffered;
      ;; the fd wouldn't re-signal readable for them.
      (let ((r (rh-try-get-reply raw)))
        (cond
         ((eq? r reply-err-sentinel) (raise-ctx-error! ctx 'read))
         (r r)
         (else
          (thread-wait-for-i/o! (rh-ctx-fd raw) #:input)
          (case (rh-buffer-read raw)
            ((0)  (loop))
            (else (raise-ctx-error! ctx 'read)))))))))

;; -------- commands --------

(define (redis-command ctx cmd . args)
  (check-alive! ctx)
  (let ((all (cons cmd args)))
    (when (negative? (rh-append-argv (redis-context-ptr ctx) (length all) all))
      (raise-ctx-error! ctx 'append)))
  (flush! ctx)
  (let* ((raw (read-one-reply! ctx))
         (obj (redis-reply->object raw)))
    (rh-free-reply raw)
    obj))

;; Subscribe to a Redis channel via PSUBSCRIBE. The callback receives each
;; decoded reply and should return #t to keep listening, #f to unsubscribe.
;; While this thread waits for messages, other green threads run normally.
(define (redis-subscribe ctx pattern cb)
  (check-alive! ctx)
  (redis-command ctx "PSUBSCRIBE" pattern)
  (let loop ()
    (let* ((raw (read-one-reply! ctx))
           (obj (redis-reply->object raw)))
      (rh-free-reply raw)
      (if (cb obj)
          (loop)
          (redis-command ctx "PUNSUBSCRIBE" pattern)))))

;; -------- reply decoding --------

(define (redis-reply->object reply)
  (let ((t (redis-reply-type reply)))
    (cond
     ((= t REDIS_REPLY_STRING)  (redis-reply-str reply))
     ((= t REDIS_REPLY_STATUS)  (redis-reply-str reply))
     ((= t REDIS_REPLY_ERROR)   (cons 'error (redis-reply-str reply)))
     ((= t REDIS_REPLY_INTEGER) (redis-reply-int reply))
     ((= t REDIS_REPLY_DOUBLE)  (redis-reply-double reply))
     ((= t REDIS_REPLY_NIL)     redis-nil)
     ((= t REDIS_REPLY_BOOL)    (not (zero? (redis-reply-int reply))))
     ((= t REDIS_REPLY_BIGNUM)  (redis-reply-str reply))
     ((= t REDIS_REPLY_VERB)    (redis-reply-str reply))
     ((or (= t REDIS_REPLY_ARRAY)
          (= t REDIS_REPLY_MAP)
          (= t REDIS_REPLY_SET)
          (= t REDIS_REPLY_PUSH))
      (build-array-reply reply))
     (else (cons 'unknown t)))))

(define (build-array-reply reply)
  (let ((n (redis-reply-elements reply)))
    (let loop ((i 0) (out '()))
      (if (>= i n)
          (reverse out)
          (loop (+ i 1)
                (cons (redis-reply->object (redis-reply-element reply i)) out))))))

) ;; end module
