(import hiredis format)

(define ctx (redis-connect))
(unless ctx (error "Failed to connect to Redis"))

;; you can do parameterize or just set the context
(redis-context ctx)

(redis-command "KEYS" "*")
(redis-command "GET" "foo")
(redis-command "HGET" "myhash" "foox")

(redis-subscribe
 "test"
 ;; reply has 4 elements for psubscribe:
 ;; ("pmessage" "sub pattern" "channel name" "msg")
 (lambda (reply)
   (let* ((msg (list-ref reply 3)))
     (format #t "received: ~A\n" reply)
     (if (string=? msg "xoxo") #f #t))))
