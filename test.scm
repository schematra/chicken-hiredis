(import hiredis format)

(define ctx (redis-connect))

(redis-command ctx "KEYS" "*")
(redis-command ctx "GET" "foo")
(redis-command ctx "HGET" "myhash" "foox")

(redis-subscribe ctx "test"
  ;; reply has 4 elements for psubscribe:
  ;; ("pmessage" "sub pattern" "channel name" "msg")
  (lambda (reply)
    (let ((msg (list-ref reply 3)))
      (format #t "received: ~A\n" reply)
      (not (string=? msg "xoxo")))))

(redis-disconnect ctx)
