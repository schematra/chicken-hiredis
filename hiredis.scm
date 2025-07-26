;; Hiredis wrapper - A simple hiredis wrapper for CHICKEN
;; Copyright (c) 2025 Rolando Abarca <cpm.rolandoa@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

(module hiredis
 (
  redis-context
  redis-connect
  redis-command
  redis-subscribe
  )

 (import scheme)
 (import
  chicken.base
  chicken.foreign)

 (foreign-declare "#include <hiredis/hiredis.h>")
 (foreign-declare "#include \"redis_helpers.c\"")

 (define redis-context (make-parameter #f))

 ;; Connect to a Redis server
 ;; hostname: Redis server hostname or IP address (string, optional, defaults to "localhost")
 ;; port: Redis server port number (integer, optional, defaults to 6379)
 ;; Returns: Redis connection context pointer for use with redis-command
 ;; Example:
 ;;   (define ctx (redis-connect "127.0.0.1" 6379))
 ;;   (define ctx (redis-connect "localhost" 6379))
 ;;   (define ctx (redis-connect))  ; Uses defaults: localhost:6379
 ;;   (define ctx (redis-connect "redis.example.com"))  ; Uses default port 6379
 (define (redis-connect #!optional (host "localhost") (port 6379))
   (let ((native-connect (foreign-lambda* c-pointer ((c-string hostname) (int port))
					   "C_return(redisConnect(hostname, port));")))
     (native-connect host port)))

 (define redis-command-argv
   (foreign-lambda*
    nonnull-c-pointer
    ((c-pointer ctx) (int argc) (scheme-object arglist))
    "char **argv = malloc(argc * sizeof(char *));"
    "C_word pair = arglist;"
    "for (int i = 0; i < argc; ++i) {"
    "  C_word item = C_block_item(pair, 0); // car"
    "  argv[i] = C_c_string(item);"
    "  pair = C_block_item(pair, 1); // cdr"
    "}"
    "void *result = redisCommandArgv(ctx, argc, (const char **)argv, NULL);"
    "free(argv);"
    "C_return(result);"
    ))

 (define REDIS_REPLY_STRING 1)
 (define REDIS_REPLY_ARRAY 2)
 (define REDIS_REPLY_INTEGER 3)
 (define REDIS_REPLY_NIL 4)
 (define REDIS_REPLY_STATUS 5)
 (define REDIS_REPLY_ERROR 6)
 (define REDIS_REPLY_DOUBLE 7)
 (define REDIS_REPLY_BOOL 8)
 (define REDIS_REPLY_MAP 9)
 (define REDIS_REPLY_SET 10)
 (define REDIS_REPLY_ATTR 11)
 (define REDIS_REPLY_PUSH 12)
 (define REDIS_REPLY_BIGNUM 13)
 (define REDIS_REPLY_VERB 14)

 ;; Execute a Redis command with optional arguments
 ;; ctx: Redis connection context from redis-connect
 ;; command: Redis command string (e.g., "GET", "SET", "HGET")
 ;; rest: Optional command arguments
 ;; Returns: Scheme object representation of Redis reply
 ;; Examples:
 ;;   (redis-command ctx "GET" "mykey")
 ;;   (redis-command ctx "SET" "mykey" "myvalue")
 ;;   (redis-command ctx "HGET" "myhash" "field")
 (define (redis-command command . args)
   (let ((ctx (redis-context)))
     (unless ctx (error "No Redis context set"))
     (apply redis-command-internal ctx command args)))
 
 (define (redis-command-internal ctx command . rest)
   (let* ((argv         (cons command rest))
	  (argc         (length argv))
	  (native-reply (redis-command-argv ctx argc argv))
	  (reply        (redis-reply->object native-reply)))
     (redis-free-reply native-reply)
     reply))

 ;; convert a hiredis reply to scheme object.
 (define (redis-reply->object reply)
   (let ((reply-type (redis-reply-type reply)))
     (cond
      ((= reply-type REDIS_REPLY_ERROR)   (cons 'error (redis-reply-str reply)))
      ((= reply-type REDIS_REPLY_STATUS)  (redis-reply-str reply))
      ((= reply-type REDIS_REPLY_STRING)  (redis-reply-str reply))
      ((= reply-type REDIS_REPLY_INTEGER) (redis-reply-int reply))
      ((= reply-type REDIS_REPLY_DOUBLE)  (redis-reply-double reply))
      ((= reply-type REDIS_REPLY_ARRAY)   (build-array-reply reply))
      ((= reply-type REDIS_REPLY_NIL)     #f)
      (else                               (cons 'unknown reply-type)))))

 (define (build-array-reply reply)
   (let ((total-elements (redis-reply-elements reply)))
     (let loop ((i 0) (out '()))
       (if (>= i total-elements)
	   (reverse out)
	   (let ((elt (redis-reply-element reply i)))
	     (loop (+ i 1) (cons (redis-reply->object elt) out)))))))

 ;; Subscribe to a Redis channel using pattern matching
 ;; channel: Channel pattern to subscribe to (string, supports wildcards)
 ;; callback: Callback function that receives each message
 ;; The callback receives a list with 4 elements for pattern messages:
 ;;   ("pmessage" "pattern" "channel" "message")
 ;; The callback should return #t to continue listening, #f to unsubscribe
 ;; Example:
 ;;   (redis-subscribe "test*"
 ;;     (lambda (reply)
 ;;       (let ((msg (list-ref reply 3)))
 ;;         (format #t "Got: ~A\n" msg)
 ;;         (not (string=? msg "quit")))))
 (define (redis-subscribe channel callback)
   (unless (redis-context) (error "No Redis context set"))
   (let ((sub-reply (redis-command "PSUBSCRIBE" channel)))
     (when (not sub-reply) (error "subscription failed")))
   ;; (format #t "subscribed to channel: ~A\n" channel)
   (let loop ()
     (let ((native-reply      (redis-get-reply)))
       (if (not native-reply) (error "Redis connection lost (NULL reply)"))
       (let* ((reply (redis-reply->object native-reply))
	      (continue? (callback reply)))	
	 (redis-free-reply native-reply)
	 (if continue? (loop) (redis-command "PUNSUBSCRIBE" channel))))))

 (define (redis-get-reply)
   (let ((native-get-reply (foreign-lambda c-pointer "getReply" c-pointer))
	 (ctx (redis-context)))
     (unless ctx (error "No Redis context set"))
     (native-get-reply ctx)))

 (define redis-free-reply
   (foreign-lambda void "freeReplyObject" c-pointer))

 ;; redisReply accessors
 (define redis-reply-type
   (foreign-lambda int "redisReplyType" c-pointer))

 ;; return a copy (that we manage) of the string in the reply
 (define (redis-reply-str reply)
   (let ((rstr ((foreign-lambda c-string "redisReplyStr" c-pointer) reply)))
     (string-copy rstr)))

 (define redis-reply-int
   (foreign-lambda integer64 "redisReplyInteger" c-pointer))

 (define redis-reply-double
   (foreign-lambda double "redisReplyDouble" c-pointer))

 (define redis-reply-elements
   (foreign-lambda size_t "redisReplyElements" c-pointer))

 (define redis-reply-element
   (foreign-lambda c-pointer "redisReplyElement" c-pointer size_t)))
