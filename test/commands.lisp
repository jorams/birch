(defpackage :birch.test/commands
  (:use :cl :birch/commands :birch.test/util :prove)
  (:import-from :birch/connection
                #:make-channel
                #:make-user
                #:activep))
(in-package :birch.test/commands)

(diag "Command tests")
(plan 12)

(define-message-test "/raw" (connection)
  ((/raw connection "TEST ~A message ~A" #\a 1)
   "TEST a message 1"))

(define-message-test "/pass" (connection)
  ((/pass connection "secretpasswordhere")
   "PASS secretpasswordhere"))

(define-message-test "/nick" (connection)
  ((/nick connection "WiZ")
   "NICK WiZ"))

(define-message-test "/user" (connection)
  ((/user connection "jto" 0 "Example Name")
   "USER jto 0 * :Example Name"))

(define-message-test "/join" (connection)
  ((/join connection "#test1") "JOIN #test1")
  ((/join connection "#test2" "secretkeyhere")
   "JOIN #test2 secretkeyhere")
  ((/join connection (make-channel connection "#test3"))
   "JOIN #test3")
  ((/join connection (make-channel connection "#test4")
          "secretkeyhere")
   "JOIN #test4 secretkeyhere"))

(define-message-test "/privmsg" (connection)
  ((/privmsg connection "#test1" "Test Message")
   "PRIVMSG #test1 :Test Message")
  ((/privmsg connection (make-channel connection "#test2")
             "Test Message")
   "PRIVMSG #test2 :Test Message")
  ((/privmsg connection (make-user connection "test-user-1")
             "Test Message")
   "PRIVMSG test-user-1 :Test Message"))

(define-message-test "/invite" (connection)
  ((/invite connection "test-user-1" "#test1")
   "INVITE test-user-1 #test1")
  ((/invite connection
            (make-user connection "test-user-2")
            (make-channel connection "#test2"))
   "INVITE test-user-2 #test2")
  ((/invite connection
            (make-user connection "test-user-3")
            "#test3")
   "INVITE test-user-3 #test3")
  ((/invite connection
            "test-user-4"
            (make-channel connection "#test4"))
   "INVITE test-user-4 #test4"))

(define-message-test "/kick" (connection)
  ((/kick connection "#test1" "test-user-1" "Message")
   "KICK #test1 test-user-1 :Message")
  ((/kick connection "#test2" "test-user-2")
   "KICK #test2 test-user-2")
  ((/kick connection
          (make-channel connection "#test3")
          (make-user connection "test-user-3")
          "Message")
   "KICK #test3 test-user-3 :Message")
  ((/kick connection
          (make-channel connection "#test4")
          (make-user connection "test-user-4"))
   "KICK #test4 test-user-4")
  ((/kick connection
          "#test5"
          (make-user connection "test-user-5")
          "Message")
   "KICK #test5 test-user-5 :Message")
  ((/kick connection
          "#test6"
          (make-user connection "test-user-6"))
   "KICK #test6 test-user-6")
  ((/kick connection
          (make-channel connection "#test7")
          "test-user-7"
          "Message")
   "KICK #test7 test-user-7 :Message")
  ((/kick connection
          (make-channel connection "#test8")
          "test-user-8")
   "KICK #test8 test-user-8"))

(define-message-test "/part" (connection)
  ((/part connection "#test1") "PART #test1")
  ((/part connection "#test2" "Message")
   "PART #test2 :Message")
  ((/part connection (make-channel connection "#test3"))
   "PART #test3")
  ((/part connection (make-channel connection "#test4")
          "Message")
   "PART #test4 :Message"))

(define-message-test "/quit" (connection)
  ((/quit connection) "QUIT :Birch, Common Lisp IRC library")
  ((/quit connection "Message") "QUIT :Message"))

(deftest "Quit disables connection"
  (plan 1)
  (with-test-connection (connection stream)
    (declare (ignore stream))
    (/quit connection)
    (ok (not (activep connection)))))

(define-message-test "/pong" (connection)
  ((/pong connection "server.1" "server.2") "PONG server.1 server.2")
  ((/pong connection "server.3") "PONG server.3"))
