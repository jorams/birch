(fiasco:define-test-package :birch.test/events
  (:use :cl :birch/events :birch.test/util)
  (:import-from :birch/parse
                #:parse-message)
  (:import-from :birch/connection
                #:make-channel
                #:make-user
                #:users
                #:channels
                #:channel-type
                #:nick))
(in-package :birch.test/events)

(defvar *result-tag* (gensym)
  "A tag that methods created by DEFINE-EVENT-THROWER will throw at, and which
will be caught by IS-THROWN-MESSAGES.")

(defmacro define-event-thrower (class result)
  "Create a method on HANDLE-EVENT specialized on CLASS, that will simply throw
RESULT at *RESULT-TAG* when invoked."
  `(defmethod handle-event ((,(gensym) test-connection)
                            (,(gensym) ,class))
     (throw *result-tag* ,result)))

(define-event-thrower privmsg-event :privmsg)
(define-event-thrower notice-event :notice)
(define-event-thrower join-event :join)
(define-event-thrower part-event :part)
(define-event-thrower quit-event :quit)
(define-event-thrower kick-event :kick)
(define-event-thrower nick-event :nick)
(define-event-thrower topic-event :topic)

(defmacro is-thrown-messages ((connection) &body pairs)
  "Handle messages on CONNECTION and test if they result in a symbol being
thrown at *RESULT-TAG*.
PAIRS is a list of the form (SYMBOL MESSAGE SYMBOL MESSAGE ...)."
  `(progn
     ,@(loop for (result message) in pairs by #'cddr
             collect `(is (eq ,result
                              (catch *result-tag*
                                (multiple-value-call #'handle-message
                                  ,connection
                                  (parse-message ,message))
                                ;; Never eq to RESULT
                                :bla))))))

(deftest message-to-event-test ()
  (with-test-connection (connection stream)
    (declare (ignore stream))
    (is-thrown-messages (connection)
      (:privmsg ":WiZ!jto@tolsun.oulu.fi PRIVMSG #test1 :Test Message")
      (:notice ":WiZ!jto@tolsun.oulu.fi NOTICE #test2 :Test Notice")
      (:join ":WiZ!jto@tolsun.oulu.fi JOIN :#test3")
      (:part ":WiZ!jto@tolsun.oulu.fi PART :#test4")
      (:quit ":WiZ!jto@tolsun.oulu.fi QUIT")
      (:kick ":WiZ!jto@tolsun.oulu.fi KICK #test5 user1")
      (:nick ":WiZ!jto@tolsun.oulu.fi NICK Kilroy")
      (:topic ":irc.example.com 332 test1 #test6 :Topic")
      (:topic ":WiZ!jto@tolsun.oulu.fi TOPIC #test7 :New Topic"))))

(define-message-test pong-test (connection)
  ((multiple-value-call #'handle-message connection
     (parse-message "PING :irc.funet.fi"))
   "PONG irc.funet.fi"))

(defun message-connection (connection message)
  (multiple-value-call #'handle-message connection
      (parse-message message)))

(deftest namreply-test ()
  (with-test-connection (connection stream)
    (declare (ignore stream))
    (message-connection
     connection
     ":irc.example.com 353 test1 = #test1 :@testop +testvoice test1")
    (let ((channel (make-channel connection "#test1")))
      (is (= (length (users channel)) 3))
      (is (member "testvoice" (users channel) :key #'nick :test #'string=))
      (is (member "testop" (users channel) :key #'nick :test #'string=))
      (is (member "test1" (users channel) :key #'nick :test #'string=))
      (is (eq (channel-type channel) :public)))

    (message-connection
     connection
     ":irc.example.com 353 test2 @ #test2 :+testvoice test2")
    (let ((channel (make-channel connection "#test2")))
      (is (= (length (users channel)) 2))
      (is (eq (channel-type (make-channel connection "#test2"))
              :secret)))

    (message-connection
     connection
     ":irc.example.com 353 test3 * #test3 :@testop +testvoice test3")
    (is (eq (channel-type (make-channel connection "#test3"))
            :private))

    (is (= (length (channels (make-user connection "testop")))
           2))
    (is (= (length (channels (make-user connection "testvoice")))
           3))))
