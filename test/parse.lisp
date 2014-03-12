(in-package #:birch.test)

(rt:deftest parse-prefix-full
  (birch::parse-prefix ":WiZ!jto@tolsun.oulu.fi")
  "WiZ" "jto" "tolsun.oulu.fi")

(rt:deftest parse-prefix-no-user
  (birch::parse-prefix ":WiZ@tolsun.oulu.fi")
  "WiZ" NIL "tolsun.oulu.fi")

(rt:deftest parse-prefix-only-nick
  (birch::parse-prefix ":WiZ")
  "WiZ" NIL NIL)

(rt:deftest reply->keyword-num1
  (birch::reply->keyword 1)
  :RPL_WELCOME)

(rt:deftest reply->keyword-num974
  (birch::reply->keyword 974)
  :ERR_CANNOTCHANGECHANMODE)

(rt:deftest reply->keyword-privmsg
  (birch::reply->keyword "PRIVMSG")
  :PRIVMSG)

(rt:deftest parse-message-privmsg-1
  (birch::parse-message "PRIVMSG Supertest")
  NIL :PRIVMSG ("Supertest"))

(rt:deftest parse-message-privmsg-2
  (birch::parse-message "PRIVMSG")
  NIL :PRIVMSG ())

;; Note that this text also depends on PARSE-PREFIX functioning correctly
(rt:deftest parse-message-privmsg-3
  (birch::parse-message ":test PRIVMSG")
  ("test" NIL NIL) :PRIVMSG ())
