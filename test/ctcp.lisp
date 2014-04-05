;;;; Note that some of the strings in this file contain literal ASCII 0x01
;;;; characters.
(in-package #:birch.test)

(rt:deftest make-ctcp-message
    (birch:make-ctcp-message "Test message")
  "Test message")

(rt:deftest ctcp-message-p-negative-1
    (birch:ctcp-message-p "Test")
  NIL)

(rt:deftest ctcp-message-p-negative-2
    (birch:ctcp-message-p "Test")
  NIL)

(rt:deftest ctcp-message-p-negative-3
    (birch:ctcp-message-p "Test")
  NIL)

(rt:deftest ctcp-message-p-positive
    (birch:ctcp-message-p "Test")
  t)
