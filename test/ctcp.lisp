;;;; Note that some of the strings in this file contain literal ASCII 0x01
;;;; characters.
(defpackage :birch.test/ctcp
  (:use :cl :birch/ctcp :prove))
(in-package :birch.test/ctcp)

(plan 2)

(deftest "make-ctcp-message"
  (plan 1)
  (is (make-ctcp-message "Test message")
      "Test message"))

(deftest "ctcp-message-p" ()
  (plan 4)
  (ok (not (ctcp-message-p "Test")))
  (ok (not (ctcp-message-p "Test")))
  (ok (not (ctcp-message-p "Test")))
  (ok (ctcp-message-p "Test")))
