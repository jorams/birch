;;;; Note that some of the strings in this file contain literal ASCII 0x01
;;;; characters.
(fiasco:define-test-package :birch.test/ctcp
  (:use :cl :birch/ctcp))
(in-package :birch.test/ctcp)

(deftest test-make-ctcp-message ()
  (is (string= (make-ctcp-message "Test message")
               "Test message")))

(deftest test-ctcp-message-p ()
  (is (not (ctcp-message-p "Test")))
  (is (not (ctcp-message-p "Test")))
  (is (not (ctcp-message-p "Test")))
  (is (ctcp-message-p "Test")))
