;;;; Some functions to aid in handling CTCP messages
(defpackage :birch/ctcp
  (:use :cl)
  (:export #:make-ctcp-message
           #:ctcp-message-p))
(in-package :birch/ctcp)

(defun make-ctcp-message (string)
  "Returns a string with a 0x01 character added to the start and end of STRING,
   turning it into a CTCP message. The resulting string can then be sent to a
   connection using PRIVMSG or NOTICE"
  (format NIL "~A~A~A" (code-char 1) string (code-char 1)))

(defun ctcp-message-p (string)
  "Returns whether or not STRING is a valid CTCP message, by checking if it
   starts and ends with a 0x01 character."
  (and (> (length string) 1)
       (char= (elt string 0)
              (code-char 1))
       (char= (elt string (1- (length string)))
              (code-char 1))))
