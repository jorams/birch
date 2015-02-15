;;;; Functions for parsing the messages read from the stream.
(defpackage :birch/parse
  (:use :cl :birch/replies)
  (:export #:parse-message
           #:parse-prefix))
(in-package :birch/parse)

(defun parse-prefix (prefix)
  "Parses a prefix, assumed to still have the ':' at the front.
   Returns 3 values:
   - NICK (or server name)
   - USER
   - HOST
   Note that this function also succeeds when a user is supplied but not a
   host, even though that isn't valid according to RFC2812."
  (let* ((names+host (split-sequence #\@ (subseq prefix 1)))
         (nick+user (split-sequence #\! (first names+host))))
    (values (first nick+user) (second nick+user) (second names+host))))

(defun parse-message (message)
  "Parses MESSAGE into a couple of parts:
   - PREFIX as a list consisting of SOURCE, USER and HOST
   - COMMAND as a keyword, numerics are converted to their keyword equivalent
   - PARAMS as a list of strings
   The trailing argument, if available, will be the last item in PARAMS"
  (let* ((final-argument (search " :" message))
         (arguments (append (split-sequence #\Space
                                            message
                                            :remove-empty-subseqs t
                                            :end final-argument)
                            (if final-argument
                                (list (subseq message
                                              (+ 2 final-argument)))))))
    (values (when (char= #\: (elt (first arguments) 0))
              (multiple-value-list (parse-prefix (pop arguments))))
            (reply->keyword (first arguments))
            (subseq arguments 1))))
