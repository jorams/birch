(defpackage :birch.test/util
  (:use :cl :fiasco)
  (:import-from :flexi-streams
                #:string-to-octets
                #:make-in-memory-output-stream
                #:get-output-stream-sequence
                #:flexi-stream-stream
                #:make-flexi-stream)
  (:import-from :birch/connection
                #:connection
                #:socket-stream)
  (:export #:test-connection
           #:with-test-connection
           #:message-ify
           #:is-message
           #:define-message-test))
(in-package :birch.test/util)

(defclass test-connection (connection) ()
  (:default-initargs
   :server-host "127.0.0.1"
   :nick "test"))

(defmacro with-test-connection ((connection-symbol stream-symbol)
                                &body body)
  `(let* ((,connection-symbol
            (make-instance
             'test-connection
             :stream (make-flexi-stream
                      (make-in-memory-output-stream)
                      :external-format '(:UTF-8 :eol-style :crlf))))
          (,stream-symbol (flexi-stream-stream
                           (socket-stream ,connection-symbol))))
     ,@body))

(defun message-ify (string)
  (coerce (string-to-octets
           (format nil "~A~A~A" string #\return #\linefeed))
          'list))

(defmacro is-message (stream command result)
  `(progn ,command
          (is (equal (get-output-stream-sequence ,stream :as-list t)
                     (message-ify ,result)))))

(defmacro define-message-test (name (connection-symbol)
                               &body pairs)
  (let ((stream-symbol (gensym)))
    `(deftest ,name ()
       (with-test-connection (,connection-symbol ,stream-symbol)
         ,@(loop for (command result) in pairs
                 collect `(is-message ,stream-symbol
                                      ,command
                                      ,result))))))
