;;;; Basic connection functionality.
(defpackage :birch/init
  (:use :cl :birch/connection :birch/parse)
  (:import-from :birch/commands
                #:/nick
                #:/user
                #:/pass)
  (:import-from :birch/events
                #:handle-message)
  (:export #:connect
           #:read-message
           #:process-message
           #:process-message-loop))
(in-package :birch/init)

(defun connect (connection)
  "Connects to the IRC network denoted by CONNECTION. Opens a connection to the
   server and performs initial registration."
  ;; Initialization
  (connect-socket connection)
  (unless (user connection)
    (setf (user connection) (nick connection)))
  ;; Registration
  (if (pass connection) (/pass connection (pass connection)))
  (/nick connection (nick connection))
  (/user connection (user connection) 0 (real-name connection)))

(defun read-message (connection)
  (parse-message
   (string-trim '(#\Return)
                (handler-bind
                    ((flex:external-format-encoding-error
                       (lambda (error)
                         (declare (ignore error))
                         (invoke-restart 'use-value #\REPLACEMENT_CHARACTER))))
                  (read-line (socket-stream connection))))))

(defun process-message (connection)
  "Reads a message from CONNECTION and calls HANDLE-MESSAGE on it. Should
   probably be called in a loop. See PROCESS-MESSAGE-LOOP."
  (multiple-value-call #'handle-message
    connection
    (read-message connection)))

(defun process-message-loop (connection)
  "Continuously calls READ-MESSAGE until the connection is closed."
  ;; We keep executing PROCESS-MESSAGE until END-OF-FILE is reached. In that
  ;; case, we check if we wanted to quit, and if not we try to reconnect until
  ;; that succeeds.
  (loop do (handler-case
               (loop (process-message connection))
             (end-of-file ()
               (if (activep connection)
                   (handler-case
                       (progn (sleep 5)
                              (connect connection)
                              t)
                     (serious-condition nil))
                   (loop-finish))))))
