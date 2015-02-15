;;;; Basic connection functionality.
(defpackage :birch/init
  (:use :cl :birch/connection :birch/parse)
  (:import-from :birch/commands
                #:nick
                #:user)
  (:export #:connect
           #:read-message-loop))
(in-package :birch/init)

(defun connect (connection)
  "Connects to the IRC network denoted by CONNECTION. Opens a connection to the
   server and performs initial registration."
  (let ((socket (usocket:socket-connect (host-of connection)
                                        (port-of connection)
                                        :element-type '(unsigned-byte 8))))
    ;; Initialization
    (setf (socket-of connection) socket
          (stream-of connection)
          (flexi-streams:make-flexi-stream
           (usocket:socket-stream socket)
           :external-format '(:UTF-8 :eol-style :crlf))
          (activep connection) t)
    (unless (user-of connection)
      (setf (user-of connection) (nick-of connection)))
    ;; Registration
    (if (pass-of connection) (pass connection (pass-of connection)))
    (nick connection (nick-of connection))
    (user connection (user-of connection) 0 (real-name-of connection))))

(defun read-message (connection)
  "Reads a message from CONNECTION and calls HANDLE-MESSAGE on it. Should
   probably be called in a loop."
  (multiple-value-call #'handle-message
    connection
    (parse-message
     (string-trim '(#\Return) (read-line (stream-of connection)))))
  ;; we return T because READ-MESSAGE-LOOP loops WHILE READ-MESSAGE. There
  ;; is probably a more elegant solution for this.
  t)

(defun read-message-loop (connection)
  "Continuously calls READ-MESSAGE until the connection is closed."
  ;; We keep executing READ-MESSAGE until END-OF-FILE is reached. In that case,
  ;; we check if we wanted to quit, and if not we try to reconnect until that
  ;; succeeds.
  (loop until (handler-case
                  (loop while (read-message connection))
                (end-of-file
                    ()
                  (if (activep connection)
                      (loop until (handler-case
                                      (progn (sleep 5)
                                             (connect connection)
                                             t)
                                    (serious-condition NIL))))))))
