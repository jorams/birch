;;;; CONNECTION class needed by other files
(in-package #:birch)

(defclass connection ()
  ((socket :type usocket:stream-usocket
           :initarg :socket
           :initform NIL
           :accessor socket-of
           :documentation "An instance of USOCKET:STREAM-USOCKET, the current
                           socket connection to the server.")
   (socket-stream :initarg :stream
                  :initform NIL
                  :accessor stream-of
                  :documentation "The stream associated with the current socket
                                  connection to the server.")
   (activep :initarg :activep
            :initform NIL
            :accessor activep
            :documentation "Whether or not the connection to the server is
                               supposed to be open. In case of connection
                               issues READ-MESSAGE-LOOP uses this to determine
                               whether or not to reconnect")
   (host :initarg :host
         :initform (error "Host required, not specified")
         :accessor host-of)
   (port :initarg :port
         :initform 6667
         :accessor port-of)
   (nick :initarg :nick
         :initform (error "Nick required, not specified")
         :accessor nick-of)
   (user :initarg :user
         :initform NIL
         :accessor user-of)
   (pass :initarg :pass
         :initform NIL
         :accessor pass-of)
   (real-name :initarg :real-name
              :initform "Birch IRC library"
              :accessor real-name-of)))
