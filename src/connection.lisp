;;;; CONNECTION class needed by other files
(defpackage :birch/connection
  (:use :cl)
  (:import-from :alexandria
                #:removef
                #:ensure-list)
  (:export #:connection
           #:socket-stream
           #:connect-socket
           #:activep
           #:server-host
           #:server-port
           #:nick
           #:user
           #:pass
           #:real-name

           #:channel-class
           #:channel-type
           #:channel
           #:make-channel
           #:name
           #:topic
           #:channel-type

           #:user-class
           #:user
           #:host
           #:make-user
           #:channels
           #:users

           #:add-user
           #:rename-user
           #:remove-user))
(in-package :birch/connection)

;;; Core classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass user ()
  ((connection :initarg :connection
               :accessor connection)
   (nick :initarg :nick
         :accessor nick)
   (user :initarg :user
         :initform nil
         :accessor user)
   (host :initarg :host
         :initform nil
         :accessor host)
   (channels :initarg :channels
             :initform ()
             :accessor channels)))

(deftype channel-type ()
  '(member :public :private :secret))

(defclass channel ()
  ((connection :initarg :connection
               :reader connection)
   (name :initarg :name
         :reader name)
   (users :initarg :users
          :initform ()
          :accessor users)
   (topic :initarg :topic
          :initform ""
          :accessor topic)
   (channel-type :initarg :channel-type
                 :type channel-type
                 :accessor channel-type)))

(defclass connection (user)
  ((%socket :initarg :socket
            :initform NIL
            :accessor %socket
            :documentation "
An instance of USOCKET:STREAM-USOCKET, the current socket connection to the
server.")
   (socket-stream :initarg :stream
                  :initform NIL
                  :accessor socket-stream
                  :documentation "
The stream associated with the current socket connection to the server.")
   (activep :initarg :activep
            :initform NIL
            :accessor activep
            :documentation "
Whether or not the connection to the server is supposed to be open. In case of
connection issues READ-MESSAGE-LOOP uses this to determine whether or not to
reconnect")
   (server-host :initarg :server-host
                :initform (error "Host required, not specified")
                :reader server-host)
   (server-port :initarg :server-port
                :initform 6667
                :accessor server-port)
   (ssl :initarg :ssl
        :initform NIL
        :accessor ssl
        :documentation "
Whether to use SSL for the IRC connection.")
   (ssl-verify :initarg :ssl
               :initform T
               :accessor ssl-verify
               :documentation "
Whether to verify the SSL hostname.")
   (pass :initarg :pass
         :initform NIL
         :accessor pass)
   (real-name :initarg :real-name
              :initform "Birch IRC library"
              :accessor real-name)
   (users :initarg :users
          :initform ()
          :accessor users)
   (user-class :initarg :user-class
               :initform 'user
               :accessor user-class)
   (channel-class :initarg :channel-class
                  :initform 'channel
                  :accessor channel-class))
  (:default-initargs :nick (error "Nick required, not specified")))

(defmethod initialize-instance :after ((connection connection)
                                       &key &allow-other-keys)
  (setf (connection connection) connection)
  (push connection (users connection)))

(defun connect-socket (connection)
  (let* ((socket (usocket:socket-connect (server-host connection)
                                         (server-port connection)
                                         :element-type '(unsigned-byte 8)))
         (socket-stream
           (if (ssl connection)
               #-birch-no-ssl (cl+ssl:make-ssl-client-stream
                             (usocket:socket-stream socket)
                             :hostname (server-host connection)
                             :verify (ssl-verify connection))
               #+birch-no-ssl (error "birch-no-ssl feature prohibits SSL support.")
               (usocket:socket-stream socket))))
    ;; Initialization
    (setf (%socket connection) socket
          (socket-stream connection)
          (flexi-streams:make-flexi-stream
           socket-stream
           :external-format '(:UTF-8 :eol-style :crlf))
          (activep connection) t)))

;;; Channel and user API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-channel (connection name)
  (find name (channels connection)
        :test #'string=
        :key #'name))

(defun get-user (connection nick &optional user host)
  (let ((found (find nick (users connection)
                     :test #'string=
                     :key #'nick)))
    (cond ((eq connection found) found)
          ((and found (null (channels found)))
           ;; If we've seen a user before but they aren't in any of the
           ;; channels we know of, we don't actually know if it's the same
           ;; person. Thus any existing USER object is irrelevant.
           ;;
           ;; A better way to handle this would be to never put those users
           ;; into the list, but due to the way the tracking is set up that's
           ;; not known at the time of creation.
           (prog1 nil
             (removef (users connection) found)))
          ((and found (channels found))
           (prog1 found
             (when (and user host)
               (setf (user found) user
                     (host found) host)))))))

(defun valid-channel-name-p (name)
  (member (char name 0) '(#\& #\# #\+ #\!)))

(defun make-channel (connection name)
  "Turn a channel name into a CHANNEL object. For unknown channels this will
result in a new CHANNEL object, for ones that are already known the existing
one will be returned. NIL is returned if NAME is not a valid channel name."
  (when (valid-channel-name-p name)
    (or (get-channel connection name)
        (let ((channel (make-instance (channel-class connection)
                                      :connection connection
                                      :name name)))
          (push channel (channels connection))
          channel))))

(defun make-user (connection nick/prefix)
  "Turn a user's nick or prefix into a USER object. For unknown users this will
result in a new USER object, for ones that are already known the existing one
will be returned. If the user is already known and NICK/PREFIX is a prefix with
both a USER and HOST component, those slots of the user object will be
updated."
  (destructuring-bind (nick &optional user host)
      (ensure-list nick/prefix)
    (or (get-user connection nick user host)
        (let ((user (make-instance (user-class connection)
                                   :connection connection
                                   :nick nick
                                   :user user
                                   :host host)))
          (push user (users connection))
          user))))

(defun rename-user (connection old-nick new-nick)
  (setf (nick (get-user connection old-nick)) new-nick))

(defun add-user (user channel)
  (pushnew user (users channel))
  (pushnew channel (channels user)))

(defun remove-user (user &optional channel)
  (cond
    (channel (removef (channels user) channel)
             (removef (users channel) user)
             (when (eq user (connection user))
               (dolist (other-user (users channel))
                 (remove-user other-user channel))))
    (t (dolist (channel (channels user))
         (remove-user user channel))))
  (when (and (not (channels user))
             (not (eq user (connection user))))
    (removef (users (connection user)) user)))
