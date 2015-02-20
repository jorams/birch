;;;; CONNECTION class needed by other files
(defpackage :birch/connection
  (:use :cl)
  (:import-from :alexandria
                #:removef
                #:ensure-list)
  (:export #:connection
           #:socket-of
           #:stream-of
           #:activep
           #:server-host-of
           #:server-port-of
           #:nick-of
           #:user-of
           #:pass-of
           #:real-name-of

           #:connection-of

           #:channel-type
           #:channel
           #:make-channel
           #:name-of
           #:topic-of
           #:channel-type-of

           #:user
           #:make-user
           #:channels-of
           #:users-of

           #:add-user
           #:rename-user
           #:remove-user))
(in-package :birch/connection)

;;; Core classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass user ()
  ((connection :initarg :connection
               :reader connection-of)
   (nick :initarg :nick
         :accessor nick-of)
   (user :initarg :user
         :initform nil
         :accessor user-of)
   (host :initarg :host
         :initform nil
         :accessor host-of)
   (channels :initarg :channels
             :initform ()
             :accessor channels-of)))

(deftype channel-type ()
  '(member :public :private :secret))

(defclass channel ()
  ((connection :initarg :connection
               :reader connection-of)
   (name :initarg :name
         :reader name-of)
   (users :initarg :users
          :initform ()
          :accessor users-of)
   (topic :initarg :topic
          :initform ""
          :accessor topic-of)
   (channel-type :initarg :channel-type
                 :type channel-type
                 :accessor channel-type-of)))

(defclass connection (user)
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
   (server-host :initarg :server-host
                :initform (error "Host required, not specified")
                :reader server-host-of)
   (server-port :initarg :server-port
                :initform 6667
                :accessor server-port-of)
   (pass :initarg :pass
         :initform NIL
         :accessor pass-of)
   (real-name :initarg :real-name
              :initform "Birch IRC library"
              :accessor real-name-of)
   (users :initarg :users
          :initform ()
          :accessor users-of))
  (:default-initargs :nick (error "Nick required, not specified")))

(defmethod initialize-instance :after ((connection connection)
                                       &key &allow-other-keys)
  (push connection (users-of connection)))

;;; Channel and user API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-channel (connection name)
  (find name (channels-of connection)
        :test #'string=
        :key #'name-of))

(defun get-user (connection nick &optional user host)
  (let ((found (find nick (users-of connection)
                     :test #'string=
                     :key #'nick-of)))
    (when (and found user host)
      (setf (user-of found) user
            (host-of found) host))
    found))

(defun valid-channel-name-p (name)
  (member (char name 0) '(#\& #\# #\+ #\!)))

(defun make-channel (connection name)
  "Turn a channel name into a CHANNEL object. For unknown channels this will
result in a new CHANNEL object, for ones that are already known the existing one
will be returned. NIL is returned if NAME is not a valid channel name."
  (when (valid-channel-name-p name)
    (or (get-channel connection name)
        (let ((channel (make-instance 'channel
                                      :connection connection
                                      :name name)))
          (push channel (channels-of connection))
          channel))))

(defun make-user (connection nick/prefix)
  "Turn a user's nick or prefix into a USER object. For unknown users this will
result in a new USER object, for ones that are already known the existing one
will be returned. If the user is already known and NICK/PREFIX is a prefix with
both a USER and HOST component, those slots of the user object will be updated."
  (destructuring-bind (nick &optional user host)
      (ensure-list nick/prefix)
    (or (get-user connection nick user host)
        (let ((user (make-instance 'user
                                   :connection connection
                                   :nick nick
                                   :user user
                                   :host host)))
          (push user (users-of connection))
          user))))

(defun rename-user (connection old-nick new-nick)
  (setf (nick-of (get-user connection old-nick)) new-nick))

(defun add-user (user channel)
  (pushnew user (users-of channel))
  (pushnew channel (channels-of user)))

(defun remove-user (user &optional channel)
  (cond
    (channel (removef (channels-of user) channel)
             (removef (users-of channel) user))
    (t (dolist (channel (channels-of user))
         (remove-user user channel))))
  (when (not (channels-of user))
    (removef (users-of (connection-of user)) user)))
