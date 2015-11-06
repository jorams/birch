;;;; Functions to handle messages and turn some of them into events.
(defpackage :birch/events
  (:use :cl)
  (:import-from :birch/connection
                #:connection
                #:user
                #:make-user
                #:nick
                #:rename-user
                #:add-user
                #:remove-user

                #:channel
                #:make-channel
                #:topic
                #:users
                #:channels
                #:channel-type)
  (:import-from :birch/commands
                #:/pong)
  (:import-from :alexandria
                #:removef)
  (:import-from :split-sequence
                #:split-sequence)
  (:export #:event
           #:handle-message
           #:handle-event
           #:define-event-dispatcher
           #:channel-event
           #:privmsg-event
           #:notice-event
           #:join-event
           #:part-event
           #:quit-event
           #:kick-event
           #:nick-event
           #:topic-event
           #:message
           #:channel
           #:target
           #:new-nick
           #:new-topic))
(in-package :birch/events)

;;; Special method combination
(define-method-combination event ()
  ((around (:around))
   (before (:before))
   (primary () :required NIL)
   (after (:after)))
  "The EVENT method combination is used by HANDLE-MESSAGE and HANDLE-EVENT to
gracefully handle the absence of message or event handlers. In standard method
combination, when only a non-primary method exists, an error is signalled. This
makes it very hard to handle errors through an :AROUND method, which would be
the most elegant solution to the problem. This method combination is just like
the standard one, except primary methods are not required."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          form))))

;;; Somewhat low-level message handling

(defgeneric handle-message (connection prefix command params)
  (:documentation "
Called when a raw message is returned. CONNECTION is the connection object of
the connection the message was received on. PREFIX is a list of (NICK USER
HOST) COMMAND is a keyword, such as :PRIVMSG or :RPL_WELCOME PARAMS is a list
of parameters")
  (:method-combination event))

(defmethod no-applicable-method ((function (eql #'handle-message)) &rest args)
  (declare (ignore function args)))

;;; Default message handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-message ((connection connection)
                           prefix
                           (command (eql :PING))
                           params)
  "Responds to a PING message by the server by sending a PONG message in
return"
  (destructuring-bind (server-1 &optional server-2)
      params
    (if server-2
        (/pong connection server-1 server-2)
        (/pong connection server-1))))

(defmethod handle-message ((connection connection)
                           prefix
                           (command (eql :RPL_NAMREPLY))
                           params)
  "Handles an RPL_NAMREPLY message and updates the channel."
  (let ((channel (make-channel connection (third params))))
    (setf (channel-type channel)
          (case (char (second params) 0)
            (#\@ :secret)
            (#\* :private)
            (#\= :public)))
    (dolist (name (split-sequence #\space (fourth params)
                                  :remove-empty-subseqs t))
      (add-user (make-user connection
                           (cond
                             ;; Strip operator symbol prefix
                             ;; (anything that is not letter | special).
                             ((let ((first (char name 0)))
                                (not (or (alpha-char-p first)
                                         (find first "[]\\`_^{|}"))))
                              (subseq name 1))
                             (t name)))
                channel))))

(defmethod handle-message ((connection connection)
                           prefix
                           (command (eql :RPL_WHOREPLY))
                           params)
  "Handles an RPL_WHOREPLY message and updates the users and channels
associated."
  (destructuring-bind
        (self-nick channel user host server nick flags hopcount-and-real-name)
      params
    (declare (ignore self-nick server flags hopcount-and-real-name))
    (add-user (make-user connection (list nick user host))
              (make-channel connection channel))))

;;; The event system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass event ()
  ((user :initarg :user
         :initform NIL
         :accessor user)
   (message :initarg :message
            :initform NIL
            :accessor message)))

(defgeneric handle-event (connection event)
  (:documentation "
Will be called after an IRC message has successfully been parsed and turned
into an event. Most IRC messages don't result in events, should you want to
handle them you can define a method on HANDLE-MESSAGE instead.")
  (:method-combination event))

(defmethod no-applicable-method ((function (eql #'handle-event)) &rest args)
  (declare (ignore function args)))

(defmacro define-event-dispatcher (command class &optional positional-initargs)
  "Defines a method on HANDLE-MESSAGE to handle messages of which the command
is COMMAND. This new method will call HANDLE-EVENT with a new instance of type
CLASS.

POSITIONAL-INITARGS should be a list of initargs to pass to MAKE-INSTANCE,
where the position of the keyword determines the IRC command parameter that
will be used as a value. A NIL will cause an IRC parameter to be ignored.

For example, when POSITIONAL-INITARGS is (:CHANNEL), the first parameter of the
IRC message will be passed as the initial value of :CHANNEL. If
POSITIONAL-INITARGS is (:CHANNEL :TARGET), the first parameter will be passed
as the initial value of :CHANNEL, and the second parameter will be passed as
the initial value of :TARGET.

Instead of a keyword, an element of POSITIONAL-INITARGS can also be a list of
the form (:KEYWORD FUNCTION), which means the value passed as the initarg will
be the result of calling FUNCTION with two arguments: the connection object and
the IRC parameter.

Any remaining arguments will be joined together (separated by spaces) and
passed as the initial value of :MESSAGE."
  `(defmethod handle-message
       ((connection connection) prefix (command (eql ,command)) params)
     (let ((user (make-user connection prefix)))
       (handle-event
        connection
        (make-instance ,class
                       :user user
                       ,@(loop for i from 0
                               for arg in positional-initargs
                               if (consp arg)
                                 append `(,(first arg)
                                          (funcall ,(or (second arg) #'identity)
                                                   connection (elt params ,i)))
                               else if (keywordp arg)
                                      append `(,arg (elt params ,i)))
                       :message (format NIL
                                        "~{~A~^ ~}"
                                        (nthcdr ,(length positional-initargs)
                                                params)))))))

;;; Event-dispatching message handlers and the events they dispatch ;;;;;;;;;;;

(defclass channel-event (event)
  ((channel :initarg :channel
            :initform NIL
            :accessor channel))
  (:documentation "
A CHANNEL-EVENT is an event that happens on a certain channel."))

(defclass privmsg-event (channel-event) ()
  (:documentation "
Event dispatched when a PRIVMSG message is received from the server. Note that
when the CHANNEL slot is STRING= to the current nickname this privmsg won't
have been sent to a channel but directly to you."))
(define-event-dispatcher :PRIVMSG 'privmsg-event ((:channel #'make-channel)))

(defclass notice-event (channel-event) ()
  (:documentation "
Event dispatched when a NOTICE message is received from the server. Note that
when the CHANNEL slot is STRING= to the current nickname this notice won't have
been sent to a channel but directly to you."))
(define-event-dispatcher :NOTICE 'notice-event ((:channel #'make-channel)))

(defclass join-event (channel-event) ())
(define-event-dispatcher :JOIN 'join-event ((:channel #'make-channel)))

(defclass part-event (channel-event) ())
(define-event-dispatcher :PART 'part-event ((:channel #'make-channel)))

(defclass quit-event (event) ())
(define-event-dispatcher :QUIT 'quit-event)

(defclass kick-event (channel-event)
  ((target :initarg :target
           :initform NIL
           :accessor target)))
(define-event-dispatcher :KICK 'kick-event ((:channel #'make-channel)
                                            (:target #'make-user)))

(defclass nick-event (event)
  ((new-nick :initarg :new-nick
             :initform NIL
             :accessor new-nick)))
(define-event-dispatcher :NICK 'nick-event (:new-nick))

(defclass topic-event (channel-event)
  ((new-topic :initarg :new-topic
              :initform NIL
              :accessor new-topic)))
(define-event-dispatcher :TOPIC 'topic-event ((:channel #'make-channel)
                                              :new-topic))
(define-event-dispatcher :RPL_TOPIC 'topic-event (nil
                                                  (:channel #'make-channel)
                                                  :new-topic))

;;; Default event handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-event ((connection connection) (event nick-event))
  "Change the nickname associated with a user, or of the connection if the old
nickname is the nickname of the connection."
  (rename-user connection (nick (user event)) (new-nick event)))

(defmethod handle-event ((connection connection) (event topic-event))
  "Change the topic associated with a channel."
  (setf (topic (channel event))
        (new-topic event)))

(defmethod handle-event ((connection connection) (event join-event))
  "Add a user to a channel."
  (add-user (user event) (channel event)))

(defmethod handle-event ((connection connection) (event part-event))
  "Remove a user from a channel they're leaving."
  (remove-user (user event) (channel event)))

(defmethod handle-event ((connection connection) (event quit-event))
  "Remove a user from all channels they're in."
  (remove-user (user event)))

(defmethod handle-event ((connection connection) (event kick-event))
  "Remove a user from a channel they're kicked from."
  (remove-user (target event) (channel event)))
