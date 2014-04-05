;;;; Functions to handle messages and turn some of them into events.
(in-package #:birch)

;;; Special method combination 
(define-method-combination event ()
  ((around (:around))
   (before (:before))
   (primary () :required NIL)
   (after (:after)))
  "The EVENT method combination is used by HANDLE-MESSAGE and HANDLE-EVENT to
   gracefully handle the absence of message or event handlers. In standard
   method combination, when only a non-primary method exists, an error is
   signalled. This makes it very hard to handle errors through an :AROUND
   method, which would be the most elegant solution to the problem. This method
   combination is just like the standard one, except primary methods are not
   required."
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
  (:documentation "Called when a raw message is returned.
                   CONNECTION is the connection object of the connection the
                   message was received on.
                   PREFIX is a list of (NICK USER HOST)
                   COMMAND is a keyword, such as :PRIVMSG or :RPL_WELCOME
                   PARAMS is a list of parameters")
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
      (pong connection server-1 server-2)
      (pong connection server-1))))

;;; The event system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass event ()
  ((nick :initarg :nick
         :initform NIL
         :accessor nick-of)
   (user :initarg :user
         :initform NIL
         :accessor user-of)
   (host :initarg :host
         :initform NIL
         :accessor host-of)
   (message :initarg :message
            :initform NIL
            :accessor message-of)))

(defgeneric handle-event (connection event)
  (:documentation "Will be called after an IRC message has successfully been
                   parsed and turned into an event. Most IRC messages don't
                   result in events, should you want to handle them you can
                   define a method on HANDLE-MESSAGE instead.")
  (:method-combination event))

(defmethod no-applicable-method ((function (eql #'handle-event)) &rest args)
  (declare (ignore function args)))

(defmacro define-event-dispatcher (command class &optional positional-initargs)
  "Defines a method on HANDLE-MESSAGE to handle messages of which the command
   is COMMAND. This new method will call HANDLE-EVENT with a new instance of
   type CLASS.

   POSITIONAL-INITARGS should be a list of initargs to pass to MAKE-INSTANCE,
   where the position of the keyword determines the IRC command parameter that
   will be used as a value.

   For example, when POSITIONAL-INITARGS is (:CHANNEL), the first parameter of
   the IRC message will be passed as the initial value of :CHANNEL.
   If POSITIONAL-INITARGS is (:CHANNEL :TARGET), the first parameter will be
   passed as the initial value of :CHANNEL, and the second parameter will be
   passed as the initial value of :TARGET.

   Any remaining arguments will be joined together (separated by spaces) and
   passed as the initial value of :MESSAGE."
  `(defmethod handle-message
       ((connection connection) prefix (command (eql ,command)) params)
     (destructuring-bind (nick user host)
         prefix
       (handle-event
	connection
	(make-instance ,class
		       :nick nick
		       :user user
		       :host host
		       ,@(loop for i from 0
			    for arg in positional-initargs
			    append `(,arg (elt params ,i)))
		       :message (format NIL
					"~{~A~^ ~}"
					(nthcdr ,(length positional-initargs)
						params)))))))

;;; Event-dispatching message handlers and the events they dispatch ;;;;;;;;;;;

(defclass channel-event (event)
  ((channel :initarg :channel
            :initform NIL
            :accessor channel-of))
  (:documentation "A CHANNEL-EVENT is an event that happens on a certain
                   channel."))

(defclass privmsg-event (channel-event) ()
  (:documentation "Event dispatched when a PRIVMSG message is received from the
                   server. Note that when the CHANNEL slot is STRING= to the
                   current nickname this privmsg won't have been sent to a
                   channel but directly to you.") )
(define-event-dispatcher :PRIVMSG 'privmsg-event (:channel))

(defclass notice-event (channel-event) ()
  (:documentation "Event dispatched when a NOTICE message is received from the
                   server. Note that when the CHANNEL slot is STRING= to the
                   current nickname this notice won't have been sent to a
                   channel but directly to you."))
(define-event-dispatcher :NOTICE 'notice-event (:channel))

(defclass join-event (channel-event) ())
(define-event-dispatcher :JOIN 'join-event (:channel))

(defclass part-event (channel-event) ())
(define-event-dispatcher :PART 'part-event (:channel))

(defclass quit-event (event) ())
(define-event-dispatcher :QUIT 'quit-event)

(defclass kick-event (channel-event)
  ((target :initarg :target
           :initform NIL
           :accessor target-of)))
(define-event-dispatcher :KICK 'kick-event (:channel :target))

(defclass nick-event (event)
  ((new-nick :initarg :new-nick
             :initform NIL
             :accessor new-nick-of)))
(define-event-dispatcher :NICK 'nick-event (:new-nick))

(defclass topic-event (channel-event)
  ((new-topic :initarg :new-topic
              :initform NIL
              :accessor new-topic-of)))
(define-event-dispatcher :TOPIC 'nick-event (:new-topic))

;;; Default event handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-event ((connection connection) (event nick-event))
  "Changes the nickname associated with a connection when a NICK message for
   the current nickname is received."
  (if (string= (nick-of event) (new-nick-of event))
      (setf (nick-of connection) (new-nick-of event))))
