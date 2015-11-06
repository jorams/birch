;;;; Generic functions to send commonly used IRC messages. Some of those have an
;;;; awful lot of methods to cover all argument type variations, which should
;;;; probably be improved (Taped over with a little macro).
(defpackage :birch/commands
  (:use :cl)
  (:import-from :birch/connection
                #:connection
                #:user
                #:socket-stream
                #:activep

                #:channel
                #:name
                #:user
                #:nick)
  (:export #:/raw
           #:/pass
           #:/nick
           #:/user
           #:/join
           #:/privmsg
           #:/notice
           #:/invite
           #:/kick
           #:/part
           #:/quit
           #:/pong
           #:/who))
(in-package :birch/commands)

;; /RAW is used to send raw messages to the server. The idea to make it work
;; somewhat like FORMAT came from Colleen, by Shinmera (Nicolas Hafner).
(defgeneric /raw (connection message &rest format-arguments)
  (:documentation "
Sends MESSAGE to the stream associated with CONNECTION, after applying FORMAT
to message with FORMAT-ARGUMENTS.")
  (:method ((connection connection) (message string) &rest format-arguments)
    (with-accessors ((stream socket-stream)) connection
      (apply #'format stream message format-arguments)
      (write-char #\newline stream)
      (finish-output stream))))

(defgeneric /pass (connection pass)
  (:documentation "
Sends a PASS message to CONNECTION, for the purpose of registration.")
  (:method ((connection connection) pass) (/raw connection "PASS ~A" pass)))

(defgeneric /nick (connection nick)
  (:documentation "Sends a NICK message to CONNECTION, to change nicknames.")
  (:method ((connection connection) (nick string))
    (/raw connection "NICK ~A" nick)))

(defgeneric /user (connection user mode real-name)
  (:documentation
   "
Sends a USER message to CONNECTION, for the purpose of registration. Note that
the IRC USER command also takes another parameter between MODE and REAL-NAME,
but this parameter is unused.")
  (:method ((connection connection) (user string) (mode integer)
            (real-name string))
    (/raw connection "USER ~A ~A * :~A" user mode real-name)))

(defgeneric /join (connection channel &optional key)
  (:documentation "Sends a JOIN message to CONNECTION, to join a channel.")
  (:method ((connection connection) (channel string) &optional key)
    (/raw connection "JOIN ~A~@[ ~A~]" channel key))
  (:method ((connection connection) (channel channel) &optional key)
    (/join connection (name channel) key)))

(defgeneric /privmsg (connection channel message)
  (:documentation
   "
Sends a PRIVMSG message to CONNECTION. CHANNEL should either be a channel name
or the name of a user connected to the network.")
  (:method ((connection connection) (channel string) (message string))
    (/raw connection "PRIVMSG ~A :~A" channel message))
  (:method ((connection connection) (channel channel) (message string))
    (/privmsg connection (name channel) message))
  (:method ((connection connection) (user user) (message string))
    (/privmsg connection (nick user) message)))

(defgeneric /notice (connection target message)
  (:documentation
   "
Sends a NOTICE message to CONNECTION. TARGET should either be a channel name or
the name of a user connected to the network.")
  (:method ((connection connection) (target string) (message string))
    (/raw connection "NOTICE ~A :~A" target message))
  (:method ((connection connection) (target channel) (message string))
    (/notice connection (name target) message))
  (:method ((connection connection) (target user) (message string))
    (/notice connection (nick target) message)))

(defgeneric /invite (connection nick channel)
  (:documentation "
Sends an INVITE message to CONNECTION, trying to invite NICK to CHANNEL")
  (:method ((connection connection) (user string) (channel string))
    (/raw connection "INVITE ~A ~A" user channel))
  (:method ((connection connection) (user user) (channel channel))
    (/invite connection (nick user) (name channel)))
  (:method ((connection connection) (user user) (channel string))
    (/invite connection (nick user) channel))
  (:method ((connection connection) (user string) (channel channel))
    (/invite connection user (name channel))))

(defgeneric /kick (connection channel nick &optional message)
  (:documentation "
Sends a KICK message to CONNECTION, trying to kick NICK from CHANNEL")
  (:method ((connection connection) (channel string) (user string)
            &optional message)
    (/raw connection "KICK ~A ~A~@[ :~A~]" channel user message))
  (:method ((connection connection) (channel channel) (user user)
            &optional message)
    (/kick connection (name channel) (nick user) message))
  (:method ((connection connection) (channel string) (user user)
            &optional message)
    (/kick connection channel (nick user) message))
  (:method ((connection connection) (channel channel) (user string)
            &optional message)
    (/kick connection (name channel) user message)))

(defgeneric /part (connection channel &optional message)
  (:documentation "Sends a PART message to CONNECTION, to leave a channel.")
  (:method ((connection connection) (channel string) &optional message)
    (/raw connection "PART ~A~@[ :~A~]" channel message))
  (:method ((connection connection) (channel channel) &optional message)
    (/part connection (name channel) message)))

(defgeneric /quit (connection &optional message)
  (:documentation "
Sends a QUIT message to CONNECTION and sets (ACTIVEP CONNECTION) to T, which
will stop READ-MESSAGE-LOOP from trying to reconnect")
  (:method ((connection connection)
            &optional (message "Birch, Common Lisp IRC library"))
    (setf (activep connection) NIL)
    (/raw connection "QUIT :~A" message)))

(defgeneric /pong (connection server-1 &optional server-2)
  (:documentation "
Sends a PONG message to CONNECTION, usually in reply to a PING by the server")
  (:method ((connection connection) (server-1 string) &optional server-2)
    (if server-2
        (/raw connection "PONG ~A ~A" server-1 server-2)
        (/raw connection "PONG ~A" server-1))))

(defgeneric /who (connection target &optional operators-only-p)
  (:documentation
   "
Sends a WHO message to CONNECTION about TARGET, optionally requesting operators
only. TARGET can be either a user, channel, or string representing the desired
mask.")
  (:method ((connection connection) (target string) &optional operators-only-p)
    (if operators-only-p
        (/raw connection "WHO ~A o" target)
        (/raw connection "WHO ~A" target)))
  (:method ((connection connection) (target user) &optional operators-only-p)
    (/who connection (nick target) operators-only-p))
  (:method ((connection connection) (target channel) &optional operators-only-p)
    (/who connection (name target) operators-only-p)))
