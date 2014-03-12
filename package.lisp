(defpackage #:birch
  (:use #:cl #:split-sequence)
  (:export #:connection
           #:connect
           #:read-message-loop

           ;; Connection accessors
           #:socket-of
           #:stream-of
           #:activep
           #:host-of
           #:port-of
           #:nick-of
           #:user-of
           #:pass-of
           #:real-name-of
           
           ;; Events and messages
           #:handle-message
           #:handle-event
           #:define-event-dispatcher
           #:event
           #:channel-event
           #:privmsg-event
           #:notice-event
           #:join-event
           #:part-event
           #:quit-event
           #:kick-event
           #:nick-event
           #:topic-event

           ;; Event accessors
           #:message-of
           #:channel-of
           #:target-of
           #:new-nick-of
           #:new-topic-of
           
           ;; Commands
           #:raw
           #:pass
           #:nick
           #:user
           #:join
           #:privmsg
           #:invite
           #:kick
           #:part
           #:quit
           #:pong))

