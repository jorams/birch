# Birch

[![Build Status](https://travis-ci.org/jorams/birch.svg?branch=master)](https://travis-ci.org/jorams/birch)

Birch is a simple Common Lisp IRC client library. It makes use of CLOS for
event handling.

## Dependencies

Birch is built in Common Lisp on SBCL. It depends on:

- [SPLIT-SEQUENCE](http://www.cliki.net/split-sequence) (Public Domain?)
- [usocket](http://common-lisp.net/project/usocket/) (MIT)
- [FLEXI-STREAMS](http://weitz.de/flexi-streams/) (BSD)
- [Alexandria](http://common-lisp.net/project/alexandria/) (Public Domain)

The tests also use [Fiasco](https://github.com/capitaomorte/fiasco) (Public Domain).

## Installation

Birch can be loaded with Quicklisp: (but is not currently in the Quicklisp dist)

```lisp
(ql:quickload :birch)
```

## Usage

The first step is to create a subclass of `CONNECTION`:

```lisp
(defclass my-connection (connection) ())
```

To connect to an IRC network you create an instance of your connection class. You then pass that instance to `CONNECT` to actually connect to the network.

```lisp
(defvar *connection* (make-instance 'my-connection
                                    :server-host "irc.example.com"
                                    :nick "mybot"))
```

The only required initargs for the default connection class are `:SERVER-HOST` and `:NICK`. The others of interest are:

- `:SERVER-PORT`, the port to connect to (defaults to 6667)
- `:USER`, the username to send upon initial connection (defaults to the supplied nickname)
- `:PASS`, the password to send. If not supplied no password will be sent.
- `:REAL-NAME`, the real name to register with (defaults to "Birch IRC library")

The accessors for all of those are the name of the slot (the initarg without the ':').

Event handling in Birch is done by defining methods on `HANDLE-EVENT`.

```lisp
(defgeneric handle-event (connection event)
  (:documentation "Will be called after an IRC message has successfully been
                   parsed and turned into an event. Most IRC messages don't
                   result in events, should you want to handle them you can
                   define a method on HANDLE-MESSAGE instead."))
```

For example, to do something when a PRIVMSG is received, you define a method specializing the first argument on your connection class and the second on the PRIVMSG-EVENT class:

```lisp
(defmethod handle-event ((connection my-connection) (event privmsg-event))
  (format t "Message received on ~A: ~A" (channel event) (message event)))
```

A list of all the events currently included in Birch is [below](#events).

Sending commands to a connection can be done in two ways. You can use the generic function `/RAW`, which is basically a glorified `FORMAT`, or you can use one of the built-in functions for sending often-used commands. They're all generic functions and can thus easily be extended.

`RAW` can, for example, be called like this:

```lisp
(/raw connection "JOIN ~A" channel)
```
But you should probably just use the function `/JOIN` in this case.

All currently implemented commands are listed [below](#commands).

To start handling messages you should call `PROCESS-MESSAGE-LOOP`, which will block until the connection to the server is closed. Even then, if `/QUIT` wasn't called (so the `ACTIVEP` slot on the connection wasn't set to `NIL`) `PROCESS-MESSAGE-LOOP` will try to reconnect. You'll likely want to run this in a new thread.

Alternatively, you can call `PROCESS-MESSAGE` yourself.


## We have to go deeper

If you want to handle a message from the server for which there is no event you can instead define a method on `HANDLE-MESSAGE`.

```lisp
(defgeneric handle-message (connection prefix command params)
  (:documentation "Called when a raw message is returned.
                   CONNECTION is the connection object of the connection the
                   message was received on.
                   PREFIX is a list of (NICK USER HOST)
                   COMMAND is a keyword, such as :PRIVMSG or :RPL_WELCOME
                   PARAMS is a list of parameters"))
```

For example, to do something when `RPL_WELCOME` is received, you could define a method like so:

```lisp
(defmethod handle-message ((connection my-connection)
                           prefix
                           (command (eql :RPL_WELCOME))
                           params)
  (format t "Received RPL_WELCOME, we can now do stuff"))
```

The `COMMAND` argument will either be the keyword-ized name of the command as defined in RFC2812, like `:PRIVMSG`, or, in the case of numeric replies, the name as found [here](https://www.alien.net.au/irc/irc2numerics.html).

If you want to handle something as an event you can define a method on `HANDLE-MESSAGE` that calls `HANDLE-EVENT` with a newly initialized `EVENT` object. The macro `DEFINE-EVENT-DISPATCHER` can be of great help with that.

```lisp
(defmacro define-event-dispatcher (command class &optional positional-initargs)
  "Defines a method on HANDLE-MESSAGE to handle messages of which the command
   is COMMAND. This new method will call HANDLE-EVENT with a new instance of
   type CLASS.

   POSITIONAL-INITARGS should be a list of initargs to pass to MAKE-INSTANCE,
   where the position of the keyword determines the IRC command parameter that
   will be used as a value. A NIL will cause an IRC parameter to be ignored.

   For example, when POSITIONAL-INITARGS is (:CHANNEL), the first parameter of
   the IRC message will be passed as the initial value of :CHANNEL.
   If POSITIONAL-INITARGS is (:CHANNEL :TARGET), the first parameter will be
   passed as the initial value of :CHANNEL, and the second parameter will be
   passed as the initial value of :TARGET.

   Instead of a keyword, an element of POSITIONAL-INITARGS can also be a list of
   the form (:KEYWORD FUNCTION), which means the value passed as the initarg
   will be the result of calling FUNCTION with two arguments: the connection
   object and the IRC parameter.

   Any remaining arguments will be joined together (separated by spaces) and
   passed as the initial value of :MESSAGE."
  ...)
```

For example, kick events are implemented like this:

```lisp
(defclass kick-event (channel-event)
  ((target :initarg :target
           :initform NIL
           :accessor target)))

(define-event-dispatcher :KICK 'kick-event ((:channel #'make-channel)
                                            (:target #'make-user)))
```

## Events

- `EVENT`, which is a superclass of all events.

    `EVENT` has a couple of slots, which you can access using the following accessors:

    - `NICK`, the nickname of the sender of the message. Taken from the message's prefix.
    - `USER`, the username of the sender of the message. Also taken from the message's prefix.
    - `HOST`, the hostname of the sender of the message. Also taken from the message's prefix.
    - `MESSAGE`, a string containing all of the parameters supplied with the message, separated by spaces. Note that the "trailing" parameter is not prefixed with a ':', so you can't recognize it from here.

    Subclasses of `EVENT` are:
    - `QUIT-EVENT`
    - `NICK-EVENT` which adds a slot, accessed with `NEW-NICK`. It contains the new nick of the person whose nick has changed.

- `CHANNEL-EVENT`, which is a superclass of all events happening on a certain channel.

    `CHANNEL-EVENT` adds another slot, accessed with `CHANNEL`.
    The subclasses of `CHANNEL-EVENT` are:

    - `PRIVMSG-EVENT`
    - `NOTICE-EVENT`
    - `JOIN-EVENT`
    - `PART-EVENT`
    - `PART-EVENT`
    - `KICK-EVENT` which adds a slot, accessed with `TARGET`. It contains the nick of the person being kicked.

    - `TOPIC-EVENT` which adds a slot, accessed with `NEW-TOPIC`. It contains the new topic of the channel.

## Commands

- `(/pass connection password)`
- `(/nick connection nick)`
- `(/user connection username mode real-name)`
- `(/join connection channel &optional key)`
- `(/privmsg connection channel message)`
- `(/invite connection nick channel)`
- `(/kick connection channel nick &optional message)`
- `(/part connection channel &optional message)`
- `(/quit connection &optional message)`
- `(/pong connection server-1 &optional server-2)` You shouldn't need this, as Birch automatically responds to PING.

## Users and Channels

Birch keeps track of users and channels for you. The default events call `MAKE-USER` and `MAKE-CHANNEL` on the appropriate parameters, turning them into `USER` and `CHANNEL` objects with appropriate slots. To get a list of all users in a channel, call `USERS` on the channel object. Similarly, to get all channels a user is in (that we know of), call `CHANNELS` on a user object.

It is worth noting that `CONNECTION` is itself a subclass of `USER` and will appear in appear channel user lists.

## CTCP

Birch provides two utility functions for working with [CTCP](https://en.wikipedia.org/wiki/Client-to-client_protocol) messages.

- `MAKE-CTCP-MESSAGE`, which takes a string as an argument and adds an ASCII 0x01 character to the start and end of it.

    The result of `MAKE-CTCP-MESSAGE` can be sent to a connection using PRIVMSG or NOTICE to send a CTCP query to someone.

- `CTCP-MESSAGE-P`, which takes a string as an argument and checks if it starts and ends with an ASCII 0x01 character.

    To handle CTCP you can pass messages received through PRIVMSG or NOTICE through `CTCP-MESSAGE-P` and parse them as such if they are, in fact, CTCP messages.

## Notes

- The test coverage is, at the moment, very low. The only things being tested are the message parser, the CTCP utilities and the command-sending functions.
- Birch does not handle errors in your event handlers, which can result in the connection staying open while the bot/client/whatever has crashed. This can be prevented, for example, by defining an `:AROUND` method on `HANDLE-EVENT`, like so:

  ```lisp
  (defmethod handle-event :around ((connection my-connection) (event event))
    (handler-case
      (if (next-method-p)
        (call-next-method))
      (serious-condition (condition)
        (format *trace-output* "~&Caught error: ~A~%" condition))))
  ```

  `HANDLE-MESSAGE` and `HANDLE-EVENT` use a modified standard method combination to allow for `:AROUND` methods to exist when there are no primary methods without signalling an (apparently implementation-defined) error.

## License

    Copyright (c) 2015 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
