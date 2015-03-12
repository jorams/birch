(in-package :asdf-user)
(asdf:defsystem #:birch
  :description "A simple Common Lisp IRC client library"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:split-sequence #:usocket #:flexi-streams #:alexandria)
  :pathname "src"
  :components ((:file "replies")
               (:file "parse")
               (:file "ctcp")
               (:file "connection")
               (:file "commands" :depends-on ("connection"))
               (:file "events" :depends-on ("connection" "commands"))
               (:file "init" :depends-on ("connection" "parse" "commands"))
               (:file "package" :depends-on ("replies"
                                             "parse"
                                             "ctcp"
                                             "connection"
                                             "commands"
                                             "events"
                                             "init")))
  :in-order-to ((test-op (test-op :birch.test))))
