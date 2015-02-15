(asdf:defsystem #:birch
  :serial t
  :description "A simple Common Lisp IRC client library"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:split-sequence #:usocket #:flexi-streams)
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
                                             "init"))))

