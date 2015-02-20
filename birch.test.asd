(in-package :asdf-user)
(asdf:defsystem #:birch.test
  :serial t
  :description "Tests for Birch"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:birch #:fiasco #:flexi-streams)
  :pathname "test"
  :components ((:file "ctcp")
               (:file "parse")
               (:file "commands"))
  :perform (test-op :after (op component)
                    (funcall (intern #.(string :run-package-tests) :fiasco)
                             :packages (list :birch.test/ctcp
                                             :birch.test/parse
                                             :birch.test/commands))))
