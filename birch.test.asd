(asdf:defsystem #:birch.test
  :serial t
  :description "Tests for Birch"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:birch #:prove #:flexi-streams)
  :pathname "test"
  :components ((:file "util")
               (:file "ctcp")
               (:file "parse")
               (:file "commands")
               (:file "events")
               (:file "connection"))
  :perform (test-op :after (op component)
             (let ((results (mapcar (intern #.(string :run-test-package) :prove)
                                    (list :birch.test/ctcp
                                          :birch.test/parse
                                          :birch.test/commands
                                          :birch.test/events
                                          :birch.test/connection))))
               (unless (every #'identity results)
                 (error "Tests failed.")))))
