(asdf:defsystem #:birch.test
  :serial t
  :description "Tests for Birch"
  :author "Joram Schrijver <i@joram.io>"
  :license "LLGPL"
  :depends-on (#:birch #:rt)
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "ctcp")
                 (:file "parse")))))

