(asdf:defsystem #:birch.test
  :serial t
  :description "Tests for Birch"
  :author "Joram Schrijver <i@joram.io>"
  :license "LLGPL"
  :depends-on (#:birch #:fiasco)
  :pathname "test"
  :components ((:file "ctcp")
               (:file "parse")))
