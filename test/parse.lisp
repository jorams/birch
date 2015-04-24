(defpackage :birch.test/parse
  (:use :cl :birch/parse :birch/replies :prove))
(in-package :birch.test/parse)

(diag "Parse tests")
(plan 3)

(deftest "parse-prefix"
  (plan 3)
  (is (multiple-value-list (parse-prefix ":WiZ!jto@tolsun.oulu.fi"))
      '("WiZ" "jto" "tolsun.oulu.fi"))
  (is (multiple-value-list (parse-prefix ":WiZ@tolsun.oulu.fi"))
      '("WiZ" nil "tolsun.oulu.fi"))
  (is (multiple-value-list (parse-prefix ":WiZ"))
      '("WiZ" nil nil)))

;; Also test replies, whose primary use is in parsing
(deftest "reply->keyword"
  (plan 3)
  (ok (eq (reply->keyword 1) :RPL_WELCOME))
  (ok (eq (reply->keyword 974) :ERR_CANNOTCHANGECHANMODE))
  (ok (eq (reply->keyword "PRIVMSG") :PRIVMSG)))

(deftest "parse-message"
  (plan 3)
  (is (multiple-value-list (parse-message "PRIVMSG Supertest"))
      '(nil :PRIVMSG ("Supertest")))
  (is (multiple-value-list (parse-message "PRIVMSG"))
      '(nil :PRIVMSG ()))
  ;; Note that this also depends on PARSE-PREFIX functioning correctly
  (is (multiple-value-list (parse-message ":test PRIVMSG"))
      '(("test" nil nil) :PRIVMSG ())))
