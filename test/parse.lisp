(fiasco:define-test-package :birch.test/parse
  (:use :cl :birch/parse :birch/replies))
(in-package :birch.test/parse)

(deftest test-parse-prefix ()
  (is (equal (multiple-value-list (parse-prefix ":WiZ!jto@tolsun.oulu.fi"))
             '("WiZ" "jto" "tolsun.oulu.fi")))
  (is (equal (multiple-value-list (parse-prefix ":WiZ@tolsun.oulu.fi"))
             '("WiZ" nil "tolsun.oulu.fi")))
  (is (equal (multiple-value-list (parse-prefix ":WiZ"))
             '("WiZ" nil nil))))

;; Also test replies, whose primary use is in parsing
(deftest test-reply->keyword ()
  (is (eq (reply->keyword 1) :RPL_WELCOME))
  (is (eq (reply->keyword 974) :ERR_CANNOTCHANGECHANMODE))
  (is (eq (reply->keyword "PRIVMSG") :PRIVMSG)))

(deftest test-parse-message ()
  (is (equal (multiple-value-list (parse-message "PRIVMSG Supertest"))
             '(nil :PRIVMSG ("Supertest"))))
  (is (equal (multiple-value-list (parse-message "PRIVMSG"))
             '(nil :PRIVMSG ())))
  ;; Note that this also depends on PARSE-PREFIX functioning correctly
  (is (equal (multiple-value-list (parse-message ":test PRIVMSG"))
             '(("test" nil nil) :PRIVMSG ()))))
