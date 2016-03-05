(defpackage :birch.test/connection
  (:use :cl :birch.test/util :prove)
  (:import-from :birch/connection
                #:make-channel
                #:make-user
                #:users
                #:channels
                #:add-user
                #:remove-user))
(in-package :birch.test/connection)

(diag "User tracking tests")
(plan 1)

(deftest "Users joining and leaving a channel"
  (plan 19)
  (with-test-connection (connection stream)
    (declare (ignore stream))
    ;; A user outside of a channel can not be tracked.
    (isnt (make-user connection "WiZ")
          (make-user connection "WiZ"))
    (let ((channel (make-channel connection "#test"))
          (user (make-user connection "WiZ")))
      ;; A user with whom we're in the same channel can be tracked.
      (add-user connection channel)
      (add-user user channel)
      (is (make-user connection "WiZ")
          user)
      ;; Both users are now in 1 channel, as far as we know.
      (is (length (channels user)) 1)
      (is (length (channels connection)) 1)
      ;; We're now tracking ourselves and USER.
      (is (length (users connection)) 2)

      ;; When the user leaves the channel again, we can no longer track them.
      (remove-user user channel)
      (is (length (users connection)) 1)
      (isnt (make-user connection "WiZ")
            user)
      (is (length (channels user)) 0)
      (is (length (channels connection)) 1)
      ;; This is a tricky one. Because "untracked" users are still in the list
      ;; of users, the new "WiZ" will be in the list of users.
      (is (length (users connection)) 2)

      ;; When a new user joins the channel we can track them fine.
      (let ((user2 (make-user connection "WiZ")))
        (add-user user2 channel)
        (is (make-user connection "WiZ")
            user2)
        (is (length (channels user2)) 1)
        (is (length (channels connection)) 1)
        (is (length (users connection)) 2))

      ;; If we now leave the channel, we no longer have a clue who they might
      ;; be.
      (remove-user connection channel)
      (is (length (users connection)) 1)
      (isnt (make-user connection "WiZ")
            user)
      (is (length (channels user)) 0)
      (is (length (channels connection)) 0)
      ;; Again, the untracked "WiZ" is now in the list of users.
      (is (length (users connection)) 2)
      ;; After we've left the channel, we no longer know who's in it.
      (is (length (users channel)) 0))))
