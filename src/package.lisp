(uiop:define-package :birch
    (:use
     :cl
     :birch/replies
     :birch/parse
     :birch/ctcp
     :birch/connection
     :birch/commands
     :birch/events
     :birch/init)
  (:reexport
   :birch/replies
   :birch/parse
   :birch/ctcp
   :birch/connection
   :birch/commands
   :birch/events
   :birch/init))

