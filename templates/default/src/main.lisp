(uiop:define-package :rename-me/main
  (:use :cl :iterate)
  (:export #:main) 
  (:local-nicknames (#:a :alexandria))
  (:nicknames :rename-me))
(cl:in-package :rename-me/main)

(defun main ()
  (format t "Hello, world!~%"))
