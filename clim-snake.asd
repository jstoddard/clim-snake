;;;; clim-snake.asd

(asdf:defsystem #:clim-snake
  :description "Snake game for CLIM"
  :author "Jeremiah Stoddard <jeremiah@jeremiahstoddard.com>"
  :license "MIT License"
  :depends-on (#:mcclim)
  :serial t
  :components ((:file "package")
               (:file "clim-snake")))

