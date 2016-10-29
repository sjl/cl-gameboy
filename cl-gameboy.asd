(asdf:defsystem :cl-gameboy
  :name "gameboy"
  :description "Simple Game Boy emulator."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :losh
               ; :trivia
               ; :cl-algebraic-data-type
               :cl-arrows)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:file "gameboy")))
