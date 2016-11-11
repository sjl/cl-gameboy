(asdf:defsystem :cl-gameboy
  :name "gameboy"
  :description "Simple Game Boy emulator."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :losh
               :cl-arrows
               :bordeaux-threads
               :qtools
               :qtcore
               :qtgui
               :qtopengl
               :cl-opengl)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:module "gui" :serial t
                              :components ((:file "gui")))
                             (:file "gameboy")))))
