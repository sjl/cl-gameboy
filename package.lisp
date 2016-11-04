(defpackage :gameboy
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :gameboy.quickutils)
  (:export)
  (:shadow :bit)
  )

(defpackage :gameboy.gui
  (:use :cl+qt :iterate :losh))
