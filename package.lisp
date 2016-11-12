(defpackage :gameboy
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :gameboy.quickutils)
  (:export)
  (:shadow :bit))


(defpackage :gameboy.gui.utils
  (:use :cl+qt :iterate :losh)
  (:export :initialize-texture))

(defpackage :gameboy.gui.screen
  (:use :cl+qt :iterate :losh
    :gameboy.quickutils
    :gameboy.gui.utils))

(defpackage :gameboy.gui.tile-viewer
  (:use :cl+qt :iterate :losh
    :gameboy.quickutils
    :gameboy.gui.utils))

(defpackage :gameboy.gui
  (:use :cl+qt :iterate :losh
    :gameboy.quickutils
    :gameboy.gui.utils))
