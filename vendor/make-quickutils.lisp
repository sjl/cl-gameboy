(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :map-product
               :mkstr
               :symb
               :once-only
               :rcurry
               :with-gensyms
               :with-output-to-file

               )
  :package "GAMEBOY.QUICKUTILS")
