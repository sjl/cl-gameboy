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
               :once-only
               :rcurry
               :read-file-into-byte-vector
               :symb
               :with-gensyms
               :write-byte-vector-into-file

               )
  :package "GAMEBOY.QUICKUTILS")
