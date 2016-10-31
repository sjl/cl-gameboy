(in-package :gameboy)


(setf *print-length* 24)
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 1) (safety 3) (debug 3)))


;;;; Types & Constants --------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))
(deftype memory-array () '(simple-array int8 (*)))

(defparameter *bios*
  '(#x31 #xFE #xFF #xAF #x21 #xFF #x9F #x32 #xCB #x7C #x20 #xFB #x21 #x26 #xFF #x0E
    #x11 #x3E #x80 #x32 #xE2 #x0C #x3E #xF3 #xE2 #x32 #x3E #x77 #x77 #x3E #xFC #xE0
    #x47 #x11 #x04 #x01 #x21 #x10 #x80 #x1A #xCD #x95 #x00 #xCD #x96 #x00 #x13 #x7B
    #xFE #x34 #x20 #xF3 #x11 #xD8 #x00 #x06 #x08 #x1A #x13 #x22 #x23 #x05 #x20 #xF9
    #x3E #x19 #xEA #x10 #x99 #x21 #x2F #x99 #x0E #x0C #x3D #x28 #x08 #x32 #x0D #x20
    #xF9 #x2E #x0F #x18 #xF3 #x67 #x3E #x64 #x57 #xE0 #x42 #x3E #x91 #xE0 #x40 #x04
    #x1E #x02 #x0E #x0C #xF0 #x44 #xFE #x90 #x20 #xFA #x0D #x20 #xF7 #x1D #x20 #xF2
    #x0E #x13 #x24 #x7C #x1E #x83 #xFE #x62 #x28 #x06 #x1E #xC1 #xFE #x64 #x20 #x06
    #x7B #xE2 #x0C #x3E #x87 #xF2 #xF0 #x42 #x90 #xE0 #x42 #x15 #x20 #xD2 #x05 #x20
    #x4F #x16 #x20 #x18 #xCB #x4F #x06 #x04 #xC5 #xCB #x11 #x17 #xC1 #xCB #x11 #x17
    #x05 #x20 #xF5 #x22 #x23 #x22 #x23 #xC9 #xCE #xED #x66 #x66 #xCC #x0D #x00 #x0B
    #x03 #x73 #x00 #x83 #x00 #x0C #x00 #x0D #x00 #x08 #x11 #x1F #x88 #x89 #x00 #x0E
    #xDC #xCC #x6E #xE6 #xDD #xDD #xD9 #x99 #xBB #xBB #x67 #x63 #x6E #x0E #xEC #xCC
    #xDD #xDC #x99 #x9F #xBB #xB9 #x33 #x3E #x3c #x42 #xB9 #xA5 #xB9 #xA5 #x42 #x4C
    #x21 #x04 #x01 #x11 #xA8 #x00 #x1A #x13 #xBE #x20 #xFE #x23 #x7D #xFE #x34 #x20
    #xF5 #x06 #x19 #x78 #x86 #x23 #x05 #x20 #xFB #x86 #x20 #xFE #x3E #x01 #xE0 #x50))


;;;; Utils --------------------------------------------------------------------
(declaim (inline k))

(defun k (n)
  (* 1024 n))

(defun make-mem (size)
  (make-array size
    :element-type 'int8
    :initial-element 0
    :adjustable nil
    :fill-pointer nil))

(defun make-bios ()
  (make-array 256
    :element-type 'int8
    :initial-contents *bios*
    :adjustable nil
    :fill-pointer nil))

(defmacro case-range (value &rest exprs)
  (once-only (value)
    `(cond
      ,@(iterate
         (for (condition . body) :in exprs)
         (if (eq t condition)
           (collect `(t ,@body))
           (progn (collect condition :into prefixes)
                  (collect `((< ,value (+ ,@(copy-seq prefixes)))
                             ,@body))))))))


;;;; Data ---------------------------------------------------------------------
(defstruct mmu
  (in-bios t :type boolean)
  (bios (make-bios) :type memory-array :read-only t)
  (rom (make-mem (k 16)) :type memory-array :read-only t)
  (working-ram (make-mem (k 8)) :type memory-array :read-only t)
  (external-ram (make-mem (k 8)) :type memory-array :read-only t)
  (zero-page-ram (make-mem 128) :type memory-array :read-only t))

(defstruct (gameboy (:conc-name gb-))
  (clock 0 :type int8)
  (clock-increment 0 :type int8)
  (a 0 :type int8)
  (b 0 :type int8)
  (c 0 :type int8)
  (d 0 :type int8)
  (e 0 :type int8)
  (h 0 :type int8)
  (l 0 :type int8)
  (f 0 :type int8)
  (pc 0 :type int16)
  (sp 0 :type int16)
  (mmu (make-mmu) :type mmu))


(define-with-macro (mmu)
  in-bios bios rom working-ram external-ram zero-page-ram)

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f pc sp clock clock-increment)


(defmethod print-object ((object gameboy) stream)
  (print-unreadable-object (object stream :type t :identity t)))


;;;; Bit Fuckery --------------------------------------------------------------
(declaim (inline to-bit chop-8 chop-16 cat))

(defun to-bit (value)
  (if value 1 0))

(defun chop-8 (value)
  (ldb (byte 8 0) value))

(defun chop-16 (value)
  (ldb (byte 16 0) value))

(defun cat (low-order high-order)
  (dpb high-order (byte 8 8) low-order))


;;;; Flag Register ------------------------------------------------------------
(declaim (inline set-flag))

(defmacro define-flag (name position)
  `(defun ,(symb 'gb-flag- name) (gameboy)
    (ldb (byte 1 ,position)
         (gb-f gameboy))))

(define-flag zero 7)
(define-flag subtract 6)
(define-flag half-carry 5)
(define-flag carry 4)

(defun set-flag (gameboy &key zero subtract half-carry carry)
  (setf (gb-f gameboy)
        (logior 0
                (ash (to-bit zero) 7)
                (ash (to-bit subtract) 6)
                (ash (to-bit half-carry) 5)
                (ash (to-bit carry) 4))))


;;;; More Utils ---------------------------------------------------------------
(declaim (inline increment-clock))

(defun increment-clock (gameboy &optional (machine-cycles 1))
  (with-gameboy (gameboy)
    (setf clock-increment machine-cycles)))

(defmacro with-chopped ((full-symbol truncated-symbol expr) &body body)
  `(let* ((,full-symbol ,expr)
          (,truncated-symbol (chop-8 ,full-symbol)))
    ,@body))


;;;; Memory -------------------------------------------------------------------
(declaim
  (ftype (function (gameboy int16) (values t t)) find-memory read-8 read-16)
  (ftype (function (gameboy int16 int8)) write-8)
  (ftype (function (gameboy int16 int16)) write-16))

(defun find-memory (gameboy address)
  (with-mmu ((gb-mmu gameboy))
    (case-range address
      ;; BIOS/ROM
      (256 (values (if in-bios bios rom) address))
      ((- (k 32) 256) (values rom address))

      ;; VRAM/ERAM/WRAM
      ((k 8) (values nil nil)) ; todo: vram
      ((k 8) (values external-ram (ldb (byte 13 0) address)))
      ((k 8) (values working-ram (ldb (byte 13 0) address)))
      ((- (k 8) 512) ; Shadow WRAM
       (values working-ram (ldb (byte 13 0) address)))

      ;; OAM
      (160 (values nil nil)) ; todo: oam
      ((- 256 160) (values nil nil)) ; todo: constant 0

      ;; I/O
      (128 (values nil nil)) ; todo: i/o

      ;; Zero Page
      (128 (values zero-page-ram (ldb (byte 7 0) address)))

      ;; Wat
      (t (error "Bad memory address: ~X" address)))))

(defun read-8 (gameboy address)
  (multiple-value-bind (array address) (find-memory gameboy address)
    (if array
      (aref array address)
      0)))

(defun read-16 (gameboy address)
  (cat (read-8 gameboy address)
       (read-8 gameboy (1+ address))))

(defun write-8 (gameboy address value)
  (multiple-value-bind (array address) (find-memory gameboy address)
    (if array
      (setf (aref array address) value)
      (error "Cannot write to memory address ~X" address))))

(defun write-16 (gameboy address value)
  (write-8 gameboy address (ldb (byte 8 0) value))
  (write-8 gameboy (1+ address) (ldb (byte 8 8) value)))


;;;; ROMs ---------------------------------------------------------------------
(defun load-rom (gameboy filename)
  (replace (-> gameboy gb-mmu mmu-rom)
           (read-file-into-byte-vector filename)))

(defun clear-rom (gameboy)
  (fill (-> gameboy gb-mmu mmu-rom) 0))


;;;; Opcodes ------------------------------------------------------------------
(defmacro define-opcode (name &rest body)
  `(defun ,name (gameboy)
    (with-gameboy (gameboy)
      ,@body)))


;;; Load & Store
(defmacro macro-map ((lambda-list items) &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))

(macro-map (register (b c d e h l))                         ; LD r, i
  `(define-opcode ,(symb 'ld-r/ register '<i)
    (setf ,register (read-8 gameboy pc))
    (incf pc)
    (increment-clock gameboy 2)))

(macro-map ((destination source)                            ; LD r, r
            #.(map-product #'list
                           '(a b c d e h l)
                           '(a b c d e h l)))
  `(define-opcode ,(symb 'ld-r/ destination '<r/ source)
    (setf ,destination ,source)
    (increment-clock gameboy)))

(macro-map (register (a b c d e h l))                       ; LD r, (HL)
  `(define-opcode ,(symb 'ld-r/ register '<mem/hl)
    (setf ,register (read-8 gameboy (cat l h)))
    (increment-clock gameboy 2)))

(macro-map (register (a b c d e h l))                       ; LD (HL), r
  `(define-opcode ,(symb 'ld-mem/hl<r/ register)
    (write-8 gameboy (cat l h) ,register)
    (increment-clock gameboy 2)))

(define-opcode ld-mem/hl<i                                  ; LD (HL), i
  (write-8 gameboy (cat l h) (read-8 gameboy pc))
  (incf pc)
  (increment-clock gameboy 3))

(define-opcode ld-mem/bc<r/a                                ; LD (BC), A
  (write-8 gameboy (cat c b) a)
  (increment-clock gameboy 2))

(define-opcode ld-mem/de<r/a                                ; LD (DE), A
  (write-8 gameboy (cat e d) a)
  (increment-clock gameboy 2))

(define-opcode ld-mem/i<a                                   ; LD (i), A
  (write-8 gameboy (read-16 gameboy pc) a)
  (incf pc 2)
  (increment-clock gameboy 4))

(define-opcode ld-r/a<mem/bc                                ; LD A, (BC)
  (setf a (read-8 gameboy (cat c b)))
  (increment-clock gameboy 2))

(define-opcode ld-r/a<mem/de                                ; LD A, (DE)
  (setf a (read-8 gameboy (cat e d)))
  (increment-clock gameboy 2))

(define-opcode ld-mem/i<a                                   ; LD A, (i)
  (setf a (read-8 gameboy (read-16 gameboy pc)))
  (incf pc 2)
  (increment-clock gameboy 4))

(macro-map ((hi lo)                                         ; LD BC/DE/HL, i
            ((b c) (d e) (h l)))
  `(define-opcode ,(symb 'ld-r/ (symb hi lo) '<i)
    (setf ,lo (read-8 gameboy pc))
    (incf pc)
    (setf ,hi (read-8 gameboy pc))
    (incf pc)
    (increment-clock gameboy 3)))

(define-opcode ld-r/sp<i                                    ; LD SP, i
  (setf sp (read-16 gameboy pc))
  (incf pc 2)
  (increment-clock gameboy 3))



(define-opcode op-add-e
  (with-chopped (full trunc (+ a e))
    (set-flag gameboy
              :zero (zerop trunc)
              :half-carry nil ; todo
              :carry (> full 255))
    (setf a trunc))
  (increment-clock gameboy))

(define-opcode op-cp-b
  (with-chopped (full trunc (- a b))
    (set-flag gameboy
              :zero (zerop trunc)
              :subtract t
              :half-carry nil ; todo
              :carry (minusp full)))
  (increment-clock gameboy))

(define-opcode op-push-bc
  (write-8 gameboy (decf sp) b)
  (write-8 gameboy (decf sp) c)
  (increment-clock gameboy 3))

(define-opcode op-pop-hl
  (setf l (read-8 gameboy sp))
  (incf sp)
  (setf h (read-8 gameboy sp))
  (incf sp)
  (increment-clock gameboy 3))

(define-opcode ld-a-mem
  (setf a (read-8 gameboy pc))
  (incf pc 2)
  (increment-clock gameboy 4))

(define-opcode op-nop
  (increment-clock gameboy))


;;;; VM -----------------------------------------------------------------------
(defun reset (gameboy)
  (with-gameboy (gameboy)
    (setf a 0 b 0 c 0 d 0 e 0 h 0 l 0 f 0 sp 0 pc 0 clock 0)))


(defparameter *running* t)

(defparameter *opcode-list*
  '(#'op-nop
    #'op-add-e
    ; ...
    ))

(defparameter *opcodes*
  (make-array (length *opcode-list*)
    :initial-contents *opcode-list*
    :adjustable nil
    :fill-pointer nil))


(defun run (gameboy)
  (with-gameboy (gameboy)
    (iterate
      (while *running*)
      (for op = (aref *opcodes* (read-8 gameboy pc)))
      (incf pc)
      (funcall op gameboy)
      (zapf pc (chop-16 %))
      (incf clock clock-increment))))










