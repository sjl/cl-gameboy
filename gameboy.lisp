(in-package :gameboy)


(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 1) (safety 3) (debug 3)))


;;;; Types --------------------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))
(deftype memory-array () '(simple-array int8 (*)))


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
  (bios (make-mem 256) :type memory-array :read-only t)
  (rom (make-mem (k 16)) :type memory-array :read-only t)
  (working-ram (make-mem (k 8)) :type memory-array :read-only t)
  (external-ram (make-mem (k 8)) :type memory-array :read-only t)
  (zero-page-ram (make-mem 128) :type memory-array :read-only t))

(defstruct (gameboy (:conc-name gb-))
  (clock-m 0 :type int8)
  (clock-t 0 :type int8)
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
  (cm 0 :type int8)
  (ct 0 :type int8)
  (mmu (make-mmu) :type mmu))


(define-with-macro (mmu)
  in-bios bios rom working-ram external-ram zero-page-ram)

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f cm ct pc sp clock-m clock-t)


;;;; Bit Fuckery --------------------------------------------------------------
(declaim (inline to-bit chop-8 chop-16))

(defun to-bit (value)
  (if value 1 0))

(defun chop-8 (value)
  (ldb (byte 8 0) value))

(defun chop-16 (value)
  (ldb (byte 16 0) value))


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

(defun increment-clock (gameboy &optional (m 1))
  (with-gameboy (gameboy)
    (setf cm m
          ct (* m 4))))

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
  (dpb (read-8 gameboy (1+ address)) (byte 8 8)
       (read-8 gameboy address)))

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
    (setf a 0 b 0 c 0 d 0 e 0 h 0 l 0 f 0 sp 0 pc 0 clock-m 0 clock-t 0)))


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
      (setf clock-m cm
            clock-t ct))))










