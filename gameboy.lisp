(in-package :gameboy)


(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 1) (safety 3) (debug 3)))


(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))


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
  (ct 0 :type int8))

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


;;;; Utils --------------------------------------------------------------------
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
(defun read-8 (gameboy address)
  ;todo
  )

(defun read-16 (gameboy address)
  ;todo
  )

(defun write-8 (gameboy address value)
  ;todo
  )

(defun write-16 (gameboy address value)
  ;todo
  )


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










