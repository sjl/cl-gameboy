(in-package :gameboy)


(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 1) (safety 3) (debug 3)))


(deftype word () '(unsigned-byte 8))
(deftype addr () '(unsigned-byte 16))


(defstruct (gameboy (:conc-name gb-))
  (clock-m 0 :type word)
  (clock-t 0 :type word)
  (a 0 :type word)
  (b 0 :type word)
  (c 0 :type word)
  (d 0 :type word)
  (e 0 :type word)
  (h 0 :type word)
  (l 0 :type word)
  (f 0 :type word)
  (pc 0 :type addr)
  (sp 0 :type addr)
  (cm 0 :type word)
  (ct 0 :type word))

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f cm ct pc sp)


;;;; Bit Fuckery --------------------------------------------------------------
(declaim (inline to-bit chop))
(defun to-bit (value)
  (if value 1 0))

(defun chop (value)
  (ldb (byte 8 0) value))


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

(defun increment-clock (gameboy &optional (m-incr 1) (t-incr 4))
  (with-gameboy (gameboy)
    (setf cm m-incr
          ct t-incr)))

(defmacro with-chopped ((full-symbol truncated-symbol expr) &body body)
  `(let* ((,full-symbol ,expr)
          (,truncated-symbol (chop ,full-symbol)))
    ,@body))


;;;; Opcodes ------------------------------------------------------------------
(defun op-add-e (gameboy)
  (with-gameboy (gameboy)
    (with-chopped (full trunc (+ a e))
      (set-flag gameboy
                :zero (zerop trunc)
                :half-carry nil ; todo
                :carry (> full 255))
      (setf a trunc)))
  (increment-clock gameboy))

(defun op-cp-b (gameboy)
  (with-gameboy (gameboy)
    (with-chopped (full trunc (- a b))
      (set-flag gameboy
                :zero (zerop trunc)
                :subtract t
                :half-carry nil ; todo
                :carry (minusp full))))
  (increment-clock gameboy))

(defun op-nop (gameboy)
  (increment-clock gameboy))

