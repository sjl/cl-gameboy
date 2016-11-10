(in-package :gameboy)


(setf *print-length* 10)
(declaim (optimize (speed 1) (safety 3) (debug 3)))
; (declaim (optimize (speed 3) (safety 0) (debug 3)))
; (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun :i () SB-EXT:*INSPECTED*)


;;;; Types & Constants --------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))
(deftype sint8 () '(signed-byte 8))
(deftype sint16 () '(signed-byte 16))
(deftype memory-array () '(simple-array int8 (*)))
(deftype bank-array () '(simple-vector *))
(deftype bit () 'cl:bit)
(deftype opcode-function () '(function (gameboy) (values gameboy &optional)))
(deftype color-index () '(integer 0 3))

(deftype sprite-cache ()
  `(simple-array ,(upgraded-array-element-type 'sprite) (40)))

(deftype tile-cache ()
  '(simple-array (integer 0 3) #.(list (* 3 128) 8 8)))

(deftype gpu-screen-data ()
  `(array ,(upgraded-array-element-type 'color-index) (160 144)))


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
(declaim (inline k nonzerop))

(defun k (n)
  (* 1024 n))

(defun make-simple-array (size &rest args)
  (apply #'make-array size
         :adjustable nil
         :fill-pointer nil
         args))

(defun make-mem (size)
  (make-simple-array size
                     :element-type 'int8
                     :initial-element 0))

(defun make-bios ()
  (make-simple-array 256
                     :element-type 'int8
                     :initial-contents *bios*))


(defmacro macro-map ((lambda-list items) &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))


(defun nonzerop (i)
  (not (zerop i)))


(defmacro case-bit (expr &body clauses)
  (once-only (expr)
    `(cond
      ,@(iterate (for (bit . body) :in clauses)
         (collect (if (eq bit t)
                    `(t ,@body)
                    `((logbitp ,bit ,expr) ,@body)))))))

(defmacro n-of (n expr)
  `(iterate (repeat ,n)
            (collect ,expr)))


;;;; Bit Fuckery --------------------------------------------------------------
(declaim (inline to-bit set-bit bit flip
                 low-nibble high-nibble
                 low-byte high-byte
                 cat
                 swap-nibbles
                 chop-4 chop-8 chop-12 chop-16
                 unsigned-to-signed-8
                 +_8 -_8
                 +_16 -_16
                 rot))

(declaim (ftype (function ((integer 0 (16)) int16 bit) int16)
                set-bit)
         (ftype (function ((integer 0 (16)) int16) bit)
                bit)
         (ftype (function (bit) bit) flip))


(defun to-bit (value)
  (ecase value
    ((0 nil) 0)
    ((1 t) 1)))

(defun set-bit (position integer value)
  (dpb value (byte 1 position) integer))

(defun bit (position integer)
  (ldb (byte 1 position) integer))

(defun flip (bit)
  (if (zerop bit) 1 0))


(defun chop-4 (value)
  (ldb (byte 4 0) value))

(defun chop-8 (value)
  (ldb (byte 8 0) value))

(defun chop-12 (value)
  (ldb (byte 12 0) value))

(defun chop-16 (value)
  (ldb (byte 16 0) value))


(defun low-nibble (byte)
  (ldb (byte 4 0) byte))

(defun high-nibble (byte)
  (ldb (byte 4 4) byte))

(defun low-byte (word)
  (ldb (byte 8 0) word))

(defun high-byte (word)
  (ldb (byte 8 8) word))


(defun cat (low-order high-order &optional
            (low-width 8)
            (high-width low-width))
  (dpb high-order (byte high-width low-width) low-order))


(defmacro incf-8 (place &optional (amount 1))
  `(zapf ,place (chop-8 (+ % ,amount))))

(defmacro incf-16 (place &optional (amount 1))
  `(zapf ,place (chop-16 (+ % ,amount))))

(defmacro decf-8 (place &optional (amount 1))
  `(zapf ,place (chop-8 (- % ,amount))))

(defmacro decf-16 (place &optional (amount 1))
  `(zapf ,place (chop-16 (- % ,amount))))


(defun unsigned-to-signed-8 (i)
  (if (logbitp 7 i)
    (- (chop-8 (1+ (lognot i))))
    i))


(defmacro with-chopped-8 ((full-symbol truncated-symbol expr) &body body)
  `(let* ((,full-symbol ,expr)
          (,truncated-symbol (chop-8 ,full-symbol)))
    ,@body))

(defmacro with-chopped-16 ((full-symbol truncated-symbol expr) &body body)
  `(let* ((,full-symbol ,expr)
          (,truncated-symbol (chop-16 ,full-symbol)))
    ,@body))


(declaim (ftype (function (int8) int8) swap-nibbles))

(defun swap-nibbles (byte)
  (cat (ldb (byte 4 4) byte)
       (ldb (byte 4 0) byte)
       4))


(declaim (ftype (function (int8 int8 &optional bit)
                          (values int8 boolean boolean boolean boolean &optional))
                +_8 -_8)
         (ftype (function (int16 int16 &optional bit)
                          (values int16 boolean boolean boolean boolean &optional))
                +_16 -_16))

(defun +_8 (x y &optional (c 0))
  (with-chopped-8 (full trunc (+ x y c))
    (values trunc
            (zerop trunc)
            nil
            (> (+ (chop-4 x) (chop-4 y) c) #xF)
            (> full #xFF))))

(defun +_16 (x y &optional (c 0))
  (with-chopped-16 (full trunc (+ x y c))
    (values trunc
            (zerop trunc)
            nil
            (> (+ (chop-12 x) (chop-12 y) c) #xFFF)
            (> full #xFFFF))))

(defun -_8 (x y &optional (c 0))
  (with-chopped-8 (full trunc (- x y c))
    (values trunc
            (zerop trunc)
            t
            (< (chop-4 x) (+ c (chop-4 y)))
            (minusp full))))

(defun -_16 (x y &optional (c 0))
  (with-chopped-16 (full trunc (- x y c))
    (values trunc
            (zerop trunc)
            t
            (< (chop-12 x) (+ c (chop-12 y)))
            (minusp full))))


(defun rot (width byte &optional (n 1))
  (cond ((plusp n)
         (logior (ldb (byte width 0) (ash byte n))
                 (ldb (byte n (- width n)) byte)))
        ((minusp n)
         (let ((n (- n)))
           (cat (ash byte (- n))
                (ldb (byte n 0) byte)
                (- width n)
                n)))
        (t byte)))


;;;; Data ---------------------------------------------------------------------
(defstruct sprite
  (raw (make-simple-array 4 :element-type 'int8)
       :type (simple-array int8 (4))
       :read-only t)
  (x -8 :type sint8)
  (y -16 :type sint8)
  (tile 0 :type int8)
  (palette 0 :type bit)
  (priority nil :type boolean)
  (x-flip nil :type boolean)
  (y-flip nil :type boolean))

(defstruct mmu
  (in-bios t :type boolean)
  (bios (make-bios) :type memory-array :read-only t)
  (rom (make-mem (k 16)) :type memory-array :read-only t)
  (banks (make-simple-array 1 :initial-contents (list (make-mem (k 16))))
         :type bank-array :read-only t)
  (working-ram (make-mem (k 8)) :type memory-array :read-only t)
  (external-ram (make-mem (k 8)) :type memory-array :read-only t)
  (zero-page-ram (make-mem 128) :type memory-array :read-only t))

(defstruct gpu
  (gui (gameboy.gui::make-qt-gui))
  (screen-data (make-array '(160 144)
                 :element-type 'color-index
                 :fill-pointer nil
                 :adjustable nil)
               :type gpu-screen-data
               :read-only t)
  (vram (make-mem (k 8)) :type memory-array :read-only t)
  (io (make-mem 256) :type memory-array :read-only t)
  (tile-cache (make-tile-cache) :type tile-cache :read-only t)
  (sprite-cache (make-sprite-cache) :type sprite-cache :read-only t)
  (mode 2 :type (integer 0 3))
  (clock 0 :type fixnum) ; fuck it, close enough
  (line 0 :type (integer 0 154))
  (line-compare 0 :type int8)
  (scroll-x 0 :type int8)
  (scroll-y 0 :type int8)
  (palette-background #xFF :type int8)
  (palette-object-1 #xFF :type int8)
  (palette-object-2 #xFF :type int8)
  (control 0 :type int8))

(defstruct (gameboy (:conc-name gb-))
  (tick 0 :type fixnum)
  (clock 0 :type fixnum)
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
  (interrupt-enable 0 :type int8)
  (interrupt-flags 0 :type int8)
  (interrupt-master nil :type boolean)
  (key-column nil)
  (mmu (make-mmu) :type mmu)
  (gpu (make-gpu) :type gpu)
  (halt 0 :type int8))


;;; Define accessors for the AF/BC/DE/HL pseudo-registers
(macro-map ((hi lo)
            ((a f) (b c) (d e) (h l)))
  (let ((name (symb 'gb- hi lo))
        (acc-lo (symb 'gb- lo))
        (acc-hi (symb 'gb- hi)))
    `(progn
      (declaim (inline ,name (setf ,name))
               (ftype (function (gameboy) int16) ,name)
               (ftype (function (int16 gameboy) int16) (setf ,name)))

      (defun ,name (gameboy)
        ; todo fix the flag register here to always read 0 in low-order bits
        (cat (,acc-lo gameboy) (,acc-hi gameboy)))

      (defun (setf ,name) (value gameboy)
        (setf (,acc-lo gameboy) (ldb (byte 8 0) value)
              (,acc-hi gameboy) (ldb (byte 8 8) value))
        value))))


(define-with-macro (sprite)
  x y tile palette priority x-flip y-flip raw)

(define-with-macro (mmu)
  in-bios bios rom banks working-ram external-ram zero-page-ram)

(define-with-macro (gpu)
  gui vram tile-cache sprite-cache screen-data
  mode clock line line-compare
  scroll-x scroll-y
  palette-background palette-object-1 palette-object-2
  control
  stat
  flag-background
  flag-sprites
  flag-sprites-size
  flag-background-tile-map
  flag-background-tile-set
  flag-window
  flag-window-tile-map
  flag-display)

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f pc sp
  interrupt-master interrupt-enable interrupt-flags
  key-column
  tick clock clock-increment af bc de hl halt
  flag-zero flag-subtract flag-half-carry flag-carry)


(defmethod print-object ((object gameboy) stream)
  (print-unreadable-object (object stream :type t :identity t)))


;;;; Flag Registers -----------------------------------------------------------
(declaim (inline set-flag
                 interrupt-bit
                 interrupt-enabled-p
                 interrupt-fired-p
                 interrupt-ready-p
                 mark-interrupt
                 clear-interrupt))

(defmacro define-flag (type prefix register position name)
  (let ((full-name (symb prefix '-flag- name))
        (place `(,(symb prefix '- register) ,type)))
    `(progn
      (declaim
        (inline ,full-name (setf ,full-name))
        (ftype (function (,type) (values bit &optional)) ,full-name)
        (ftype (function (bit ,type) (values bit &optional)) (setf ,full-name)))

      (defun ,full-name (,type)
        (bit ,position ,place))

      (defun (setf ,full-name) (value ,type)
        (setf (ldb (byte 1 ,position) ,place) value)))))

(macro-map ; CPU Flag Register
  ((bit name)
   ((7 zero)
    (6 subtract)
    (5 half-carry)
    (4 carry)))
  `(define-flag gameboy gb f ,bit ,name))

(macro-map ; GPU Control Flags
  ((bit name)
   ((0 background)
    (1 sprites)
    (2 sprites-size)
    (3 background-tile-map)
    (4 background-tile-set)
    (5 window)
    (6 window-tile-map)
    (7 display)))
  `(define-flag gpu gpu control ,bit ,name))


(declaim (ftype (function
                  (gameboy &key (:zero t) (:subtract t) (:half-carry t) (:carry t))
                  int8)
                set-flag))

(defun set-flag (gameboy &key
                 (zero :preserve)
                 (subtract :preserve)
                 (half-carry :preserve)
                 (carry :preserve))
  (macrolet ((sf (flag)
               `(when (not (eq :preserve ,flag))
                 (setf (,(symb 'gb-flag- flag) gameboy)
                       (to-bit ,flag)))))
    (sf zero)
    (sf subtract)
    (sf half-carry)
    (sf carry))
  (gb-f gameboy))


(defun interrupt-bit (interrupt)
  (ecase interrupt
    (:vblank 0)
    (:lcd-stat 1)
    (:timer 2)
    (:serial 3)
    (:joypad 4)))

(defun interrupt-enabled-p (gameboy interrupt)
  (bit (interrupt-bit interrupt)
       (gb-interrupt-enable gameboy)))

(defun interrupt-fired-p (gameboy interrupt)
  (= 1 (bit (interrupt-bit interrupt)
            (gb-interrupt-enable gameboy))))

(defun interrupt-ready-p (gameboy interrupt)
  (and (interrupt-enabled-p gameboy interrupt)
       (interrupt-fired-p gameboy interrupt)))

(defun mark-interrupt (gameboy interrupt)
  (setf (ldb (byte 1 (interrupt-bit interrupt))
             (gb-interrupt-flags gameboy))
        1))

(defun clear-interrupt (gameboy interrupt)
  (setf (ldb (byte 1 (interrupt-bit interrupt))
             (gb-interrupt-flags gameboy))
        0))


;;;; More Utils ---------------------------------------------------------------
(declaim (inline increment-clock increment-pc
                 stack-push stack-pop))


(defun increment-clock (gameboy &optional (machine-cycles 1))
  (with-gameboy (gameboy)
    (incf clock-increment machine-cycles)))

(defun increment-pc (gameboy &optional (increment 1))
  (incf-16 (gb-pc gameboy) increment))


(defun stack-push (gameboy word)
  (with-gameboy (gameboy)
    (decf sp 2)
    (setf (mem-16 gameboy sp) word)))

(defun stack-pop (gameboy)
  (with-gameboy (gameboy)
    (prog1 (mem-16 gameboy sp)
      (incf sp 2))))


;;;; Memory -------------------------------------------------------------------
(declaim
  (ftype (function (gameboy int16) int8) mem-8)
  (ftype (function (gameboy int16) int16) mem-16)
  (ftype (function (int8 gameboy int16)) (setf mem-8))
  (ftype (function (int16 gameboy int16)) (setf mem-16))
  (ftype (function (gameboy) int8) read-keys)
  (ftype (function (gameboy int8) null) write-keys))


(defun read-keys (gameboy)
  (with-gameboy (gameboy)
    (let ((gui (gpu-gui (gb-gpu gameboy))))
      (case key-column
        (0 #x00)
        (1 (-<> #b11010000
             (set-bit 0 <> (to-bit (not (gameboy.gui::qt-gui-key-a gui))))
             (set-bit 1 <> (to-bit (not (gameboy.gui::qt-gui-key-b gui))))
             (set-bit 2 <> (to-bit (not (gameboy.gui::qt-gui-key-select gui))))
             (set-bit 3 <> (to-bit (not (gameboy.gui::qt-gui-key-start gui))))))
        (2 (-<> #b11100000
             (set-bit 0 <> (to-bit (not (gameboy.gui::qt-gui-key-right gui))))
             (set-bit 1 <> (to-bit (not (gameboy.gui::qt-gui-key-left gui))))
             (set-bit 2 <> (to-bit (not (gameboy.gui::qt-gui-key-up gui))))
             (set-bit 3 <> (to-bit (not (gameboy.gui::qt-gui-key-down gui))))))
        (t #xff)))))

(defun write-keys (gameboy value)
  (with-gameboy (gameboy)
    (setf key-column
          (case (logand value #b00110000)
            (#b00000000 0)
            (#b00010000 1)
            (#b00100000 2)
            (#b00110000 3))))
  nil)


(defun mem-8 (gameboy address)
  (with-mmu ((gb-mmu gameboy))
    (cond
      ;; BIOS/ROM
      ((< address 256) (aref (if in-bios bios rom) address))
      ((= address 256) (setf in-bios nil) (aref rom address))
      ((< address #x4000) (aref rom address))

      ;; ROM Banks
      ;; todo bank switching
      ((< address #x8000) (-<> banks
                            (aref <> 0)
                            (aref <> (- address #x4000))))

      ;; VRAM/ERAM/WRAM/Shadow WRAM
      ((< address #xA000) (aref (gpu-vram (gb-gpu gameboy))
                                (- address #x8000)))
      ((< address #xC000) (aref external-ram (- address #xA000)))
      ((< address #xE000) (aref working-ram (- address #xC000)))
      ((< address #xFE00) (aref working-ram (- address #xE000)))

      ;; OAM
      ((< address #xFEA0) (read-sprite-data gameboy (- address #xFE00)))
      ((< address #xFF00) (error "Read OAM unimplemented ~4,'0X" address))

      ;; I/O
      ;; Joypad
      ((= address #xFF00) (read-keys gameboy))

      ;; Interrupt Flags
      ((= address #xFF0F) (gb-interrupt-flags gameboy))

      ((<= #xFF10 address #xFF3F) 0) ; todo: sound
      ((= address #xFF40) (gpu-control (gb-gpu gameboy)))
      ((= address #xFF41) (gpu-stat (gb-gpu gameboy)))
      ((= address #xFF42) (gpu-scroll-y (gb-gpu gameboy)))
      ((= address #xFF43) (gpu-scroll-x (gb-gpu gameboy)))
      ((= address #xFF44) (gpu-line (gb-gpu gameboy)))
      ((= address #xFF45) (gpu-line-compare (gb-gpu gameboy)))
      ((= address #xFF47) (gpu-palette-background (gb-gpu gameboy)))
      ((= address #xFF48) (gpu-palette-object-1 (gb-gpu gameboy)))
      ((= address #xFF49) (gpu-palette-object-2 (gb-gpu gameboy)))
      ((or (= address #xFF50)) ; todo ???
       (aref (gpu-io (gb-gpu gameboy)) (- address #xFF00)))
      ((< address #xFF80) (error "Read I/O unimplemented ~4,'0X" address))

      ;; Zero Page
      ((<= address #xFFFF) (aref zero-page-ram (- address #xFF80)))

      ;; Interrupt-Enable
      ((= address #xFFFF) (gb-interrupt-enable gameboy))

      ;; Wat
      (t (error "Bad memory address: ~X" address)))))

(defun (setf mem-8) (value gameboy address)
  (with-mmu ((gb-mmu gameboy))
    (cond
      ;; BIOS/ROM
      ((< address 256) (setf (aref (if in-bios bios rom) address) value))
      ((< address #x4000) (setf (aref rom address) value))

      ;; ROM Banks
      ;; todo bank switching
      ((< address #x8000) (-<> banks
                            (aref <> 0)
                            (aref <> (- address #x4000))
                            (setf <> value)))

      ;; VRAM/ERAM/WRAM/Shadow WRAM
      ((< address #xA000)
       (progn
         (setf (aref (gpu-vram (gb-gpu gameboy))
                     (- address #x8000))
               value)
         (when (<= #x8000 address (1- #x9800))
           (update-tile-memory gameboy address))))
      ((< address #xC000) (setf (aref external-ram (- address #xA000)) value))
      ((< address #xE000) (setf (aref working-ram (- address #xC000)) value))
      ((< address #xFE00) (setf (aref working-ram (- address #xE000)) value))

      ;; OAM
      ((< address #xFEA0) (write-sprite-data gameboy (- address #xFE00) value))
      ((< address #xFF00) (error "Write OAM unimplemented ~4,'0X" address))

      ;; I/O
      ;; Joypad
      ((= address #xFF00) (write-keys gameboy value))

      ;; Interrupt Flags
      ((= address #xFF0F) (setf (gb-interrupt-flags gameboy) value))

      ;; I/O
      ((<= #xFF10 address #xFF3F) 0) ; todo: sound
      ((= address #xFF40) (setf (gpu-control (gb-gpu gameboy)) value))
      ((= address #xFF42) (setf (gpu-scroll-y (gb-gpu gameboy)) value))
      ((= address #xFF43) (setf (gpu-scroll-x (gb-gpu gameboy)) value))
      ;; writing here always reset the scanline counter, gameboys are weird
      ((= address #xFF44) (setf (gpu-line (gb-gpu gameboy)) 0))
      ((= address #xFF45) (setf (gpu-line-compare (gb-gpu gameboy)) value))
      ((= address #xFF47) (setf (gpu-palette-background (gb-gpu gameboy)) value))
      ((= address #xFF48) (setf (gpu-palette-object-1 (gb-gpu gameboy)) value))
      ((= address #xFF49) (setf (gpu-palette-object-2 (gb-gpu gameboy)) value))
      ((= address #xFF50) (setf (aref (gpu-io (gb-gpu gameboy)) ; todo
                                      (- address #xFF00))
                                value))
      ((or (= address #xFF41) ; todo STAT??
           (= address #xFF50)) ; todo ???
       (setf (aref (gpu-io (gb-gpu gameboy)) (- address #xFF00))
             value))
      ((< address #xFF80) (error "Write I/O unimplemented ~4,'0X" address))

      ;; Zero Page
      ((< address #xFFFF) (setf (aref zero-page-ram (- address #xFF80)) value))

      ;; Interrupt-Enable
      ((= address #xFFFF) (setf (gb-interrupt-enable gameboy) value))

      ;; Wat
      (t (error "Cannot write to memory address: ~X" address)))))


(defun mem-16 (gameboy address)
  (cat (mem-8 gameboy address)
       (mem-8 gameboy (1+ address))))

(defun (setf mem-16) (value gameboy address)
  (setf (mem-8 gameboy address) (ldb (byte 8 0) value)
        (mem-8 gameboy (1+ address)) (ldb (byte 8 8) value)))


;;;; ROMs ---------------------------------------------------------------------
(defun load-rom (gameboy filename)
  (replace (-> gameboy gb-mmu mmu-rom)
           (read-file-into-byte-vector filename)))

(defun clear-rom (gameboy)
  (fill (-> gameboy gb-mmu mmu-rom) 0))


;;;; Graphics -----------------------------------------------------------------
(declaim (inline apply-palette)
         (ftype (function (int8 color-index) color-index) apply-palette))


(defun apply-palette (palette data)
  ;; Palette data is stored in a byte like so:
  ;;
  ;;   33 22 11 00 color data
  ;;   76 54 32 10 bit index
  ;;
  ;; e.g. when the data requests color 3, whatever is in the two high-order bits
  ;; is the actual color that gets rendered.
  (ldb (byte 2 (* 2 data)) palette))


(defun make-sprite-cache ()
  (make-simple-array 40
    :element-type 'sprite
    :initial-contents (n-of 40 (make-sprite))))

(defun update-sprite (sprite)
  (with-sprite (sprite)
    (setf y (aref raw 0)
          x (aref raw 1)
          tile (aref raw 2)
          palette (bit 4 (aref raw 3))
          x-flip (nonzerop (bit 5 (aref raw 3)))
          y-flip (nonzerop (bit 6 (aref raw 3)))
          priority (nonzerop (bit 7 (aref raw 3))))))

(defun read-sprite-data (gameboy sprite-address)
  (with-gpu ((gb-gpu gameboy))
    (let ((sprite (aref sprite-cache (floor sprite-address 4))))
      (aref (sprite-raw sprite) (mod sprite-address 4)))))

(defun write-sprite-data (gameboy sprite-address value)
  (with-gpu ((gb-gpu gameboy))
    (let ((sprite (aref sprite-cache (floor sprite-address 4))))
      (setf (aref (sprite-raw sprite) (mod sprite-address 4)) value)
      (update-sprite sprite))))


(defun tile-pixel (tile-cache tile-id row col)
  (aref tile-cache tile-id row col))

(defun (setf tile-pixel) (value tile-cache tile-id row col)
  (setf (aref tile-cache tile-id row col) value))

(defun make-tile-cache ()
  (make-simple-array (list (* 3 128) 8 8)
    :element-type '(integer 0 3)))

(defun update-tile-cache (tile-cache tile-id row low-byte high-byte)
  (iterate
    (for col :from 0 :to 7)
    (for bit :from 7 :downto 0)
    (setf (tile-pixel tile-cache tile-id row col)
          (cat (bit bit low-byte)
               (bit bit high-byte)
               1))))

(defun update-tile-memory (gameboy address)
  ;; Tile data is held contiguously in a hunk of VRAM.
  ;;
  ;; Each row of a tile is described by two bytes, and each tile has eight rows
  ;; of pixels, so each tile contains a total of sixteen bytes.
  ;;
  ;; two bytes per row
  ;;  ||   ||
  ;; vvvv vvvv
  ;; t0₀h t0₀l t0₁h t0₁l t0₂h t0₂l ...  <== sixteen total bytes per tile
  ;; t1₀h t1₀l t1₁h t1₁l t1₂h t1₂l ...
  (let* ((offset (- address #x8000))
         (tile-id (floor offset (* 8 2)))
         (tile-row (mod (floor offset 2) 8)))
    (with-gpu ((gb-gpu gameboy))
      (update-tile-cache tile-cache tile-id tile-row
                         (aref vram (set-bit 0 offset 1))
                         (aref vram (set-bit 0 offset 0))))))


(defun dump-tile (gameboy id)
  (iterate (for row :from 0 :below 8)
           (iterate (for col :from 0 :below 8)
                    (prin1 (tile-pixel (gpu-tile-cache (gb-gpu gameboy))
                                       id row col)))
           (terpri))
  (finish-output))

(defun gpu-stat (gpu)
  ;; The STAT register is special.  It looks like this:
  ;;
  ;; .....CMM
  ;;
  ;; C is set if the line and line-comparison registers (LY and LYC) are equal.
  ;; MM is set to the current GPU mode.
  ;;
  ;; todo: fill in the rest
  (with-gpu (gpu)
    (cat mode (to-bit (= line line-compare))
         2 1)))


(declaim (inline get-pixel render-pixel)
         (ftype (function (gpu (integer 0 (160)) (integer 0 (144)) color-index))
                render-pixel)
         (ftype (function (gpu (integer 0 (160)) (integer 0 (144))) color-index)
                get-pixel)
         (ftype (function (gameboy)) clear-scanline))

(defun render-pixel (gpu x y final-color)
  (setf (aref (gpu-screen-data gpu) x y) final-color)
  (gameboy.gui::blit-pixel (gpu-gui gpu) x y final-color))

(defun get-pixel (gpu x y)
  (aref (gpu-screen-data gpu) x y))


(defun clear-scanline (gameboy)
  (let ((gpu (gb-gpu gameboy)))
    (with-gpu (gpu)
      (iterate
        (with y = line)
        (for x :from 0 :below 160)
        (render-pixel gpu x y 0)))))


(defun render-background-scanline (gameboy)
  (with-gpu ((gb-gpu gameboy))
    (let ((background-map-offset
            ;; The background map is stored in a hunk of VRAM.  Each element
            ;; is a single byte (the tile ID).
            ;;
            ;; 0,0 0,1 0,2 0,3 0,4 ... 0,32
            ;; 1,0 1,1 1,2 1,3 1,4 ... 1,32
            (+ (if (zerop flag-background-tile-map)
                 #x1800
                 #x1c00)
               (-<> scroll-y  ; start at the scroll offset
                 (+ line <>)  ; add the current line
                 (chop-8 <>)  ;         with an 8-bit add
                 (floor <> 8) ; each tile is 8 rows tall
                 (* 32 <>)))) ; 32 tiles per row
          (pal palette-background))
      (labels ((tile-id (byte)
                 (if (nonzerop flag-background-tile-set)
                   ; tileset 1 is signed
                   byte
                   ; tileset 0 is just normal
                   (+ 256 (unsigned-to-signed-8 byte))))
               (background-map-ref (x)
                 ;; Take a screen X coordinate and retrieve the tile id from the
                 ;; appropriate background map.
                 (-<> background-map-offset
                   (+ <> (floor (+ scroll-x x) 8))
                   (aref vram <>)
                   (tile-id <>))))
        (iterate
          (with g = gui)
          (with y = line)
          (with cache = tile-cache)
          (with tile-row = (mod (+ y scroll-y) 8))
          (with sx = scroll-x)

          (for x :from 0 :below 160)
          (for full-x = (+ x sx))
          (for color = (tile-pixel cache (background-map-ref x)
                                   tile-row (mod full-x 8)))
          (gameboy.gui::blit-pixel g x y (apply-palette pal color)))))))


(defun sprite-on-scanline-p (sprite line)
  (with-sprite (sprite)
    (<= y line (+ y 7))))

(defun render-sprite-scanline (gameboy sprite)
  (let ((gpu (gb-gpu gameboy)))
    (with-gpu (gpu)
      (when (sprite-on-scanline-p sprite line)
        (iterate
          (with screen-y = line)
          (with pal = (if (zerop (sprite-palette sprite))
                        palette-object-1
                        palette-object-2))
          (with sprite-row = (- screen-y (sprite-y sprite)))
          (with tile-row = (if (sprite-y-flip sprite)
                             (- 7 sprite-row)
                             sprite-row))

          (for x-offset :from 0 :below 8)
          (for screen-x = (+ (sprite-x sprite) x-offset))
          (for tile-col = (if (sprite-x-flip sprite)
                            (- 7 x-offset)
                            x-offset))
          (for background-pixel = (get-pixel gpu screen-x screen-y))
          (for sprite-pixel = (tile-pixel tile-cache (sprite-tile sprite)
                                          tile-row tile-col))
          (when (and (<= 0 screen-x 159)
                     (nonzerop sprite-pixel)
                     (or (zerop background-pixel)
                         (sprite-priority sprite)))
            (render-pixel gpu screen-x screen-y
                          (apply-palette pal sprite-pixel))))))))


(defun render-sprites-scanline (gameboy)
  (with-gpu ((gb-gpu gameboy))
    (iterate (for sprite-index :below 40)
             (for sprite = (aref sprite-cache sprite-index))
             (render-sprite-scanline gameboy sprite))))


(defun scanline (gameboy)
  (with-gpu ((gb-gpu gameboy))
    (gameboy.gui::set-debug gui `())
    (clear-scanline gameboy)
    (when flag-display (render-background-scanline gameboy))
    (when flag-sprites (render-sprites-scanline gameboy)))
  nil)


(defun step-gpu (gameboy cycles)
  "Step the GPU.

  `cycles` should be given in CPU cycles.

  "
  (with-gpu ((gb-gpu gameboy))
    (incf clock cycles)
    (case mode
      ;; HBlank
      (0 (when (>= clock 204)
           ;; Once HBlank is finished, increment the line and flip back to OAM
           ;; mode.  Unless this was the LAST line, then render the frame to the
           ;; physical screen and go into VBlank instead.
           (decf clock 204)
           (incf line)
           (if (= line 143)
             (progn (gameboy.gui::refresh-screen gui)
                    (setf mode 1)
                    (mark-interrupt gameboy :vblank))
             (setf mode 2))))
      ;; VBlank
      (1 (when (>= clock 456)
           (decf clock 456)
           (incf line)
           (when (> line 153)
             (setf mode 2 line 0))))
      ;; Scan (OAM)
      (2 (when (>= clock 80)
           ;; When the OAM portion of the scanline is done, just flip the mode.
           (decf clock 80)
           (setf mode 3)))
      ;; Scan (VRAM)
      (3 (when (>= clock 172)
           ;; When the VRAM portion of the scanline is done, flip the mode and
           ;; also blit the scanline into the framebuffer.
           (decf clock 172)
           (setf mode 0)
           (scanline gameboy))))))


;;;; Opcodes ------------------------------------------------------------------
(deftype opcode-array ()
  '(simple-array t (256)))


(defun unimplemented-opcode (code gameboy)
  (declare (ignore gameboy))
  (error "Unimplemented opcode #x~2,'0X!" code))

(defun unimplemented-extended-opcode (code gameboy)
  (declare (ignore gameboy))
  (error "Unimplemented extended opcode #x~2,'0X!" code))

(defparameter *unimplemented-functions*
  (iterate (for code :from 0 :below 256)
           (collect (curry #'unimplemented-opcode code))))

(defparameter *unimplemented-extended-functions*
  (iterate (for code :from 0 :below 256)
           (collect (curry #'unimplemented-extended-opcode code))))


(declaim (type opcode-array *opcodes* *extended-opcodes*))

(defparameter *opcodes*
  (make-array 256
    :element-type 'opcode-function
    :initial-contents *unimplemented-functions*
    :adjustable nil
    :fill-pointer nil))

(defparameter *extended-opcodes*
  (make-array 256
    :element-type 'opcode-function
    :initial-contents *unimplemented-extended-functions*
    :adjustable nil
    :fill-pointer nil))


(defmacro define-opcode (name arguments &body body)
  (let* ((name (symb 'op- name))
         (argument-length 0)
         (argument-bindings (iterate
                              (with offset = 0)
                              (for argument :in arguments)
                              (destructuring-bind (symbol &optional (size 8))
                                  (ensure-list argument)
                                (collect `(,symbol (,(case size
                                                       (8 'mem-8)
                                                       (16 'mem-16))
                                                     gameboy (+ pc ,offset))))
                                (incf offset (/ size 8)))
                              (finally (setf argument-length offset)))))
    `(progn
      (declaim (ftype opcode-function ,name))
      (defun ,name (gameboy)
        (with-gameboy (gameboy)
          (let ,argument-bindings
            ,@(when (not (zerop argument-length))
                `((increment-pc gameboy ,argument-length)))
            ,@body))
        gameboy))))


;;; Load & Store
(macro-map (register (a b c d e h l))                  ; LD r, i
  `(define-opcode ,(symb 'ld-r/ register '<i) (val)
    (setf ,register val)
    (increment-clock gameboy 2)))

(macro-map ((destination source)                       ; LD r, r
            #.(map-product #'list
                           '(a b c d e h l)
                           '(a b c d e h l)))
  `(define-opcode ,(symb 'ld-r/ destination '<r/ source) ()
    (setf ,destination ,source)
    (increment-clock gameboy)))

(macro-map (register (a b c d e h l))                  ; LD r, (HL)
  `(define-opcode ,(symb 'ld-r/ register '<mem/hl) ()
    (setf ,register (mem-8 gameboy hl))
    (increment-clock gameboy 2)))

(macro-map (register (a b c d e h l))                  ; LD (HL), r
  `(define-opcode ,(symb 'ld-mem/hl<r/ register) ()
    (setf (mem-8 gameboy hl) ,register)
    (increment-clock gameboy 2)))

(define-opcode ld-mem/hl<i (val)                       ; LD (HL), i
  (setf (mem-8 gameboy hl) val)
  (increment-clock gameboy 3))

(macro-map (register (bc de))                          ; LD (BC/DE), A
  `(define-opcode ,(symb 'ld-mem/ register '<r/a) ()
    (setf (mem-8 gameboy ,register) a)
    (increment-clock gameboy 2)))

(define-opcode ld-mem/i<r/a ((val 16))                 ; LD (i), A
  (setf (mem-8 gameboy val) a)
  (increment-clock gameboy 4))

(macro-map (register (bc de))                          ; LD A, (BC/DE)
  `(define-opcode ,(symb 'ld-r/a<mem/ register) ()
    (setf a (mem-8 gameboy ,register))
    (increment-clock gameboy 2)))

(define-opcode ld-r/a<mem/i ((val 16))                 ; LD A, (i)
  (setf a (mem-8 gameboy val))
  (increment-clock gameboy 4))

(macro-map (register (bc de hl))                       ; LD BC/DE/HL, i
  `(define-opcode ,(symb 'ld-r/ register '<i) ((val 16))
    ;; todo check this
    (setf ,register val)
    (increment-clock gameboy 3)))

(define-opcode ld-r/sp<i ((val 16))                    ; LD SP, i
  (setf sp val)
  (increment-clock gameboy 3))

(define-opcode ld-mem/i<r/sp ((val 16))                ; LD (i), SP
  (setf (mem-16 gameboy val) sp)
  (increment-clock gameboy 5))

(define-opcode ldi-mem/hl<r/a ()                       ; LDI (HL), A
  (setf (mem-8 gameboy hl) a)
  (incf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldi-r/a<mem/hl ()                       ; LDI A, (HL)
  (setf a (mem-8 gameboy hl))
  (incf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldd-mem/hl<r/a ()                       ; LDD (HL), A
  (setf (mem-8 gameboy hl) a)
  (decf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldd-r/a<mem/hl ()                       ; LDD A, (HL)
  (setf a (mem-8 gameboy hl))
  (decf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldh-r/a<mem/i (val)                     ; LDH A, (i)
  (setf a (mem-8 gameboy (+ #xff00 val)))
  (increment-clock gameboy 3))

(define-opcode ldh-mem/i<r/a (val)                     ; LDH (i), A
  (setf (mem-8 gameboy (+ #xff00 val)) a)
  (increment-clock gameboy 3))

(define-opcode ld-r/a<mem/c ()                         ; LDH A, (C)
  (setf a (mem-8 gameboy (+ #xff00 c)))
  (increment-clock gameboy 2))

(define-opcode ld-mem/c<r/a ()                         ; LDH (C), A
  (setf (mem-8 gameboy (+ #xff00 c)) a)
  (increment-clock gameboy 2))

(define-opcode ld-r/hl<mem/sp+i (offset)               ; LD HL, SP+i
  (with-chopped-16 (full trunc (+ sp (unsigned-to-signed-8 offset)))
    (setf hl trunc)
    (set-flag gameboy
              :zero nil
              :subtract nil
              :half-carry nil ; todo
              :carry (> full 255)))
  (increment-clock gameboy 3))


;;; Swap
(macro-map (register (a b c d e h l))                  ; SWAP r
  `(define-opcode ,(symb 'swap-r/ register) ()
    (-<> ,register
      (swap-nibbles <>)
      (setf ,register <>)
      (zerop <>)
      (set-flag gameboy :zero <>
                :subtract nil :half-carry nil :carry nil))
    (increment-clock gameboy 2)))

(define-opcode swap-mem/hl ()                          ; SWAP (HL)
  (-<> hl
    (mem-8 gameboy <>)
    (swap-nibbles <>)
    (setf (mem-8 gameboy hl) <>)
    (zerop <>)
    (set-flag gameboy :zero <>
              :subtract nil :half-carry nil :carry nil))
  (increment-clock gameboy 4))


;;; Arithmetic
(defmacro operate-into% (operation destination source carry &key
                         ignore-result)
  `(multiple-value-bind (result zero subtract half-carry carry)
    (,operation ,destination ,source ,carry)
    ,(if ignore-result
       '(declare (ignore result))
       `(setf ,destination result))
    (set-flag gameboy
              :zero zero
              :subtract subtract
              :half-carry half-carry
              :carry carry)))


(macro-map                                             ; ADD/ADC A, *
  (((name source &optional (clock 1) (arglist ())) with-carry)
   #.(map-product #'list '((r/a    a)
                           (r/b    b)
                           (r/c    c)
                           (r/d    d)
                           (r/e    e)
                           (r/h    h)
                           (r/l    l)
                           (mem/hl (mem-8 gameboy hl) 2)
                           (i      val                2 (val)))
                  '(nil t)))
  `(define-opcode ,(symb (if with-carry 'adc 'add) '-r/a< name) ,arglist
    (operate-into% +_8 a ,source
                   ,(if with-carry 'flag-carry 0))
    (increment-clock gameboy ,clock)))

(macro-map (register (bc de hl sp))                    ; ADD HL, BC/DE/HL/SP
  `(define-opcode ,(symb 'add-r/hl<r/ register) ()
    (multiple-value-bind (result zero half-carry carry)
        (+_16 hl ,register)
      (declare (ignore zero))
      (setf hl result)
      (set-flag gameboy
                ; the zero flag isn't affected here for some weird fuckin' reason
                :subtract nil
                :half-carry half-carry
                :carry carry))
    (increment-clock gameboy 2)))

(define-opcode add-r/sp<i (val)                        ; ADD SP, i
  ; todo this hole thing is fucked, especially the flags, fix later
  (incf-16 sp (unsigned-to-signed-8 val))
  (set-flag gameboy
            :zero nil ; lol what gameboy?
            :subtract nil
            :half-carry nil ; todo
            :carry nil) ; todo
  (increment-clock gameboy 4))

(macro-map                                             ; SUB/SBC A, *
  (((name source &optional (clock 1) (arglist ())) with-carry)
   #.(map-product #'list '((r/a    a)
                           (r/b    b)
                           (r/c    c)
                           (r/d    d)
                           (r/e    e)
                           (r/h    h)
                           (r/l    l)
                           (mem/hl (mem-8 gameboy hl) 2)
                           (i      val                2 (val)))
                  '(nil t)))
  `(define-opcode ,(symb (if with-carry 'sbc 'sub) '-r/a< name) ,arglist
    (operate-into% -_8 a ,source
                   ,(if with-carry 'flag-carry 0))
    (increment-clock gameboy ,clock)))

(macro-map                                             ; CP A, *
    ((name source &optional (clock 1) (arglist ()))
     ((r/a    a)
      (r/b    b)
      (r/c    c)
      (r/d    d)
      (r/e    e)
      (r/h    h)
      (r/l    l)
      (mem/hl (mem-8 gameboy hl) 2)
      (i      val                2 (val))))
  `(define-opcode ,(symb 'cp-r/a= name) ,arglist
    (operate-into% -_8 a ,source 0 :ignore-result t)
    (increment-clock gameboy ,clock)))

(macro-map                                             ; INC/DEC * (8-bit)
  (((name source &optional (clock 1))
    (op-name operation))
   #.(map-product #'list
                  '((r/a    a)
                    (r/b    b)
                    (r/c    c)
                    (r/d    d)
                    (r/e    e)
                    (r/h    h)
                    (r/l    l)
                    (mem/hl (mem-8 gameboy hl) 3))
                  '((inc +_8)
                    (dec -_8))))
  `(define-opcode ,(symb op-name '- name) ()
    (multiple-value-bind (result zero subtract half-carry carry)
        (,operation ,source 1)
      (declare (ignore carry)) ;; carry is unaffected for some reason...
      (setf ,source result)
      (set-flag gameboy
                :zero zero
                :subtract subtract
                :half-carry half-carry))
    (increment-clock gameboy ,clock)))

(macro-map                                             ; INC/DEC * (16-bit)
  (((name source) (op-name operation))
   #.(map-product #'list
                  '((r/bc bc)
                    (r/de de)
                    (r/hl hl)
                    (r/sp sp))
                  '((inc +_16)
                    (dec -_16))))
  `(define-opcode ,(symb op-name '- name) ()
    (setf ,source (,operation ,source 1))
    (increment-clock gameboy 2)))


;;; Miscellaneous
(define-opcode daa ()                                  ; DAA
  (if flag-subtract
    (progn (when flag-half-carry
             (zapf a (chop-8 (- % #x06))))
           (when flag-carry
             (decf a #x60)))
    (progn (when (or flag-half-carry (> (low-nibble a) 9))
             (incf a #x06))
           (when (or flag-carry (> (high-nibble a) 9)) ; todo check this
             (incf a #x60))))
  (let ((full a)
        (trunc (chop-8 a)))
    (set-flag gameboy
              :zero (zerop trunc)
              ; :subtract flag is unaffected
              :half-carry 0
              :carry (> full #xFF))
    (setf a trunc))
  (increment-clock gameboy))

(define-opcode nop ()                                  ; NOP
  (increment-clock gameboy))

(define-opcode halt ()                                 ; HALT
  (setf halt 1)
  (increment-clock gameboy))


;;; Logic
(macro-map                                             ; AND/OR/XOR A, *
  (((name source &optional (clock 1) (arglist ()))
    (op-name operation hc))
   #.(map-product #'list
                  '((r/a    a)
                    (r/b    b)
                    (r/c    c)
                    (r/d    d)
                    (r/e    e)
                    (r/h    h)
                    (r/l    l)
                    (mem/hl (mem-8 gameboy hl) 2)
                    (i      val                2 (val)))
                  '((and logand t)
                    (or logior nil)
                    (xor logxor nil))))
  `(define-opcode ,(symb op-name '-r/a< name) ,arglist
    (-<> ,source
      (,operation a <>)
      (setf a <>)
      (zerop <>)
      (set-flag gameboy
                :zero <>
                :subtract nil
                :half-carry ,hc
                :carry nil))
    (increment-clock gameboy ,clock)))


;;; Bit Twiddling
(macro-map                                             ; BIT b, *
  (((name source &optional (clock 2)) bit)
   #.(map-product #'list
                  '((r/a    a)
                    (r/b    b)
                    (r/c    c)
                    (r/d    d)
                    (r/e    e)
                    (r/h    h)
                    (r/l    l)
                    (mem/hl (mem-8 gameboy hl) 4))
                  '(0 1 2 3 4 5 6 7)))
  `(define-opcode ,(symb 'bit- bit '- name) ()
    (set-flag gameboy
              :zero (logbitp ,bit ,source)
              :subtract nil
              ;; carry is unaffected
              :half-carry t)
    (increment-clock gameboy ,clock)))

(macro-map                                             ; SET/RES b, *
  (((name source &optional (clock 2)) bit (op-name value))
   #.(map-product #'list
                  '((r/a    a)
                    (r/b    b)
                    (r/c    c)
                    (r/d    d)
                    (r/e    e)
                    (r/h    h)
                    (r/l    l)
                    (mem/hl (mem-8 gameboy hl) 4))
                  '(0 1 2 3 4 5 6 7)
                  '((set 1) (res 0))))
  `(define-opcode ,(symb op-name '- bit '- name) ()
    (zapf ,source (set-bit ,bit % ,value))
    (increment-clock gameboy ,clock)))

(define-opcode cpl ()                                  ; CPL
  (zapf a (chop-8 (lognot %)))
  (set-flag gameboy
            :subtract t
            :half-carry t)
  (increment-clock gameboy))

(define-opcode ccf ()                                  ; CCF
  (set-flag gameboy
            :subtract nil
            :half-carry nil
            :carry (flip flag-carry))
  (increment-clock gameboy))

(define-opcode scf ()                                  ; SCF
  (set-flag gameboy
            :subtract nil
            :half-carry nil
            :carry 1)
  (increment-clock gameboy))


;;; Rotate
(macro-map                                             ; RLA/RRA/RL/RR
  (((name source clock)
    (direction offset))
   #.(map-product #'list
                  '((a       a                  1)
                    (-r/a    a                  2)
                    (-r/b    b                  2)
                    (-r/c    c                  2)
                    (-r/d    d                  2)
                    (-r/e    e                  2)
                    (-r/h    h                  2)
                    (-r/l    l                  2)
                    (-mem/hl (mem-8 gameboy hl) 4))
                  '((l 1)
                    (r -1))))
  `(define-opcode ,(symb 'r direction name) ()
    (with-chopped-8 (full trunc (rot 9 (cat ,source flag-carry 8 1)
                                     ,offset))
      (set-flag gameboy
                ; todo: http://www.devrs.com/gb/files/opcodes.html says the zero
                ; flag is always reset for the bare A variants...
                :zero (zerop trunc)
                :subtract nil
                :half-carry nil
                :carry (bit 8 full))
      (setf ,source trunc))
    (increment-clock gameboy ,clock)))

(macro-map                                             ; RLCA/RRCA/RLC/RRC
  (((name source clock)
    (direction offset))
   #.(map-product #'list
                  '((a       a                  1)
                    (-r/a    a                  2)
                    (-r/b    b                  2)
                    (-r/c    c                  2)
                    (-r/d    d                  2)
                    (-r/e    e                  2)
                    (-r/h    h                  2)
                    (-r/l    l                  2)
                    (-mem/hl (mem-8 gameboy hl) 4))
                  '((l 1)
                    (r -1))))
  `(define-opcode ,(symb 'r direction 'c name) ()
    (let ((result (rot 8 ,source ,offset)))
      (set-flag gameboy
                ; todo: http://www.devrs.com/gb/files/opcodes.html says the zero
                ; flag is always reset for the bare A variants...
                :zero (zerop result)
                :subtract nil
                :half-carry nil
                :carry (bit 7 a))
      (setf ,source result))
    (increment-clock gameboy ,clock)))


;;; Shift
(macro-map                                             ; SLA *
  ((name source &optional (clock 2))
   ((r/a    a)
    (r/b    b)
    (r/c    c)
    (r/d    d)
    (r/e    e)
    (r/h    h)
    (r/l    l)
    (mem/hl (mem-8 gameboy hl) 4)))
  `(define-opcode ,(symb 'sla- name) ()
    (with-chopped-8 (full trunc (ash ,source 1))
      (set-flag gameboy
                :zero (zerop trunc)
                :subtract nil
                :half-carry nil
                :carry (bit 8 full))
      (setf ,source trunc))
    (increment-clock gameboy ,clock)))

(macro-map                                             ; SRA *
  ((name source &optional (clock 2))
   ((r/a    a)
    (r/b    b)
    (r/c    c)
    (r/d    d)
    (r/e    e)
    (r/h    h)
    (r/l    l)
    (mem/hl (mem-8 gameboy hl) 4)))
  `(define-opcode ,(symb 'sra- name) ()
    (let ((orig ,source))
      (with-chopped-8 (full trunc (-<> orig
                                    (ash <> -1)
                                    (set-bit 8 <> (bit 8 orig))))
        (set-flag gameboy
                  :zero (zerop trunc)
                  :subtract nil
                  :half-carry nil
                  :carry (bit 0 orig))
        (setf ,source trunc)))
    (increment-clock gameboy ,clock)))

(macro-map                                             ; SRL *
  ((name source &optional (clock 2))
   ((r/a    a)
    (r/b    b)
    (r/c    c)
    (r/d    d)
    (r/e    e)
    (r/h    h)
    (r/l    l)
    (mem/hl (mem-8 gameboy hl) 4)))
  `(define-opcode ,(symb 'sra- name) ()
    (let* ((orig ,source)
           (trunc (chop-8 (ash orig -1))))
      (set-flag gameboy
                :zero (zerop trunc)
                :subtract nil
                :half-carry nil
                :carry (bit 0 orig))
      (setf ,source trunc))
    (increment-clock gameboy ,clock)))


;;; Stack
(macro-map                                             ; PUSH *
  ((name register)
   ((r/af af)
    (r/bc bc)
    (r/de de)
    (r/hl hl)))
  `(define-opcode ,(symb 'push- name) ()
    (stack-push gameboy ,register)
    (increment-clock gameboy 4)))

(macro-map                                             ; POP *
  ((name register)
   ((r/af af)
    (r/bc bc)
    (r/de de)
    (r/hl hl)))
  `(define-opcode ,(symb 'pop- name) ()
    (setf ,register (stack-pop gameboy))
    (increment-clock gameboy 3)))


;;; Jumps
(define-opcode jp-i ((target 16))                      ; JP, i
  (setf pc target)
  (increment-clock gameboy 3))

(define-opcode jp-mem/hl ()                            ; JP, (HL)
  (setf pc hl)
  (increment-clock gameboy 1))

(define-opcode jr-i (offset)                           ; JR, i
  (incf pc (unsigned-to-signed-8 offset))
  (increment-clock gameboy 2))

(macro-map                                             ; JP cond, i
  ((name flag flag-value)
   ((nz flag-zero  0)
    (z  flag-zero  1)
    (nc flag-carry 0)
    (c  flag-carry 1)))
  `(define-opcode ,(symb 'jp- name '-i) ((target 16))
    (if (= ,flag ,flag-value)
      (progn (setf pc target)
             (increment-clock gameboy 4))
      (increment-clock gameboy 3))))

(macro-map                                             ; JR cond, i
  ((name flag flag-value)
   ((nz flag-zero  0)
    (z  flag-zero  1)
    (nc flag-carry 0)
    (c  flag-carry 1)))
  `(define-opcode ,(symb 'jr- name '-i) (offset)
    (if (= ,flag ,flag-value)
      (progn (incf pc (unsigned-to-signed-8 offset))
             (increment-clock gameboy 3))
      (increment-clock gameboy 2))))


;;; Call/Return
(define-opcode call-i ((target 16))                    ; CALL i
  (stack-push gameboy pc)
  (setf pc target)
  (increment-clock gameboy 6))

(macro-map                                             ; CALL cond, i
  ((name flag flag-value)
   ((nz flag-zero  0)
    (z  flag-zero  1)
    (nc flag-carry 0)
    (c  flag-carry 1)))
  `(define-opcode ,(symb 'call- name '-i) ((target 16))
    (if (= ,flag ,flag-value)
      (progn
        (stack-push gameboy pc)
        (setf pc target)
        (increment-clock gameboy 6))
      (increment-clock gameboy 3))))

(define-opcode ret ()                                  ; RET
  (setf pc (stack-pop gameboy))
  (increment-clock gameboy 4))

(macro-map                                             ; RET cond
  ((name flag flag-value)
   ((nz flag-zero  0)
    (z  flag-zero  1)
    (nc flag-carry 0)
    (c  flag-carry 1)))
  `(define-opcode ,(symb 'ret- name) ()
    (if (= ,flag ,flag-value)
      (progn
        (setf pc (stack-pop gameboy))
        (increment-clock gameboy 5))
      (increment-clock gameboy 2))))


;;; Interrupts
(define-opcode di ()                                   ; DI
  (setf interrupt-master nil) ; todo fix for the real-world 1-instr lag)
  (increment-clock gameboy))

(define-opcode ei ()                                   ; EI
  (setf interrupt-master t) ; todo fix for the real-world 1-instr lag
  (increment-clock gameboy 1))

(macro-map                                             ; RST addr
  ((address) (#x00 #x08 #x10 #x18 #x20 #x28 #x30 #x38 #x40 #x48 #x50 #x58 #x60))
  `(define-opcode ,(symb 'rst- (format nil "~2,'0X" address)) ()
    (stack-push gameboy pc)
    (setf pc ,address)
    (increment-clock gameboy 4)))

(define-opcode reti ()                                 ; RETI
  (setf pc (stack-pop gameboy)
        interrupt-master t)
  (increment-clock gameboy 4))


;;; Extended Opcodes
(define-opcode extended (code)
  (funcall (the opcode-function (aref *extended-opcodes* code)) ; trust me, lisp
           gameboy))


;;; Tables
(defparameter *opcodes-list*
  '((#x00 nop)
    (#x01 ld-r/bc<i)
    (#x02 ld-mem/bc<r/a)
    (#x03 inc-r/bc)
    (#x04 inc-r/b)
    (#x05 dec-r/b)
    (#x06 ld-r/b<i)
    (#x07 rlc-r/a)
    (#x08 ld-mem/i<r/sp)
    (#x09 add-r/hl<r/bc)
    (#x0A ld-r/a<mem/bc)
    (#x0B dec-r/bc)
    (#x0C inc-r/c)
    (#x0D dec-r/c)
    (#x0E ld-r/c<i)
    (#x0F rrc-r/a)

    ; (#x10 stop) todo
    (#x11 ld-r/de<i)
    (#x12 ld-mem/de<r/a)
    (#x13 inc-r/de)
    (#x14 inc-r/d)
    (#x15 dec-r/d)
    (#x16 ld-r/d<i)
    (#x17 rl-r/a)
    (#x18 jr-i)
    (#x19 add-r/hl<r/de)
    (#x1A ld-r/a<mem/de)
    (#x1B dec-r/de)
    (#x1C inc-r/e)
    (#x1D dec-r/e)
    (#x1E ld-r/e<i)
    (#x1F rr-r/a)

    (#x20 jr-nz-i)
    (#x21 ld-r/hl<i)
    (#x22 ldi-mem/hl<r/a)
    (#x23 inc-r/hl)
    (#x24 inc-r/h)
    (#x25 dec-r/h)
    (#x26 ld-r/h<i)
    (#x27 daa)
    (#x28 jr-z-i)
    (#x29 add-r/hl<r/hl)
    (#x2A ldi-r/a<mem/hl)
    (#x2B dec-r/hl)
    (#x2C inc-r/l)
    (#x2D dec-r/l)
    (#x2E ld-r/l<i)
    (#x2F cpl)

    (#x30 jr-nc-i)
    (#x31 ld-r/sp<i)
    (#x32 ldd-mem/hl<r/a)
    (#x33 inc-r/sp)
    (#x34 inc-mem/hl)
    (#x35 dec-mem/hl)
    (#x36 ld-mem/hl<i)
    (#x37 scf)
    (#x38 jr-c-i)
    (#x39 add-r/hl<r/sp)
    (#x3A ldd-r/a<mem/hl)
    (#x3B dec-r/sp)
    (#x3C inc-r/a)
    (#x3D dec-r/a)
    (#x3E ld-r/a<i)
    (#x3F ccf)

    (#x40 ld-r/b<r/b)
    (#x41 ld-r/b<r/c)
    (#x42 ld-r/b<r/d)
    (#x43 ld-r/b<r/e)
    (#x44 ld-r/b<r/h)
    (#x45 ld-r/b<r/l)
    (#x46 ld-r/b<mem/hl)
    (#x47 ld-r/b<r/a)
    (#x47 ld-r/b<r/a)
    (#x48 ld-r/c<r/b)
    (#x49 ld-r/c<r/c)
    (#x4A ld-r/c<r/d)
    (#x4B ld-r/c<r/e)
    (#x4C ld-r/c<r/h)
    (#x4D ld-r/c<r/l)
    (#x4E ld-r/c<mem/hl)
    (#x4F ld-r/c<r/a)

    (#x50 ld-r/d<r/b)
    (#x51 ld-r/d<r/c)
    (#x52 ld-r/d<r/d)
    (#x53 ld-r/d<r/e)
    (#x54 ld-r/d<r/h)
    (#x55 ld-r/d<r/l)
    (#x56 ld-r/d<mem/hl)
    (#x57 ld-r/d<r/a)
    (#x58 ld-r/e<r/b)
    (#x59 ld-r/e<r/c)
    (#x5A ld-r/e<r/d)
    (#x5B ld-r/e<r/e)
    (#x5C ld-r/e<r/h)
    (#x5D ld-r/e<r/l)
    (#x5E ld-r/e<mem/hl)
    (#x5F ld-r/e<r/a)

    ; (#x60 )
    ; (#x61 )
    ; (#x62 )
    ; (#x63 )
    ; (#x64 )
    ; (#x65 )
    (#x66 ld-r/h<mem/hl)
    (#x67 ld-r/h<r/a)
    (#x68 ld-r/l<r/b)
    (#x69 ld-r/l<r/c)
    (#x6A ld-r/l<r/d)
    (#x6B ld-r/l<r/e)
    (#x6C ld-r/l<r/h)
    (#x6D ld-r/l<r/l)
    (#x6E ld-r/l<mem/hl)
    (#x6F ld-r/l<r/a)

    ; (#x70 )
    ; (#x71 )
    ; (#x72 )
    ; (#x73 )
    ; (#x74 )
    ; (#x75 )
    ; (#x76 )
    (#x77 ld-mem/hl<r/a)
    (#x78 ld-r/a<r/b)
    (#x79 ld-r/a<r/c)
    (#x7A ld-r/a<r/d)
    (#x7B ld-r/a<r/e)
    (#x7C ld-r/a<r/h)
    (#x7D ld-r/a<r/l)
    (#x7E ld-r/a<mem/hl)
    (#x7F ld-r/a<r/a)

    (#x80 add-r/a<r/b)
    (#x81 add-r/a<r/c)
    (#x82 add-r/a<r/d)
    (#x83 add-r/a<r/e)
    (#x84 add-r/a<r/h)
    (#x85 add-r/a<r/l)
    (#x86 add-r/a<mem/hl)
    (#x87 add-r/a<r/a)
    (#x88 adc-r/a<r/b)
    (#x89 adc-r/a<r/c)
    (#x8A adc-r/a<r/d)
    (#x8B adc-r/a<r/e)
    (#x8C adc-r/a<r/h)
    (#x8D adc-r/a<r/l)
    (#x8E adc-r/a<mem/hl)
    (#x8F adc-r/a<r/a)

    (#x90 sub-r/a<r/b)
    (#x91 sub-r/a<r/c)
    (#x92 sub-r/a<r/d)
    (#x93 sub-r/a<r/e)
    (#x94 sub-r/a<r/h)
    (#x95 sub-r/a<r/l)
    (#x96 sub-r/a<mem/hl)
    (#x97 sub-r/a<r/a)
    (#x98 sbc-r/a<r/b)
    (#x99 sbc-r/a<r/c)
    (#x9A sbc-r/a<r/d)
    (#x9B sbc-r/a<r/e)
    (#x9C sbc-r/a<r/h)
    (#x9D sbc-r/a<r/l)
    (#x9E sbc-r/a<mem/hl)
    (#x9F sbc-r/a<r/a)

    (#xA0 and-r/a<r/b)
    (#xA1 and-r/a<r/c)
    (#xA2 and-r/a<r/d)
    (#xA3 and-r/a<r/e)
    (#xA4 and-r/a<r/h)
    (#xA5 and-r/a<r/l)
    (#xA6 and-r/a<mem/hl)
    ; (#xA7 )
    (#xA8 xor-r/a<r/b)
    ; (#xA9 )
    ; (#xAA )
    ; (#xAB )
    ; (#xAC )
    ; (#xAD )
    ; (#xAE )
    (#xAF xor-r/a<r/a)

    (#xB0 or-r/a<r/b)
    (#xB1 or-r/a<r/c)
    (#xB2 or-r/a<r/d)
    (#xB3 or-r/a<r/e)
    (#xB4 or-r/a<r/h)
    (#xB5 or-r/a<r/l)
    (#xB6 or-r/a<mem/hl)
    (#xB7 or-r/a<r/a)
    (#xB8 cp-r/a=r/b)
    (#xB9 cp-r/a=r/c)
    (#xBA cp-r/a=r/d)
    (#xBB cp-r/a=r/e)
    (#xBC cp-r/a=r/h)
    (#xBD cp-r/a=r/l)
    (#xBE cp-r/a=mem/hl)
    (#xBF cp-r/a=r/a)

    (#xC0 ret-nz)
    (#xC1 pop-r/bc)
    (#xC2 jp-nz-i)
    (#xC3 jp-i)
    (#xC4 call-nz-i)
    (#xC5 push-r/bc)
    ; (#xC6 )
    (#xC7 rst-00)
    (#xC8 ret-z)
    (#xC9 ret)
    (#xCA jp-z-i)
    (#xCB extended)
    ; (#xCC )
    (#xCD call-i)
    ; (#xCE )
    (#xCF rst-08)

    (#xD0 ret-nc)
    (#xD1 pop-r/de)
    ; (#xD2 )
    ; (#xD3 )
    (#xD4 call-nc-i)
    (#xD5 push-r/de)
    ; (#xD6 )
    (#xD7 rst-10)
    (#xD8 ret-c)
    (#xD9 reti)
    ; (#xDA )
    ; (#xDB )
    ; (#xDC )
    ; (#xDD )
    ; (#xDE )
    (#xDF rst-18)

    (#xE0 ldh-mem/i<r/a)
    (#xE1 pop-r/hl)
    (#xE2 ld-mem/c<r/a)
    ; (#xE3 )
    ; (#xE4 )
    (#xE5 push-r/hl)
    (#xE6 and-r/a<i)
    (#xE7 rst-20)
    (#xE8 add-r/sp<i)
    ; (#xE9 )
    (#xEA ld-mem/i<r/a)
    ; (#xEB )
    ; (#xEC )
    ; (#xED )
    ; (#xEE )
    (#xEF rst-28)

    (#xF0 ldh-r/a<mem/i)
    (#xF1 pop-r/af)
    (#xF2 ld-r/a<mem/c)
    (#xF3 di)
    ; (#xF4 )
    (#xF5 push-r/af)
    (#xF6 or-r/a<i)
    (#xF7 rst-30)
    ; (#xF8 )
    ; (#xF9 )
    (#xFA ld-r/a<mem/i)
    (#xFB ei)
    ; (#xFC )
    ; (#xFD )
    (#xFE cp-r/a=i)
    (#xFF rst-38)
    ))

(defparameter *extended-list*
  '((#x00 rlc-r/b)
    (#x01 rlc-r/c)
    (#x02 rlc-r/d)
    (#x03 rlc-r/e)
    (#x04 rlc-r/h)
    (#x05 rlc-r/l)
    ; (#x06 )
    ; (#x07 )
    ; (#x08 )
    ; (#x09 )
    ; (#x0A )
    ; (#x0B )
    ; (#x0C )
    ; (#x0D )
    ; (#x0E )
    ; (#x0F )
    (#x10 rl-r/b)
    (#x11 rl-r/c)
    (#x12 rl-r/d)
    (#x13 rl-r/e)
    (#x14 rl-r/h)
    (#x15 rl-r/l)
    (#x16 rl-mem/hl)
    (#x17 rl-r/a)
    (#x18 rr-r/b)
    (#x19 rr-r/c)
    (#x1A rr-r/d)
    (#x1B rr-r/e)
    (#x1C rr-r/h)
    (#x1D rr-r/l)
    (#x1E rr-mem/hl)
    (#x1F rr-r/a)
    (#x20 sla-r/b)
    (#x21 sla-r/c)
    (#x22 sla-r/d)
    (#x23 sla-r/e)
    (#x24 sla-r/h)
    (#x25 sla-r/l)
    ; (#x26 )
    ; (#x27 )
    ; (#x28 )
    ; (#x29 )
    ; (#x2A )
    ; (#x2B )
    ; (#x2C )
    ; (#x2D )
    ; (#x2E )
    ; (#x2F )
    (#x30 swap-r/b)
    (#x31 swap-r/c)
    (#x32 swap-r/d)
    (#x33 swap-r/e)
    (#x34 swap-r/h)
    (#x35 swap-r/l)
    (#x36 swap-mem/hl)
    (#x37 swap-r/a)
    ; (#x38 )
    ; (#x39 )
    ; (#x3A )
    ; (#x3B )
    ; (#x3C )
    ; (#x3D )
    ; (#x3E )
    ; (#x3F )
    ; (#x40 )
    ; (#x41 )
    ; (#x42 )
    ; (#x43 )
    ; (#x44 )
    ; (#x45 )
    ; (#x46 )
    ; (#x47 )
    ; (#x48 )
    ; (#x49 )
    ; (#x4A )
    ; (#x4B )
    ; (#x4C )
    ; (#x4D )
    ; (#x4E )
    ; (#x4F )
    ; (#x50 )
    ; (#x51 )
    ; (#x52 )
    ; (#x53 )
    ; (#x54 )
    ; (#x55 )
    ; (#x56 )
    ; (#x57 )
    ; (#x58 )
    ; (#x59 )
    ; (#x5A )
    ; (#x5B )
    ; (#x5C )
    ; (#x5D )
    ; (#x5E )
    ; (#x5F )
    ; (#x60 )
    ; (#x61 )
    ; (#x62 )
    ; (#x63 )
    ; (#x64 )
    ; (#x65 )
    ; (#x66 )
    ; (#x67 )
    ; (#x68 )
    ; (#x69 )
    ; (#x6A )
    ; (#x6B )
    ; (#x6C )
    ; (#x6D )
    ; (#x6E )
    ; (#x6F )
    ; (#x70 )
    ; (#x71 )
    ; (#x72 )
    ; (#x73 )
    ; (#x74 )
    ; (#x75 )
    ; (#x76 )
    ; (#x77 )
    ; (#x78 )
    ; (#x79 )
    ; (#x7A )
    ; (#x7B )
    (#x7C bit-7-r/h)
    ; (#x7D )
    ; (#x7E )
    ; (#x7F )
    ; (#x80 )
    ; (#x81 )
    ; (#x82 )
    ; (#x83 )
    ; (#x84 )
    ; (#x85 )
    ; (#x86 )
    ; (#x87 )
    ; (#x88 )
    ; (#x89 )
    ; (#x8A )
    ; (#x8B )
    ; (#x8C )
    ; (#x8D )
    ; (#x8E )
    ; (#x8F )
    ; (#x90 )
    ; (#x91 )
    ; (#x92 )
    ; (#x93 )
    ; (#x94 )
    ; (#x95 )
    ; (#x96 )
    ; (#x97 )
    ; (#x98 )
    ; (#x99 )
    ; (#x9A )
    ; (#x9B )
    ; (#x9C )
    ; (#x9D )
    ; (#x9E )
    ; (#x9F )
    ; (#xA0 )
    ; (#xA1 )
    ; (#xA2 )
    ; (#xA3 )
    ; (#xA4 )
    ; (#xA5 )
    ; (#xA6 )
    ; (#xA7 )
    ; (#xA8 )
    ; (#xA9 )
    ; (#xAA )
    ; (#xAB )
    ; (#xAC )
    ; (#xAD )
    ; (#xAE )
    ; (#xAF )
    ; (#xB0 )
    ; (#xB1 )
    ; (#xB2 )
    ; (#xB3 )
    ; (#xB4 )
    ; (#xB5 )
    ; (#xB6 )
    ; (#xB7 )
    ; (#xB8 )
    ; (#xB9 )
    ; (#xBA )
    ; (#xBB )
    ; (#xBC )
    ; (#xBD )
    ; (#xBE )
    ; (#xBF )
    ; (#xC0 )
    ; (#xC1 )
    ; (#xC2 )
    ; (#xC3 )
    ; (#xC4 )
    ; (#xC5 )
    ; (#xC6 )
    ; (#xC7 )
    ; (#xC8 )
    ; (#xC9 )
    ; (#xCA )
    ; (#xCB )
    ; (#xCC )
    ; (#xCD )
    ; (#xCE )
    ; (#xCF )
    ; (#xD0 )
    ; (#xD1 )
    ; (#xD2 )
    ; (#xD3 )
    ; (#xD4 )
    ; (#xD5 )
    ; (#xD6 )
    ; (#xD7 )
    ; (#xD8 )
    ; (#xD9 )
    ; (#xDA )
    ; (#xDB )
    ; (#xDC )
    ; (#xDD )
    ; (#xDE )
    ; (#xDF )
    (#xE0 set-4-r/b)
    (#xE1 set-4-r/c)
    (#xE2 set-4-r/d)
    (#xE3 set-4-r/d)
    (#xE4 set-4-r/h)
    (#xE5 set-4-r/l)
    ; (#xE6 )
    ; (#xE7 )
    ; (#xE8 )
    ; (#xE9 )
    ; (#xEA )
    ; (#xEB )
    ; (#xEC )
    ; (#xED )
    ; (#xEE )
    ; (#xEF )
    ; (#xF0 )
    ; (#xF1 )
    ; (#xF2 )
    ; (#xF3 )
    ; (#xF4 )
    ; (#xF5 )
    ; (#xF6 )
    ; (#xF7 )
    ; (#xF8 )
    ; (#xF9 )
    ; (#xFA )
    ; (#xFB )
    ; (#xFC )
    ; (#xFD )
    ; (#xFE )
    ; (#xFF )
    ))


(defun load-opcode-tables ()
  (let ((*package* (find-package :gameboy)))
    (iterate
      (for (list array) :in `((,*opcodes-list* ,*opcodes*)
                              (,*extended-list* ,*extended-opcodes*)))
      (iterate (for (code name) :in list)
               (setf (aref array code)
                     (symbol-function (symb 'op- name)))))))


;;;; VM -----------------------------------------------------------------------
(defun reset (gameboy)
  ;; todo: zero out mmu/gpu arrays
  (with-gameboy (gameboy)
    (setf a 0 b 0 c 0 d 0 e 0 h 0 l 0 f 0 sp 0 pc 0 clock 0)))


(defparameter *running* nil)
(defparameter *running* t)
(defparameter *paused* nil)
(defparameter *step* nil)
(defparameter *breakpoint* #x239C)
(defparameter *breakpoint* nil)

(defparameter *gb* nil)


(defun dump-op (gameboy)
  (with-gameboy (gameboy)
    (format t "~4,'0X: (~{~2,'0X~^ ~} ...) ~A~%"
            pc
            (iterate (for i :from 0 :to 3)
                     (collect (mem-8 gameboy (+ i pc))))
            (aref *opcodes* (mem-8 gameboy pc))))
  (finish-output))

(defun dump-registers (gameboy)
  (macrolet ((slotmap (slots)
               `(iterate (for slot :in ',slots)
                 (for val :in (list ,@slots))
                 (collect slot)
                 (collect val))))
    (with-gameboy (gameboy)
      (format t "  ~{ ~A: #x~2,'0X~^     ~}~%" (slotmap (a b c d)))
      (format t "  ~{ ~A: #x~2,'0X~^     ~}~%" (slotmap (e h l f)))
      (format t "  ~{~A: #x~4,'0X   ~}~%" (slotmap (af bc de hl)))
      (format t "  ~{~A: #x~4,'0X   ~}~%" (slotmap (sp pc)))
      (format t "  Flags: #b~8,'0B/~2,'0X   IME: ~A   HALT: #x~2,'0X~%" f f interrupt-master halt)
      (format t "     IE: #b~8,'0B/~2,'0X~%" interrupt-enable interrupt-enable)
      (format t "     IF: #b~8,'0B/~2,'0X~%" interrupt-flags interrupt-flags)
      (format t "  Clock: ~D~%" clock)
      (terpri))
    (with-gpu ((gb-gpu gameboy))
      (format t "  ~{~A: ~D~%~^  ~}" (slotmap (scroll-x scroll-y)))
      (format t "   PAL-BG: #b~8,'0B~%" palette-background)
      (format t "   PAL-O1: #b~8,'0B~%" palette-object-1)
      (format t "   PAL-O2: #b~8,'0B~%" palette-object-2)
      (format t "     LINE: ~D/~2,'0X~%" line line)
      (format t "  LINECMP: ~D/~2,'0X~%" line-compare line-compare)
      (format t "     MODE: ~D/~2,'0X~%" mode mode)
      (format t "  CONTROL: #b~8,'0B/~2,'0X~%" control control)
      (format t "     STAT: #b~8,'0B/~2,'0X~%" stat stat)
      (terpri))
    (finish-output)))


(defun dispatch-interrupts (gameboy)
  (with-gameboy (gameboy)
    (when (and interrupt-master
               (nonzerop interrupt-enable)
               (nonzerop interrupt-flags))
      (setf halt 0 interrupt-master nil)
      (macrolet ((b (i op)
                   `((interrupt-bit ,i)
                     (,op gameboy)
                     (clear-interrupt gameboy ,i))))
        (case-bit (logand interrupt-enable interrupt-flags)
          (0 (op-rst-40 gameboy) (clear-interrupt gameboy :vblank))
          (1 (op-rst-48 gameboy) (clear-interrupt gameboy :lcd-stat))
          (2 (op-rst-50 gameboy) (clear-interrupt gameboy :timer))
          (3 (op-rst-58 gameboy) (clear-interrupt gameboy :serial))
          (4 (op-rst-60 gameboy) (clear-interrupt gameboy :joypad))
          (t (setf interrupt-master t)))))))


(defun run (gameboy)
  (load-opcode-tables)
  (load-rom gameboy "roms/opus2.gb")
  (setf *running* t *gb* gameboy)
  (bt:make-thread
    (lambda ()
      (let ((*package* (find-package :gameboy)))
        (with-gameboy (gameboy)
          (iterate
            (while *running*)
            (if (and *breakpoint* (= *breakpoint* pc) (not *paused*))
              (progn (dump-op gameboy)
                     (dump-registers gameboy)
                     (setf *paused* t))
              (if (or *step* (not *paused*))
                (progn
                  (incf tick)
                  (setf clock-increment 0)
                  (for opcode = (mem-8 gameboy pc))
                  (for op = (aref *opcodes* opcode))
                  (incf-16 pc)
                  (funcall op gameboy)
                  (dispatch-interrupts gameboy)
                  (incf clock clock-increment)
                  (step-gpu gameboy (* 4 clock-increment))
                  (when *step*
                    (dump-op gameboy)
                    (dump-registers gameboy)
                    (setf *step* nil))
                  (when (zerop (mod tick 1000))
                    (sleep 0.001)))
                (sleep 0.01))))))))
  (->> gameboy gb-gpu gpu-gui gameboy.gui::run-qt-gui))

(defun start ()
  (run (make-gameboy)))


;;;; TODO ---------------------------------------------------------------------
;;; * STOP opcode
