(in-package :gameboy)


(setf *print-length* 8)
(declaim (optimize (speed 1) (safety 3) (debug 3)))
; (declaim (optimize (speed 3) (safety 0) (debug 0)))


;;;; Types & Constants --------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))
(deftype sint8 () '(signed-byte 8))
(deftype sint16 () '(signed-byte 16))
(deftype memory-array () '(simple-array int8 (*)))
(deftype bit () 'cl:bit)
(deftype opcode-function () '(function (gameboy) (values gameboy &optional)))

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

(defmacro macro-map ((lambda-list items) &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))


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
  (case value
    ((0 nil) 0)
    ((1 t) 1)
    (t 1)))

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
(defstruct mmu
  (in-bios t :type boolean)
  (bios (make-bios) :type memory-array :read-only t)
  (rom (make-mem (k 16)) :type memory-array :read-only t)
  (working-ram (make-mem (k 8)) :type memory-array :read-only t)
  (external-ram (make-mem (k 8)) :type memory-array :read-only t)
  (zero-page-ram (make-mem 128) :type memory-array :read-only t))

(defstruct gpu
  (gui (gameboy.gui::make-qt-gui))
  (ram (make-mem (k 8)) :type memory-array :read-only t)
  (tile-cache (make-tile-cache) :type tile-cache :read-only t)
  (mode 0 :type (integer 0 3))
  (clock 0 :type fixnum) ; fuck it, close enough
  (line 0 :type (integer 0 154))
  (background-map 0 :type bit)
  (scroll-x 0 :type (unsigned-byte 8))
  (scroll-y 0 :type (unsigned-byte 8)))

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


(define-with-macro (mmu)
  in-bios bios rom working-ram external-ram zero-page-ram)

(define-with-macro (gpu)
  gui ram tile-cache
  mode clock line background-map
  scroll-x scroll-y)

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f pc sp clock clock-increment af bc de hl halt
  flag-zero flag-subtract flag-half-carry flag-carry)


(defmethod print-object ((object gameboy) stream)
  (print-unreadable-object (object stream :type t :identity t)))


;;;; Flag Register ------------------------------------------------------------
(declaim (inline set-flag))

(defmacro define-flag (name position)
  (let ((full-name (symb 'gb-flag- name)))
    `(progn
      (declaim (inline ,full-name)
               (ftype (function (gameboy) bit)
                      ,full-name))
      (defun ,full-name (gameboy)
        (bit ,position (gb-f gameboy))))))

(define-flag zero 7)
(define-flag subtract 6)
(define-flag half-carry 5)
(define-flag carry 4)

(declaim (ftype (function
                  (gameboy &key (:zero t) (:subtract t) (:half-carry t) (:carry t))
                  int8)
                set-flag))

(defun set-flag (gameboy &key
                 (zero :preserve)
                 (subtract :preserve)
                 (half-carry :preserve)
                 (carry :preserve))
  (flet ((set-flag% (flag index value)
           (if (eq :preserve flag)
             value
             (set-bit index value (to-bit flag)))))
    (declare (inline set-flag%))
    (zapf (gb-f gameboy)
          (-<> %
            (set-flag% zero 7 <>)
            (set-flag% subtract 6 <>)
            (set-flag% half-carry 5 <>)
            (set-flag% carry 4 <>)))))


;;;; More Utils ---------------------------------------------------------------
(declaim (inline increment-clock increment-pc
                 stack-push stack-pop))


(defun increment-clock (gameboy &optional (machine-cycles 1))
  (with-gameboy (gameboy)
    (setf clock-increment machine-cycles)))

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
  (ftype (function (gameboy int16) (values t t)) find-memory)
  (ftype (function (gameboy int16) int8) mem-8)
  (ftype (function (gameboy int16) int16) mem-16)
  (ftype (function (int8 gameboy int16)) (setf mem-8))
  (ftype (function (int16 gameboy int16)) (setf mem-16)))


(defun find-memory (gameboy address)
  (with-mmu ((gb-mmu gameboy))
    (case-range address
      ;; BIOS/ROM
      (256 (values (if in-bios bios rom) address))
      ((- (k 32) 256) (values rom address))

      ;; VRAM/ERAM/WRAM
      ((k 8) (values (gpu-ram (gb-gpu gameboy)) (- address #x8000)))
      ((k 8) (values external-ram (- address #xA000)))
      ((k 8) (values working-ram (- address #xC000)))
      ((- (k 8) 512) ; Shadow WRAM
       (values working-ram (- address #xC000)))

      ;; OAM
      (160 (values nil nil)) ; todo: oam
      ((- 256 160) (values nil nil)) ; todo: constant 0

      ;; I/O
      (128 (values nil nil)) ; todo: i/o

      ;; Zero Page
      (128 (values zero-page-ram (ldb (byte 7 0) address)))

      ;; Wat
      (t (error "Bad memory address: ~X" address)))))


(defun mem-8 (gameboy address)
  (multiple-value-bind (array address) (find-memory gameboy address)
    (if array
      (aref array address)
      0)))

(defun mem-16 (gameboy address)
  (cat (mem-8 gameboy address)
       (mem-8 gameboy (1+ address))))

(defun (setf mem-8) (value gameboy address)
  (multiple-value-bind (array address) (find-memory gameboy address)
    (if array
      (setf (aref array address) value)
      (error "Cannot write to memory address ~X" address)))
  (if (<= #x8000 address (1- #xA000))
    (update-tile-memory gameboy address)))

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
(defun palette-map (gameboy color)
  color)


(deftype tile-cache ()
  '(simple-array (integer 0 3) #.(list (* 3 128) 8 8)))

(defun tile-pixel (tile-cache tile-id row col)
  (aref tile-cache tile-id row col))

(defun (setf tile-pixel) (value tile-cache tile-id row col)
  (setf (aref tile-cache tile-id row col) value))

(defun make-tile-cache ()
  (make-array (list (* 3 128) 8 8)
    :element-type '(integer 0 3)
    :adjustable nil
    :fill-pointer nil))

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
                         (aref ram (set-bit 0 offset 1))
                         (aref ram (set-bit 0 offset 0))))))


(defun scanline (gameboy)
  (with-gpu ((gb-gpu gameboy))
    (gameboy.gui::set-debug gui `(line ,line))
    (labels ((tile-id (byte)
               (if (zerop background-map)
                 byte
                 (+ 256 (unsigned-to-signed-8 byte))))
             (background-map-ref (x)
               ;; Take a screen X coordinate and retrieve the tile id from the
               ;; appropriate background map.
               (tile-id (aref ram (+ (if (zerop background-map) #x1800 #x1c00)
                                     (-<> scroll-y
                                       (+ line <>)
                                       (chop-8 <>)
                                       (ash <> -3)
                                       (* 2 32 <>))
                                     (mod (+ scroll-x x) 8))))))
      (iterate
        (with g = gui)
        (with y = line)
        (with cache = tile-cache)
        (with tile-row = (mod (+ y scroll-y) 8))
        (with sx = scroll-x)

        (for x :from 0 :below 160)
        (for full-x = (+ x sx))
        (for color = (tile-pixel cache (background-map-ref full-x)
                                 tile-row (mod full-x 8)))
        (gameboy.gui::blit-pixel g x y (palette-map gameboy color)))))
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
           (setf clock 0)
           (incf line)
           (if (= line 143)
             (progn (gameboy.gui::refresh-screen gui)
                    (setf mode 1))
             (setf mode 2))))
      ;; VBlank
      (1 (when (>= clock 456)
           (setf clock 0)
           (incf line)
           (when (> line 153)
             (setf mode 2 line 0))))
      ;; Scan (OAM)
      (2 (when (>= clock 80)
           ;; When the OAM portion of the scanline is done, just flip the mode.
           (setf clock 0 mode 3)))
      ;; Scan (VRAM)
      (3 (when (>= clock 172)
           ;; When the VRAM portion of the scanline is done, flip the mode and
           ;; also blit the scanline into the framebuffer.
           (setf clock 0 mode 0)
           (scanline gameboy))))))


;;;; Opcodes ------------------------------------------------------------------
(deftype opcode-array ()
  '(simple-array t (256)))


(defun unimplemented-opcode (code gameboy)
  (declare (ignore gameboy))
  (error "Unimplemented opcode #x~2,'0X!" code))

(defparameter *unimplemented-functions*
  (iterate (for code :from 0 :below 256)
           (collect (curry #'unimplemented-opcode code))))


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
    :initial-contents *unimplemented-functions*
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

(define-opcode ldh-r/a<mem/c ()                        ; LDH A, (C)
  (setf a (mem-8 gameboy (+ #xff00 c)))
  (increment-clock gameboy 2))

(define-opcode ldh-mem/c<r/a ()                        ; LDH (C), A
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
      (set-flag gameboy :zero <>
                :subtract nil :half-carry nil :carry nil))
    (increment-clock gameboy 2)))

(define-opcode swap-mem/hl ()                          ; SWAP (HL)
  (-<> hl
    (mem-8 gameboy <>)
    (swap-nibbles <>)
    (setf (mem-8 gameboy hl) <>)
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

(define-opcode add-r/sp<i ()                           ; ADD SP, i
  ; todo this hole thing is fucked, especially the flags, fix later
  (incf-16 sp (unsigned-to-signed-8 (mem-8 gameboy pc)))
  (set-flag gameboy
            :zero nil ; lol what gameboy?
            :subtract nil
            :half-carry nil ; todo
            :carry nil) ; todo
  (increment-pc gameboy)
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
  (increment-clock gameboy 1))

(define-opcode halt ()                                 ; HALT
  (setf halt 1)
  (increment-clock gameboy 1))


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
  (increment-clock gameboy 1))

(define-opcode ccf ()                                  ; CCF
  (set-flag gameboy
            :subtract nil
            :half-carry nil
            :carry (flip flag-carry))
  (increment-clock gameboy 1))

(define-opcode scf ()                                  ; SCF
  (set-flag gameboy
            :subtract nil
            :half-carry nil
            :carry 1)
  (increment-clock gameboy 1))


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
             (increment-clock gameboy 3))
      (increment-clock gameboy 2))))

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
    ; (#x7C )
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

    ; (#xE0 )
    ; (#xE1 )
    ; (#xE2 )
    ; (#xE3 )
    ; (#xE4 )
    ; (#xE5 )
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

(defparameter *extended-list*
  '((#x00 )
    (#x01 )
    (#x02 )
    (#x03 )
    (#x04 )
    (#x05 )
    (#x06 )
    (#x07 )
    (#x08 )
    (#x09 )
    (#x0A )
    (#x0B )
    (#x0C )
    (#x0D )
    (#x0E )
    (#x0F )
    (#x10 )
    (#x11 )
    (#x12 )
    (#x13 )
    (#x14 )
    (#x15 )
    (#x16 )
    (#x17 )
    (#x18 )
    (#x19 )
    (#x1A )
    (#x1B )
    (#x1C )
    (#x1D )
    (#x1E )
    (#x1F )
    (#x20 )
    (#x21 )
    (#x22 )
    (#x23 )
    (#x24 )
    (#x25 )
    (#x26 )
    (#x27 )
    (#x28 )
    (#x29 )
    (#x2A )
    (#x2B )
    (#x2C )
    (#x2D )
    (#x2E )
    (#x2F )
    (#x30 )
    (#x31 )
    (#x32 )
    (#x33 )
    (#x34 )
    (#x35 )
    (#x36 )
    (#x37 )
    (#x38 )
    (#x39 )
    (#x3A )
    (#x3B )
    (#x3C )
    (#x3D )
    (#x3E )
    (#x3F )
    (#x40 )
    (#x41 )
    (#x42 )
    (#x43 )
    (#x44 )
    (#x45 )
    (#x46 )
    (#x47 )
    (#x48 )
    (#x49 )
    (#x4A )
    (#x4B )
    (#x4C )
    (#x4D )
    (#x4E )
    (#x4F )
    (#x50 )
    (#x51 )
    (#x52 )
    (#x53 )
    (#x54 )
    (#x55 )
    (#x56 )
    (#x57 )
    (#x58 )
    (#x59 )
    (#x5A )
    (#x5B )
    (#x5C )
    (#x5D )
    (#x5E )
    (#x5F )
    (#x60 )
    (#x61 )
    (#x62 )
    (#x63 )
    (#x64 )
    (#x65 )
    (#x66 )
    (#x67 )
    (#x68 )
    (#x69 )
    (#x6A )
    (#x6B )
    (#x6C )
    (#x6D )
    (#x6E )
    (#x6F )
    (#x70 )
    (#x71 )
    (#x72 )
    (#x73 )
    (#x74 )
    (#x75 )
    (#x76 )
    (#x77 )
    (#x78 )
    (#x79 )
    (#x7A )
    (#x7B )
    (#x7C )
    (#x7D )
    (#x7E )
    (#x7F )
    (#x80 )
    (#x81 )
    (#x82 )
    (#x83 )
    (#x84 )
    (#x85 )
    (#x86 )
    (#x87 )
    (#x88 )
    (#x89 )
    (#x8A )
    (#x8B )
    (#x8C )
    (#x8D )
    (#x8E )
    (#x8F )
    (#x90 )
    (#x91 )
    (#x92 )
    (#x93 )
    (#x94 )
    (#x95 )
    (#x96 )
    (#x97 )
    (#x98 )
    (#x99 )
    (#x9A )
    (#x9B )
    (#x9C )
    (#x9D )
    (#x9E )
    (#x9F )
    (#xA0 )
    (#xA1 )
    (#xA2 )
    (#xA3 )
    (#xA4 )
    (#xA5 )
    (#xA6 )
    (#xA7 )
    (#xA8 )
    (#xA9 )
    (#xAA )
    (#xAB )
    (#xAC )
    (#xAD )
    (#xAE )
    (#xAF )
    (#xB0 )
    (#xB1 )
    (#xB2 )
    (#xB3 )
    (#xB4 )
    (#xB5 )
    (#xB6 )
    (#xB7 )
    (#xB8 )
    (#xB9 )
    (#xBA )
    (#xBB )
    (#xBC )
    (#xBD )
    (#xBE )
    (#xBF )
    (#xC0 )
    (#xC1 )
    (#xC2 )
    (#xC3 )
    (#xC4 )
    (#xC5 )
    (#xC6 )
    (#xC7 )
    (#xC8 )
    (#xC9 )
    (#xCA )
    (#xCB )
    (#xCC )
    (#xCD )
    (#xCE )
    (#xCF )
    (#xD0 )
    (#xD1 )
    (#xD2 )
    (#xD3 )
    (#xD4 )
    (#xD5 )
    (#xD6 )
    (#xD7 )
    (#xD8 )
    (#xD9 )
    (#xDA )
    (#xDB )
    (#xDC )
    (#xDD )
    (#xDE )
    (#xDF )
    (#xE0 )
    (#xE1 )
    (#xE2 )
    (#xE3 )
    (#xE4 )
    (#xE5 )
    (#xE6 )
    (#xE7 )
    (#xE8 )
    (#xE9 )
    (#xEA )
    (#xEB )
    (#xEC )
    (#xED )
    (#xEE )
    (#xEF )
    (#xF0 )
    (#xF1 )
    (#xF2 )
    (#xF3 )
    (#xF4 )
    (#xF5 )
    (#xF6 )
    (#xF7 )
    (#xF8 )
    (#xF9 )
    (#xFA )
    (#xFB )
    (#xFC )
    (#xFD )
    (#xFE )
    (#xFF )
    ))


(defun load-opcode-tables ()
  (iterate
    (for (list array) :in `((,*opcodes-list* ,*opcodes*)
                            #+notyet (,*extended-list* ,*extended-opcodes*)))
    (iterate (for (code name) :in *opcodes-list*)
             (setf (aref array code)
                   (symbol-function (symb 'op- name))))))


;;;; VM -----------------------------------------------------------------------
(defun reset (gameboy)
  ;; todo: zero out mmu/gpu arrays
  (with-gameboy (gameboy)
    (setf a 0 b 0 c 0 d 0 e 0 h 0 l 0 f 0 sp 0 pc 0 clock 0)))


(defparameter *running* t)
(defparameter *gb* nil)


(defun run (gameboy)
  (setf *running* t
        *gb* gameboy)
  (bt:make-thread
    (lambda ()
      (with-gameboy (gameboy)
        (iterate
          (while *running*)
          ; (for op = (aref *opcodes* (mem-8 gameboy pc)))
          ; todo does the chopping have to happen AFTER the funcall like in the JS?
          ; (incf-16 pc)
          ; (funcall op gameboy)
          ; (incf clock clock-increment)
          (sleep 0.0001)
          (step-gpu gameboy 1)))))
  (->> gameboy gb-gpu gpu-gui gameboy.gui::run-qt-gui))

(defun start ()
  (run (make-gameboy)))


;;;; TODO ---------------------------------------------------------------------
;;; * Automatically call opcodes with their operands
;;; * RETI, RST* and other interrupty stuff
;;; * STOP opcode
;;; * DI/EI interrupt stuff
