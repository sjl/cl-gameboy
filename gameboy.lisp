(in-package :gameboy)


(setf *print-length* 24)
(declaim (optimize (speed 3) (safety 0) (debug 0)))
; (declaim (optimize (speed 1) (safety 3) (debug 3)))


;;;; Types & Constants --------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int16 () '(unsigned-byte 16))
(deftype sint8 () '(signed-byte 8))
(deftype sint16 () '(signed-byte 16))
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

(defmacro macro-map ((lambda-list items) &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))


;;;; Bit Fuckery --------------------------------------------------------------
(declaim (inline to-bit set-bit
                 low-nibble high-nibble
                 cat cat-nibbles
                 swap-nibbles
                 chop-4 chop-8 chop-12 chop-16
                 unsigned-to-signed-8
                 +_8 -_8
                 +_16))

(declaim (ftype (function ((integer 0 (16)) int16 bit) int16)
                set-bit))

(defun to-bit (value)
  (if value 1 0))

(defun set-bit (position integer value)
  (dpb value (byte 1 position) integer))


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


(defun cat (low-order high-order)
  (dpb high-order (byte 8 8) low-order))

(defun cat-nibbles (low-order high-order)
  (dpb high-order (byte 4 4) low-order))


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


(defun swap-nibbles (byte)
  (cat-nibbles (ldb (byte 4 4) byte)
               (ldb (byte 4 0) byte)))


(declaim (ftype (function (int8 int8 &optional bit)
                          (values int8 boolean boolean boolean boolean &optional))
                +_8 -_8)
         (ftype (function (int16 int16 &optional bit)
                          (values int16 boolean boolean boolean boolean &optional))
                +_16))

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


;;;; Data ---------------------------------------------------------------------
(defstruct mmu
  (in-bios t :type boolean)
  (bios (make-bios) :type memory-array :read-only t)
  (rom (make-mem (k 16)) :type memory-array :read-only t)
  (working-ram (make-mem (k 8)) :type memory-array :read-only t)
  (external-ram (make-mem (k 8)) :type memory-array :read-only t)
  (zero-page-ram (make-mem 128) :type memory-array :read-only t))

(defstruct gpu
  (renderer #'identity :type function)
  (framebuffer (make-mem (* 160 144)))
  (mode 0 :type (integer 0 3))
  (clock 0 :type fixnum) ; fuck it, close enough
  (line 0 :type (integer 0 (144))))

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
  (gpu (make-gpu) :type gpu))


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
        (cat (,acc-lo gameboy) (,acc-hi gameboy)))

      (defun (setf ,name) (value gameboy)
        (setf (,acc-lo gameboy) (ldb (byte 8 0) value)
              (,acc-hi gameboy) (ldb (byte 8 8) value))
        value))))


(define-with-macro (mmu)
  in-bios bios rom working-ram external-ram zero-page-ram)

(define-with-macro (gpu)
  renderer framebuffer mode clock line)

(define-with-macro (gameboy :conc-name gb)
  a b c d e h l f pc sp clock clock-increment af bc de hl
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
        (ldb (byte 1 ,position)
             (gb-f gameboy))))))

(define-flag zero 7)
(define-flag subtract 6)
(define-flag half-carry 5)
(define-flag carry 4)

(declaim (ftype (function
                  (gameboy &key (:zero t) (:subtract t) (:half-carry t) (:carry t))
                  int8)
                set-flag))

(defun set-flag (gameboy &key
                 (zero (gb-flag-zero gameboy))
                 (subtract (gb-flag-subtract gameboy))
                 (half-carry (gb-flag-half-carry gameboy))
                 (carry (gb-flag-carry gameboy)))
  (setf (gb-f gameboy)
        (-<> 0
          (set-bit 7 <> (to-bit zero))
          (set-bit 6 <> (to-bit subtract))
          (set-bit 5 <> (to-bit half-carry))
          (set-bit 4 <> (to-bit carry)))))


;;;; More Utils ---------------------------------------------------------------
(declaim (inline increment-clock increment-pc))

(defun increment-clock (gameboy &optional (machine-cycles 1))
  (with-gameboy (gameboy)
    (setf clock-increment machine-cycles)))

(defun increment-pc (gameboy &optional (increment 1))
  (incf-16 (gb-pc gameboy) increment))


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
      (error "Cannot write to memory address ~X" address))))

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
(defun blit-scanline (gpu)
  nil)

(defun step-gpu (gpu cycles)
  "Step the GPU.

  `cycles` should be given in CPU cycles.

  "
  (with-gpu (gpu)
    (incf clock cycles)
    (case mode
      ;; HBlank
      (0 (when (>= clock 204)
           ;; Once HBlank is finished, increment the line and flip back to OAM
           ;; mode.  Unless this was the LAST line, then render the frame to the
           ;; physical screen and go into VBlank instead.
           (setf clock 0)
           (if (= line 143)
             (progn (funcall renderer gpu)
                    (setf mode 1))
             (incf line))))
      ;; VBlank
      (1 (when (>= clock 4560)
           ;; todo: can we do away with the extra bullshit lines here or is this
           ;; gonna cause problems?
           (setf line 0 mode 2)))
      ;; Scan (OAM)
      (2 (when (>= clock 80)
           ;; When the OAM portion of the scanline is done, just flip the mode.
           (setf clock 0 mode 3)))
      ;; Scan (VRAM)
      (3 (when (>= clock 80)
           ;; When the VRAM portion of the scanline is done, flip the mode and
           ;; also blit the scanline into the framebuffer.
           (setf clock 0 mode 0)
           (blit-scanline gpu))))))


;;;; Opcodes ------------------------------------------------------------------
(defmacro define-opcode (name &rest body)
  `(progn
    (declaim (ftype (function (gameboy) gameboy) ,name))
    (defun ,name (gameboy)
      (with-gameboy (gameboy)
        ,@body)
      gameboy)))


;;; Load & Store
(macro-map (register (b c d e h l))                    ; LD r, i
  `(define-opcode ,(symb 'ld-r/ register '<i)
    (setf ,register (mem-8 gameboy pc))
    (increment-pc gameboy)
    (increment-clock gameboy 2)))

(macro-map ((destination source)                       ; LD r, r
            #.(map-product #'list
                           '(a b c d e h l)
                           '(a b c d e h l)))
  `(define-opcode ,(symb 'ld-r/ destination '<r/ source)
    (setf ,destination ,source)
    (increment-clock gameboy)))

(macro-map (register (a b c d e h l))                  ; LD r, (HL)
  `(define-opcode ,(symb 'ld-r/ register '<mem/hl)
    (setf ,register (mem-8 gameboy hl))
    (increment-clock gameboy 2)))

(macro-map (register (a b c d e h l))                  ; LD (HL), r
  `(define-opcode ,(symb 'ld-mem/hl<r/ register)
    (setf (mem-8 gameboy hl) ,register)
    (increment-clock gameboy 2)))

(define-opcode ld-mem/hl<i                             ; LD (HL), i
  (setf (mem-8 gameboy hl) (mem-8 gameboy pc))
  (increment-pc gameboy)
  (increment-clock gameboy 3))

(macro-map (register (bc de))                          ; LD (BC/DE), A
  `(define-opcode ,(symb 'ld-mem/ register '<r/a)
    (setf (mem-8 gameboy ,register) a)
    (increment-clock gameboy 2)))

(define-opcode ld-mem/i<r/a                            ; LD (i), A
  (setf (mem-8 gameboy (mem-16 gameboy pc)) a)
  (increment-pc gameboy 2)
  (increment-clock gameboy 4))

(macro-map (register (bc de))                          ; LD A, (BC/DE)
  `(define-opcode ,(symb 'ld-r/a<mem/ register)
    (setf a (mem-8 gameboy ,register))
    (increment-clock gameboy 2)))

(define-opcode ld-r/a<mem/i                            ; LD A, (i)
  (setf a (mem-8 gameboy (mem-16 gameboy pc)))
  (increment-pc gameboy 2)
  (increment-clock gameboy 4))

(macro-map (register (bc de hl))                       ; LD BC/DE/HL, i
  `(define-opcode ,(symb 'ld-r/ register '<i)
    ;; todo check this
    (setf ,register (mem-16 gameboy pc))
    (increment-pc gameboy 2)
    (increment-clock gameboy 3)))

(define-opcode ld-r/sp<i                               ; LD SP, i
  (setf sp (mem-16 gameboy pc))
  (increment-pc gameboy 2)
  (increment-clock gameboy 3))

(define-opcode ld-mem/i<r/sp                           ; LD (i), SP
  (setf (mem-16 gameboy (mem-16 gameboy pc)) sp)
  (increment-pc gameboy 2)
  (increment-clock gameboy 5))

(define-opcode ldi-mem/hl<a                            ; LDI (HL), A
  (setf (mem-8 gameboy hl) a)
  (incf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldi-a<mem/hl                            ; LDI A, (HL)
  (setf a (mem-8 gameboy hl))
  (incf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldd-mem/hl<a                            ; LDD (HL), A
  (setf (mem-8 gameboy hl) a)
  (decf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldd-a<mem/hl                            ; LDD A, (HL)
  (setf a (mem-8 gameboy hl))
  (decf-16 hl)
  (increment-clock gameboy 2))

(define-opcode ldh-a<mem/i                             ; LDH A, (i)
  (setf a (mem-8 gameboy (+ #xff00 (mem-8 gameboy pc))))
  (increment-pc gameboy)
  (increment-clock gameboy 3))

(define-opcode ldh-mem/i<a                             ; LDH (i), A
  (setf (mem-8 gameboy (+ #xff00 (mem-8 gameboy pc))) a)
  (increment-pc gameboy)
  (increment-clock gameboy 3))

(define-opcode ldh-a<mem/c                             ; LDH A, (C)
  (setf a (mem-8 gameboy (+ #xff00 c)))
  (increment-clock gameboy 2))

(define-opcode ldh-mem/c<a                             ; LDH (C), A
  (setf (mem-8 gameboy (+ #xff00 c)) a)
  (increment-clock gameboy 2))

(define-opcode ld-r/hl<mem/sp+i                        ; LD HL, SP+i
  (let ((offset (unsigned-to-signed-8 (mem-8 gameboy pc))))
    (with-chopped-16 (full trunc (+ sp offset))
      (setf hl trunc)
      (set-flag gameboy
                :zero nil
                :subtract nil
                :half-carry nil ; todo
                :carry (> full 255))))
  (increment-pc gameboy)
  (increment-clock gameboy 3))


;;; Swap
(macro-map (register (a b c d e h l))                  ; SWAP r
  `(define-opcode ,(symb 'swap-r/ register)
    (-<> ,register
      (swap-nibbles <>)
      (setf ,register <>)
      (set-flag gameboy :zero <>
                :subtract nil :half-carry nil :carry nil))
    (increment-clock gameboy 2)))

(define-opcode swap-mem/hl                             ; SWAP (HL)
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
  (((name source &optional (clock 1) (pc nil)) with-carry)
   #.(map-product #'list '((r/a    a)
                           (r/b    b)
                           (r/c    c)
                           (r/d    d)
                           (r/e    e)
                           (r/h    h)
                           (r/l    l)
                           (mem/hl (mem-8 gameboy hl) 2)
                           (i      (mem-8 gameboy pc) 2 t))
                  '(nil t)))
  `(define-opcode ,(symb (if with-carry 'adc 'add) '-r/a< name)
    (operate-into% +_8 a ,source
                   ,(if with-carry 'flag-carry 0))
    ,@(when pc `((increment-pc gameboy)))
    (increment-clock gameboy ,clock)))


(macro-map (register (bc de hl sp))                    ; ADD HL, BC/DE/HL/SP
  `(define-opcode ,(symb 'add-r/hl<r/ register)
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

(define-opcode add-r/sp<i                              ; ADD SP, i
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
  (((name source &optional (clock 1) (pc nil)) with-carry)
   #.(map-product #'list '((r/a    a)
                           (r/b    b)
                           (r/c    c)
                           (r/d    d)
                           (r/e    e)
                           (r/h    h)
                           (r/l    l)
                           (mem/hl (mem-8 gameboy hl) 2)
                           (i      (mem-8 gameboy pc) 2 t))
                  '(nil t)))
  `(define-opcode ,(symb (if with-carry 'sbc 'sub) '-r/a< name)
    (operate-into% -_8 a ,source
                   ,(if with-carry 'flag-carry 0))
    ,@(when pc `((increment-pc gameboy)))
    (increment-clock gameboy ,clock)))


(macro-map                                             ; CP A, *
    ((name source &optional (clock 1) (pc nil))
     ((r/a    a)
      (r/b    b)
      (r/c    c)
      (r/d    d)
      (r/e    e)
      (r/h    h)
      (r/l    l)
      (mem/hl (mem-8 gameboy hl) 2)
      (i      (mem-8 gameboy pc) 2 t)))
  `(define-opcode ,(symb 'cp-r/a= name)
    (operate-into% -_8 a ,source 0 :ignore-result t)
    ,@(when pc `((increment-pc gameboy)))
    (increment-clock gameboy ,clock)))


(macro-map                                             ; INC/DEC *
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
  `(define-opcode ,(symb op-name '-r/a= name)
    (multiple-value-bind (result zero subtract half-carry carry)
        (,operation ,source 1)
      (declare (ignore carry)) ;; carry is unaffected for some reason...
      (setf ,source result)
      (set-flag gameboy
                :zero zero
                :subtract subtract
                :half-carry half-carry))
    (increment-clock gameboy ,clock)))


;;; Miscellaneous
(define-opcode daa
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


;;; Logic
(macro-map                                             ; AND/OR/XOR A, *
  (((name source &optional (clock 1) (pc nil))
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
                    (i      (mem-8 gameboy pc) 2 t))
                  '((and logand t)
                    (or logior nil)
                    (xor logxor nil))))
  `(define-opcode ,(symb op-name '-r/a< name)
    (-<> ,source
      (,operation a <>)
      (setf a <>)
      (set-flag gameboy
                :zero <>
                :subtract nil
                :half-carry ,hc
                :carry nil))
    ,@(when pc `((increment-pc gameboy)))
    (increment-clock gameboy ,clock)))


;;;; VM -----------------------------------------------------------------------
(defun reset (gameboy)
  ;; todo: zero out mmu/gpu arrays
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
      (for op = (aref *opcodes* (mem-8 gameboy pc)))
      ; todo does the chopping have to happen AFTER the funcall like in the JS?
      (incf-16 pc)
      (funcall op gameboy)
      (incf clock clock-increment))))










