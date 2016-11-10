(in-package :gameboy.gui)
(named-readtables:in-readtable :qtools)


;;;; Config
(defparameter *scale* 2)
(defparameter *width* (* *scale* 160))
(defparameter *height* (* *scale* 144))

(deftype screen-data-array ()
  '(simple-array (unsigned-byte 8) (#.(* 144 160))))


(defmacro sref (arr x y)
  `(aref ,arr (+ (* 160 ,y) ,x)))


;;;; Main Window
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (gui :accessor screen-gui :initarg :gui)
   (debugger :accessor screen-debugger)))

(defun die (screen)
  (setf gameboy::*running* nil)
  (when (slot-boundp screen 'debugger)
    (q+:close (screen-debugger screen)))
  (q+:close screen))


;;;; Debug Window
(define-widget debugger (QDialog)
  ())

(define-subwidget (debugger pause-button)
  (q+:make-qpushbutton "Pause" debugger))

(define-subwidget (debugger layout) (q+:make-qvboxlayout debugger)
  (q+:add-widget layout pause-button))

(define-slot (debugger pause-pressed) ()
  (declare (connected pause-button (released)))
  (zapf gameboy::*paused* (not %)))


;;;; Init
(defun initialize-texture (size)
  (let* ((handle (gl:gen-texture)))
    (gl:bind-texture :texture-2d handle)

    (gl:tex-image-2d :texture-2d 0 :luminance size size 0 :luminance
                     :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest) ; sharp pixels or gtfo
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:enable :texture-2d)

    (gl:bind-texture :texture-2d 0)

    handle))

(define-initializer (screen setup)
  (setf (q+:window-title screen) "cl-gameboy")
  (setf (q+:fixed-size screen) (values *width* *height*))

  (setf (screen-debugger screen) (make-instance 'debugger))
  (q+:show (screen-debugger screen)))

(define-override (screen "initializeGL") ()
  (setf (screen-texture screen) (initialize-texture 256))
  (stop-overriding))


;;;; Timer
(define-subwidget (screen timer) (q+:make-qtimer screen)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/60)))

(define-slot (screen update) ()
  (declare (connected timer (timeout)))

  (if gameboy::*running*
    (q+:repaint screen)
    (die screen)))


;;;; Keyboard
(defun key (code)
  (cond
    ((= code (q+:qt.key_a)) :a)
    ((= code (q+:qt.key_s)) :b)
    ((= code (q+:qt.key_q)) :start)
    ((= code (q+:qt.key_w)) :select)
    ((= code (q+:qt.key_left)) :left)
    ((= code (q+:qt.key_right)) :right)
    ((= code (q+:qt.key_up)) :up)
    ((= code (q+:qt.key_down)) :down)))

(define-override (screen key-press-event) (ev)
  (let ((gui (screen-gui screen)))
    (case (key (q+:key ev))
      ((:a) (setf (qt-gui-key-a gui) t))
      ((:b) (setf (qt-gui-key-b gui) t))
      ((:start) (setf (qt-gui-key-start gui) t))
      ((:select) (setf (qt-gui-key-select gui) t))
      ((:left) (setf (qt-gui-key-left gui) t))
      ((:right) (setf (qt-gui-key-right gui) t))
      ((:up) (setf (qt-gui-key-up gui) t))
      ((:down) (setf (qt-gui-key-down gui) t))))
  (stop-overriding))

(define-override (screen key-release-event) (ev)
  (let ((gui (screen-gui screen)))
    (case (key (q+:key ev))
      ((:a) (setf (qt-gui-key-a gui) nil))
      ((:b) (setf (qt-gui-key-b gui) nil))
      ((:start) (setf (qt-gui-key-start gui) nil))
      ((:select) (setf (qt-gui-key-select gui) nil))
      ((:left) (setf (qt-gui-key-left gui) nil))
      ((:right) (setf (qt-gui-key-right gui) nil))
      ((:up) (setf (qt-gui-key-up gui) nil))
      ((:down) (setf (qt-gui-key-down gui) nil))
      (t (cond ((= (q+:key ev) (q+:qt.key_escape))
                (die screen))
               ((= (q+:key ev) (q+:qt.key_space))
                (zapf gameboy::*paused* (not %)))
               ((= (q+:key ev) (q+:qt.key_s))
                (setf gameboy::*step* t))))))
  (stop-overriding))


;;;; Redraw
(define-override (screen paint-event) (ev)
  (declare (ignore ev))

  (with-finalizing ((painter (q+:make-qpainter screen)))
    (q+:begin-native-painting painter)

    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)

    (gl:bind-texture :texture-2d (screen-texture screen))
    (gl:tex-sub-image-2d :texture-2d 0 0 0 160 144 :luminance :unsigned-byte
                         (qt-gui-data-screen (screen-gui screen)))

    (gl:with-primitives :quads
      (gl:tex-coord 0 0)
      (gl:vertex 0 0)

      (gl:tex-coord (/ 160 256) 0)
      (gl:vertex *width* 0)

      (gl:tex-coord (/ 160 256) (/ 144 256))
      (gl:vertex *width* *height*)

      (gl:tex-coord 0 (/ 144 256))
      (gl:vertex 0 *height*))

    (gl:bind-texture :texture-2d 0)

    (q+:end-native-painting painter)

    (let ((debug (qt-gui-debug (screen-gui screen))))
      (when (or debug gameboy::*paused*)
        (with-finalizing ((font (q+:make-qfont "Menlo" 40))
                          (border-color (q+:make-qcolor 255 255 255))
                          (fill-color (q+:make-qcolor 0 0 0))
                          (path (q+:make-qpainterpath))
                          (pen (q+:make-qpen)))
          (with-finalizing ((brush (q+:make-qbrush fill-color)))
            (setf (q+:width pen) 1)
            (setf (q+:color pen) border-color)

            (setf (q+:pen painter) pen)
            (setf (q+:brush painter) brush)
            (setf (q+:font painter) font)
            (setf (q+:weight font) (q+:qfont.black))
            (setf (q+:style-hint font) (q+:qfont.type-writer))

            ; (setf (q+:pen painter) (q+:make-qcolor "#ff0000"))
            (q+:add-text path 10 40 font
                         (let ((*package* (find-package :gameboy)))
                           (prin1-to-string debug)))
            (q+:draw-path painter path)))))))


;;;; Main
(defparameter *current* nil)

(defstruct (qt-gui (:constructor make-qt-gui%))
  (data-buffer (error "Required.") :type screen-data-array)
  (data-screen (error "Required.") :type screen-data-array)
  (debug nil)
  gameboy
  key-left
  key-right
  key-up
  key-down
  key-a
  key-b
  key-start
  key-select)

(defun make-qt-gui ()
  (let* ((data-buffer (make-array (* 144 160)
                        :element-type '(unsigned-byte 8)
                        :adjustable nil
                        :fill-pointer nil))
         (data-screen (make-array (* 144 160)
                        :element-type '(unsigned-byte 8)
                        :adjustable nil
                        :fill-pointer nil)))
    (make-qt-gui% :data-buffer data-buffer
                  :data-screen data-screen)))

(defun run-qt-gui (g)
  (with-main-window
    (window (make-instance 'screen :gui g))))


(defun blit-pixel (gui x y value)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type qt-gui gui)
           (type (integer 0 (160)) x)
           (type (integer 0 (144)) y)
           (type (integer 0 3) value))
  (setf (sref (qt-gui-data-buffer gui) x y)
        (case value
          (0 255)
          (1 180)
          (2 90)
          (3 0)
          (t 255)))
  nil)

(defun refresh-screen (gui)
  (rotatef (qt-gui-data-screen gui)
           (qt-gui-data-buffer gui)))

(defun set-debug (gui object)
  (setf (qt-gui-debug gui) object))


(defun main ()
  (setf *current* (make-qt-gui))
  (run-qt-gui *current*))


