(in-package :gameboy.gui)
(named-readtables:in-readtable :qtools)


;;;; Config
(defparameter *scale* 5)
(defparameter *width* (* *scale* 160))
(defparameter *height* (* *scale* 144))

(deftype screen-data-array ()
  '(simple-array (unsigned-byte 8) (#.(* 144 160))))


(defmacro sref (arr x y)
  `(aref ,arr (+ (* 160 ,y) ,x)))


;;;; Main Window
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (data :accessor screen-data :initarg :data)
   (gui :accessor screen-gui :initarg :gui)))

(defun die (screen)
  (q+:close screen)
  (setf gameboy::*running* nil))


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
  (setf (q+:fixed-size screen) (values *width* *height*)))

(define-override (screen "initializeGL") ()
  (setf (screen-texture screen) (initialize-texture 256))
  (stop-overriding))


;;;; Timer
(define-subwidget (screen timer) (q+:make-qtimer screen)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (screen update) ()
  (declare (connected timer (timeout)))

  ; (iterate (repeat 2)
  ;          (setf (sref (screen-data screen) (random 160) (random 144))
  ;                (random 256)))

  (if gameboy::*running*
    (q+:repaint screen)
    (die screen)))


;;;; Keyboard
(define-override (screen key-release-event) (ev)
  (cond ((= (q+:key ev) (q+:qt.key_escape))
         (die screen)))
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
                         (screen-data screen))

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
      (when debug
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
  (data (error "Required.") :type screen-data-array)
  (debug nil))

(defun make-qt-gui ()
  (let* ((data (make-array (* 144 160)
                :element-type '(unsigned-byte 8)
                :adjustable nil
                :fill-pointer nil)))
    (check-type data screen-data-array) ; make sure I didn't fuck up the deftype
    (make-qt-gui% :data data)))

(defun run-qt-gui (g)
  (with-main-window
    (window (make-instance 'screen
              :data (qt-gui-data g)
              :gui g))))


(defun blit-pixel (gui x y value)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type qt-gui gui)
           (type (integer 0 (160)) x)
           (type (integer 0 (144)) y)
           (type (integer 0 3) value))
  (setf (sref (qt-gui-data gui) x y)
        (case value
          (0 180)
          (1 120)
          (2 60)
          (3 0)
          (t 255)))
  nil)

(defun refresh-screen (gui)
  (declare (ignore gui)))

(defun set-debug (gui object)
  (setf (qt-gui-debug gui) object))


(defun main ()
  (setf *current* (make-qt-gui))
  (run-qt-gui *current*))


