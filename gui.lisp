(in-package :gameboy.gui)
(named-readtables:in-readtable :qtools)


;;;; Config
(defparameter *scale* 4)
(defparameter *width* (* *scale* 160))
(defparameter *height* (* *scale* 144))

(defmacro sref (arr x y)
  `(aref ,arr ,y ,x))


;;;; Main Window
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (data :accessor screen-data :initarg :data)
   (raw :accessor screen-raw :initarg :raw)))


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

  (iterate (repeat 2)
           (setf (sref (screen-data screen) (random 160) (random 144))
                 (random 256)))

  (q+:repaint screen))


;;;; Keyboard
(define-override (screen key-release-event) (ev)
  (cond ((= (q+:key ev) (q+:qt.key_escape))
         (q+:close screen)))
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
                         (screen-raw screen))

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

    (q+:end-native-painting painter)))


;;;; Main
(defstruct (qt-gui (:constructor make-qt-gui%))
  data raw)

(defun make-qt-gui ()
  (let* ((raw (make-array (* 144 160)
                :element-type '(unsigned-byte 8)
                :adjustable nil
                :fill-pointer nil))
         (data (make-array (list 144 160)
                 :element-type '(unsigned-byte 8)
                 :displaced-to raw)))
    (make-qt-gui% :data data :raw raw)))

(defun run-qt-gui (g)
  (with-main-window
    (window (make-instance 'screen
              :data (qt-gui-data g)
              :raw (qt-gui-raw g)))))


(defun blit-pixel (gui x y value)
  (setf (sref (screen-data (qt-gui-data gui)) x y)
        (ecase value
          (0 255)
          (1 170)
          (2 85)
          (3 0))))

(defun refresh-screen (gui)
  (declare (ignore gui)))


(defun main ()
  (run-qt-gui (make-qt-gui)))


