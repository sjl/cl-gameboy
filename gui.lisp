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
   (data :accessor screen-data)
   (raw :accessor screen-raw)))


;;;; Init
(defun initialize-texture (size)
  (let* ((handle (gl:gen-texture))
         (raw (make-array (* size size)
                 :element-type '(unsigned-byte 8)
                 :adjustable nil
                 :fill-pointer nil))
         (data (make-array (list size size)
                 :element-type '(unsigned-byte 8)
                 :displaced-to raw)))
    (gl:bind-texture :texture-2d handle)

    (iterate (for-nested ((x :from 0 :below 160)
                          (y :from 0 :below 144)))
             (setf (sref data x y) (truncate (* 255 (/ x 160)))))

    (gl:tex-image-2d :texture-2d 0 :luminance size size 0 :luminance :unsigned-byte raw)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest) ; sharp pixels or gtfo
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:enable :texture-2d)

    (gl:bind-texture :texture-2d 0)

    (values handle data raw)))

(define-initializer (screen setup)
  (setf (q+:window-title screen) "cl-gameboy")
  (setf (q+:fixed-size screen) (values *width* *height*)))

(define-override (screen "initializeGL") ()
  (multiple-value-bind (texture data raw) (initialize-texture 256)
    (setf (screen-texture screen) texture
          (screen-data screen) data
          (screen-raw screen) raw))
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
  ; (cond ((= (q+:key ev) (q+:qt.key_left))
  ;        (decf angle-delta))
  ;       ((= (q+:key ev) (q+:qt.key_right))
  ;        (incf angle-delta)))
  (stop-overriding))


;;;; Redraw
(define-override (screen paint-event) (ev)
  (declare (ignore ev))

  (with-finalizing ((painter (q+:make-qpainter screen)))
    (q+:begin-native-painting painter)

    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)

    (gl:bind-texture :texture-2d (screen-texture screen))
    (gl:tex-sub-image-2d :texture-2d 0 0 0 256 256 :luminance :unsigned-byte
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
(defun main ()
  (with-main-window (window (make-instance 'screen))))
