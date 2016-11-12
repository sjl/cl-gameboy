(in-package :gameboy.gui.tile-viewer)
(named-readtables:in-readtable :qtools)


(defparameter *scale* 2)
(defparameter *rows* 24)
(defparameter *cols* 16)

(defparameter *pixel-width* (* *cols* 8))
(defparameter *pixel-height* (* *rows* 8))

(defparameter *width* (* *scale* *pixel-width*))
(defparameter *height* (* *scale* *pixel-height*))


;;;; Tile Viewer --------------------------------------------------------------
(define-widget tile-viewer (QGLWidget)
  ((texture :accessor tv-texture)
   (timestamp :accessor tv-timestamp :initform -1)
   (gameboy :accessor tv-gameboy :initarg :gameboy)))

(define-initializer (tile-viewer setup)
  (setf (q+:window-title tile-viewer) "Tile Data")
  (setf (q+:fixed-size tile-viewer) (values *width* *height*)))

(define-override (tile-viewer "initializeGL") ()
  (setf (tv-texture tile-viewer) (initialize-texture 256))
  (stop-overriding))

(define-subwidget (tile-viewer timer) (q+:make-qtimer tile-viewer)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/1)))

(define-slot (tile-viewer update) ()
  (declare (connected timer (timeout)))

  (when gameboy::*running*
    (when (> (gameboy::gpu-tile-cache-timestamp
               (gameboy::gb-gpu (tv-gameboy tile-viewer)))
             (tv-timestamp tile-viewer))
      (q+:repaint tile-viewer))))


(define-override (tile-viewer paint-event) (ev)
  (declare (ignore ev))

  (setf (tv-timestamp tile-viewer) (get-internal-real-time))
  (pr "repainting tile viewer...")

  (with-finalizing ((painter (q+:make-qpainter tile-viewer)))
    (q+:begin-native-painting painter)

    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)

    (gl:bind-texture :texture-2d (tv-texture tile-viewer))
    (gl:tex-sub-image-2d :texture-2d 0 0 0 *pixel-width* *pixel-height*
                         :luminance :unsigned-byte
                         (dump-tile-data (tv-gameboy tile-viewer)))

    (gl:with-primitives :quads
      (gl:tex-coord 0 0)
      (gl:vertex 0 0)

      (gl:tex-coord (/ *pixel-width* 256) 0)
      (gl:vertex *width* 0)

      (gl:tex-coord (/ *pixel-width* 256) (/ *pixel-height* 256))
      (gl:vertex *width* *height*)

      (gl:tex-coord 0 (/ *pixel-height* 256))
      (gl:vertex 0 *height*))

    (gl:bind-texture :texture-2d 0)

    (q+:end-native-painting painter)))


(defun flat-idx (x y)
  (+ (* *pixel-width* y) x))

(defun tex-idx (tile-id tile-row tile-col)
  ;; 000011112222...
  ;; 000011112222...
  ;; 000011112222...
  ;; 000011112222...
  ;; ...
  (let* ((row (floor tile-id *cols*))
         (col (mod tile-id *cols*))
         (x (+ (* col 8) tile-col))
         (y (+ (* row 8) tile-row)))
    (flat-idx x y)))

(defun dump-tile-data (gameboy)
  (let ((data (make-array (* 384 8 8) :element-type '(unsigned-byte 8)))
        (tile-cache (gameboy::gpu-tile-cache (gameboy::gb-gpu gameboy))))
    (iterate
      (for tile-id :from 0 :below 384)
      (iterate (for-nested ((row :from 0 :below 8)
                            (col :from 0 :below 8)))
               (setf (aref data (tex-idx tile-id row col))
                     (ecase (gameboy::tile-pixel tile-cache tile-id row col)
                       (0 255)
                       (1 180)
                       (2 90)
                       (3 0)))))
    data))
