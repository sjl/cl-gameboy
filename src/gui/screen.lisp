(in-package :gameboy.gui.screen)
(named-readtables:in-readtable :qtools)

(defparameter *scale* 2)
(defparameter *width* (* *scale* 160))
(defparameter *height* (* *scale* 144))



;;;; Screen -------------------------------------------------------------------
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (gui :accessor screen-gui :initarg :gui)
   (keys :accessor screen-keys :initform '())
   (tile-viewer :accessor screen-tile-viewer)))


(defun die (screen)
  (setf gameboy::*running* nil)
  (when (slot-boundp screen 'tile-viewer)
    (q+:close (screen-tile-viewer screen)))
  (q+:close screen))


(define-initializer (screen setup)
  (setf (q+:window-title screen) "cl-gameboy"
        (q+:fixed-size screen) (values *width* *height*)

        ;; ugly circular deps here...
        (gameboy.gui::gui-screen (screen-gui screen))
        screen

        (screen-tile-viewer screen)
        (make-instance 'gameboy.gui.tile-viewer::tile-viewer
          ;; please just kill me, this whole thing is a rat's nest of pointers
          :gameboy (gameboy.gui::gui-gameboy (screen-gui screen))))
  (q+:show (screen-tile-viewer screen)))

(define-override (screen "initializeGL") ()
  (setf (screen-texture screen) (initialize-texture 256))
  (stop-overriding))


(define-subwidget (screen timer) (q+:make-qtimer screen)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/60)))

(define-slot (screen update) ()
  (declare (connected timer (timeout)))

  (if gameboy::*running*
    (q+:repaint screen)
    (die screen)))


(define-override (screen paint-event) (ev)
  (declare (ignore ev))

  (let ((gui (screen-gui screen)))
    (with-finalizing ((painter (q+:make-qpainter screen)))
      (q+:begin-native-painting painter)

      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit)

      (gl:bind-texture :texture-2d (screen-texture screen))
      (gl:tex-sub-image-2d :texture-2d 0 0 0 160 144 :luminance :unsigned-byte
                           (gameboy.gui::gui-data-screen gui))

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

      (let ((debug (gameboy.gui::gui-debug gui)))
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
              (q+:draw-path painter path))))))))


;;;; Keyboard -----------------------------------------------------------------
(defun pad-key-for (code)
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
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key)))
    (when pad-key
      (pushnew pad-key (screen-keys screen))))
  (stop-overriding))

(define-override (screen key-release-event) (ev)
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key)))
    (if pad-key
      (removef (screen-keys screen) pad-key)
      (cond ((= key (q+:qt.key_escape))
             (die screen))

            ((= key (q+:qt.key_space))
             (zapf gameboy::*paused* (not %)))

            ((= key (q+:qt.key_s))
             (setf gameboy::*step* t)))))
  (stop-overriding))

