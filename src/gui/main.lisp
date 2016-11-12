(in-package :gameboy.gui)
(named-readtables:in-readtable :qtools)


(defparameter *current* nil)


(deftype screen-data-array ()
  '(simple-array (unsigned-byte 8) (#.(* 144 160))))

(defun make-screen-buffer ()
  (make-array (* 144 160)
    :element-type '(unsigned-byte 8)
    :adjustable nil
    :fill-pointer nil))

(defmacro sref (arr x y)
  `(aref ,arr (+ (* 160 ,y) ,x)))


(defstruct gui
  (data-buffer (make-screen-buffer) :type screen-data-array)
  (data-screen (make-screen-buffer) :type screen-data-array)
  (debug nil)
  gameboy
  screen)


(defun run-gui (g gameboy)
  (setf (gui-gameboy g) gameboy)
  (with-main-window
    (window (make-instance
              'gameboy.gui.screen::screen
              :gui g))))


(defun blit-pixel (gui x y value)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type gui gui)
           (type (integer 0 (160)) x)
           (type (integer 0 (144)) y)
           (type (integer 0 3) value))
  (setf (sref (gui-data-buffer gui) x y)
        (case value
          (0 255)
          (1 180)
          (2 90)
          (3 0)
          (t 255)))
  nil)

(defun refresh-screen (gui)
  (rotatef (gui-data-screen gui)
           (gui-data-buffer gui)))

(defun set-debug (gui object)
  (setf (gui-debug gui) object))

(defun keys-down (gui)
  (gameboy.gui.screen::screen-keys (gui-screen gui)))


