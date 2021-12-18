;;;; clim-snake.lisp

(in-package #:clim-snake)

;;; "clim-snake" goes here. Hacks and glory await!

(defparameter *board-width* 40)
(defparameter *board-height* 40)
(defparameter *y-offset* 20)
(defparameter *sleep-time* 0.1)

;;; Generic functions

(defgeneric snake-head (snake))
(defgeneric (setf snake-head) (newvalue snake))
(defgeneric collision? (snake))
(defgeneric collision-self (snake &optional (index)))
(defgeneric collision-wall (snake))
(defgeneric advance-snake (snake))
(defgeneric snake-next-pos (snake))
(defgeneric set-dir (snake dir))
(defgeneric apple-eaten? (snake apple))

;;; The below interact with the objects and the GUI
(defgeneric draw-snake (snake pane))
(defgeneric draw-apple (apple pane))

(defstruct pos
  (x 20 :type integer)
  (y 20 :type integer))

;;; Snake class
(defclass snake ()
  ((cells :initform (make-array
		     1 :initial-contents (list (make-pos))
		     :adjustable t :fill-pointer t)
	  :accessor snake-cells)
   (direction :initform 'up :accessor snake-dir)
   (state :initform 'alive :accessor snake-state)
   (growth-points :initform 0 :accessor snake-growth-points)))
   
(defun make-snake ()
  (make-instance 'snake))

(defmethod set-dir ((s snake) dir)
  (setf (snake-dir s) dir))

(defmethod snake-head ((s snake))
  (aref (snake-cells s) 0))

(defmethod (setf snake-head) (newvalue (s snake))
  (setf (aref (snake-cells s) 0) newvalue))

(defmethod snake-next-pos ((s snake))
  (let* ((d (snake-dir s))
	 (p (snake-head s))
	 (x (pos-x p))
	 (y (pos-y p)))
    (case d
      (up (decf y))
      (down (incf y))
      (left (decf x))
      (right (incf x)))
    (make-pos :x x :y y)))

(defmethod advance-snake ((s snake))
  (let ((snake-len (length (snake-cells s))))
    (when (> (snake-growth-points s) 0)
      (vector-push-extend (make-pos) (snake-cells s) (snake-growth-points s))
      (incf snake-len)
      (decf (snake-growth-points s)))
    (loop for i from (- snake-len 1) downto 1
       do (setf (aref (snake-cells s) i) (aref (snake-cells s) (- i 1))))
    (setf (snake-head s) (snake-next-pos s)))
  (when (collision? s) (setf (snake-state s) 'dead)))

(defmethod collision? ((s snake))
  (if (or (collision-self s) (collision-wall s))
      t
      nil))

(defmethod collision-self ((s snake) &optional (index 1))
  (cond
    ((eql index (length (snake-cells s))) nil)
    ((equalp (snake-head s) (aref (snake-cells s) index)) t)
    (t (collision-self s (+ index 1)))))

(defmethod collision-wall ((s snake))
  (let* ((p (snake-head s))
	 (x (pos-x p))
	 (y (pos-y p)))
    (cond
      ((< x 0) t)
      ((< y 0) t)
      ((>= x *board-width*) t)
      ((>= y *board-height*) t)
      (t nil))))

;;; Apple code

(defclass apple ()
  ((place :initform (make-pos :x (random *board-width*)
			      :y (random *board-height*))
	  :accessor apple-pos)))

(defun make-apple ()
  (make-instance 'apple))

(defmethod apple-eaten? ((s snake) (a apple))
  (when (equalp (snake-head s) (apple-pos a))
    t))

;;; GUI

(defclass board-pane (clim-stream-pane)
  ())

(define-application-frame snake-app ()
  ((snake :initform nil :accessor app-snake)
   (apple :initform nil :accessor app-apple)
   (score :initform 0 :accessor app-score))
  (:menu-bar snake-menu-bar)
  (:panes
   (app (make-pane 'board-pane
		   :display-time t
		   :display-function 'display-app
		   :height (+ (* *board-height* 10) *y-offset*)
		   :width (* *board-width* 10))))
  (:layouts
   (default
       app)))

(make-command-table 'snake-menu-bar
		    :errorp nil
		    :menu '(("Game" :menu snake-game-menu)))

(make-command-table 'snake-game-menu
		    :errorp nil
		    :menu '(("New Game" :command com-new)
			    ("Quit" :command com-quit)))

(defun get-app-pane (&optional (frame *application-frame*))
  (find-pane-named frame 'app))

(defun plot (pane x y &optional (color +black+))
  "Plot a 10x10 'pixel' in the application pane"
  (draw-rectangle* pane (* x 10) (+ (* y 10) *y-offset*)
		   (+ (* x 10) 10) (+ (* y 10) 10 *y-offset*)
		   :ink color))

(defun plotc (pane x y &optional (color +black+))
  "Plot a circle in the application pane"
  (draw-circle* pane (+ (* x 10) 5) (+ (* y 10) 5 *y-offset*) 5
		:ink color))

;;; GUI + Objects

(defmethod draw-snake ((s snake) pane)
  (map nil #'(lambda (p)
	       (plot pane (pos-x p) (pos-y p) +yellow+))
       (snake-cells s)))

(defmethod draw-apple ((a apple) pane)
  (plotc pane (pos-x (apple-pos a)) (pos-y (apple-pos a)) +red+))

(defun draw-board (pane snake apple score)
  (cond ((eq (snake-state snake) 'dead)
	 (window-clear pane)
	 (draw-text* pane "Your snake died!"
		     (- (* *board-width* 5) 40)
		     (- (* *board-height* 5) 6))
	 (draw-text* pane (format nil "Score: ~d" score)
		     (- (* *board-width* 5) 40)
		     (+ (* *board-height* 5) 6)))
	(t
	 (draw-rectangle* pane 0 0 (* *board-width* 10) *y-offset*
			  :ink +white+)
	 (draw-rectangle* pane
			  0 *y-offset*
			  (* *board-width* 10)
			  (+ (* *board-height* 10) *y-offset*) :ink +blue+)
	 (draw-text* pane (format nil "Score ~d" score) 0 10)
	 (draw-apple apple pane)
	 (draw-snake snake pane))))

;;; Event handling

(defclass refresh-event (window-manager-event) ())

(defmethod handle-event ((frame snake-app) (event refresh-event))
  (draw-board (get-app-pane frame) (app-snake frame)
	      (app-apple frame) (app-score frame)))

(defmethod handle-event ((pane board-pane) (event key-press-event))
  (let ((s (app-snake *application-frame*)))
    (unless (null s)
      (case (keyboard-event-key-name event)
	(:up (set-dir s 'up))
	(:down (set-dir s 'down))
	(:left (set-dir s 'left))
	(:right (set-dir s 'right)))))
  (call-next-method))

(defun display-app (frame stream)
  (declare (ignore frame stream))
  (unless (null (app-snake *application-frame*))
    (draw-board (get-app-pane)
		(app-snake *application-frame*)
		(app-apple *application-frame*)
		(app-score *application-frame*))))

;;; Game loop

(defun play-snake (frame)
  (queue-event (frame-top-level-sheet frame)
	       (make-instance 'refresh-event
			      :sheet frame))
  (unwind-protect
       (loop while (eq (snake-state (app-snake frame)) 'alive)
	  do
	    (progn
	      (sleep *sleep-time*)
	      (advance-snake (app-snake frame))
	      (when (apple-eaten? (app-snake frame) (app-apple frame))
		(incf (app-score frame) 100)
		(setf (app-apple frame) (make-apple))
		(incf (snake-growth-points (app-snake frame)) 5))
	      (queue-event (frame-top-level-sheet frame)
			   (make-instance 'refresh-event
					  :sheet frame))))
    (setf (snake-state (app-snake frame)) 'dead)))

(define-snake-app-command (com-new :name t)
    ()
  (let ((frame *application-frame*))
    (when (or (null (app-snake frame))
	      (eq (snake-state (app-snake frame)) 'dead))
      (clim-sys:make-process
       #'(lambda ()
	   (setf (app-snake frame) (make-snake))
	   (setf (app-apple frame) (make-apple))
	   (setf (app-score frame) 0)
	   (play-snake frame))))))

(define-snake-app-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'snake-app)))
