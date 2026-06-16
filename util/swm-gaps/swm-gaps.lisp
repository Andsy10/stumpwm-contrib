;;;; swm-gaps.lisp

(in-package #:swm-gaps)

(export '(*inner-gaps-size* *outer-gaps-size* *head-gaps-size* *gaps-on* toggle-gaps toggle-gaps-on toggle-gaps-off))

(defvar *inner-gaps-size* 5)
(defvar *outer-gaps-size* 10)
(defvar *head-gaps-size* 0)
(defvar *gaps-on* nil)

(defun apply-gaps-p (win)
  "Tell if gaps should be applied to this window"
  (and *gaps-on* (not (stumpwm::window-transient-p win)) (not (window-fullscreen win))))

(defun window-edging-p (win direction)
  "Tell if the window is touching the head in the given direction."
  (let* ((frame (stumpwm::window-frame win))
         (head (stumpwm::frame-head (stumpwm:window-group win) frame))
         (offset (nth-value 2 (stumpwm::get-edge frame direction))))
    (ecase direction
      (:top
       (= offset (stumpwm::head-y head)))
      (:bottom
       (= offset (+ (stumpwm::head-y head) (stumpwm::head-height head))))
      (:left
       (= offset (stumpwm::head-x head)))
      (:right
       (= offset (+ (stumpwm::head-x head) (stumpwm::head-width head)))))))

(defun gaps-offsets (win)
  "Return gap offset values for the window. X and Y values are added. WIDTH and
HEIGHT are subtracted."
  (let ((x *inner-gaps-size*)
        (y *inner-gaps-size*)
        (width (* 2 *inner-gaps-size*))
        (height (* 2 *inner-gaps-size*))
        (head (stumpwm::frame-head (stumpwm:window-group win) (stumpwm::window-frame win))))
    (flet ((edge-offset-at-top ()
             (let ((ml (stumpwm::head-mode-line head)))
               (if (and ml (eq (stumpwm::mode-line-position ml) :top)
                        (not (eq (stumpwm::mode-line-mode ml) :hidden)))
                   *inner-gaps-size*
                   (+ *outer-gaps-size* *head-gaps-size*))))
           (edge-offset-at-bottom ()
             (let ((ml (stumpwm::head-mode-line head)))
               (if (and ml (eq (stumpwm::mode-line-position ml) :bottom)
                        (not (eq (stumpwm::mode-line-mode ml) :hidden)))
                   *inner-gaps-size*
                   (+ *outer-gaps-size* *head-gaps-size*)))))
      (if (window-edging-p win :top)
          (setf y (+ y (edge-offset-at-top))
                height (+ height (edge-offset-at-top))))
      (if (window-edging-p win :bottom)
          (setf height (+ height (edge-offset-at-bottom))))
      (if (window-edging-p win :left)
          (setf x (+ x *outer-gaps-size* *head-gaps-size*)
                width (+ width *outer-gaps-size* *head-gaps-size*)))
      (if (window-edging-p win :right)
          (setf width (+ width *outer-gaps-size* *head-gaps-size*)))
      (values x y width height))))

(defun stumpwm::maximize-window (win)
  "Redefined gaps aware maximize function."
  (multiple-value-bind (x y wx wy width height border stick)
      (stumpwm::geometry-hints win)

    (let ((ox 0) (oy 0) (ow 0) (oh 0)
          (frame (stumpwm::window-frame win)))
      (if (apply-gaps-p win)
          (multiple-value-setq (ox oy ow oh) (gaps-offsets win)))

      ;; Only do width or height subtraction if result will be positive,
      ;; otherwise stumpwm will crash. Also, only modify window dimensions
      ;; if needed (i.e. window at least fills frame minus gap).
      (when (and (< ow width)
                 (>= width (- (stumpwm::frame-display-width (window-group win) frame) ow)))
        (setf width (- width ow)))
      (when (and (< oh height)
                 (>= height (- (stumpwm::frame-display-height (window-group win) frame) oh)))
        (setf height (- height oh)))

      (setf x (+ x ox)
            y (+ y oy))

      ;; This is the only place a window's geometry should change
      (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
      (xlib:with-state ((window-parent win))
        ;; FIXME: updating the border doesn't need to be run everytime
        ;; the window is maximized, but only when the border style or
        ;; window type changes. The overhead is probably minimal,
        ;; though.
        (setf (xlib:drawable-x (window-parent win)) x
              (xlib:drawable-y (window-parent win)) y
              (xlib:drawable-border-width (window-parent win)) border)
        ;; the parent window should stick to the size of the window
        ;; unless it isn't being maximized to fill the frame.
        (if (or stick
                (find *window-border-style* '(:tight :none)))
            (setf (xlib:drawable-width (window-parent win)) (window-width win)
                  (xlib:drawable-height (window-parent win)) (window-height win))
            (let ((frame (stumpwm::window-frame win)))
              (setf (xlib:drawable-width (window-parent win)) (- (stumpwm::frame-display-width (window-group win) frame)
                                                                 (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                 ow)
                    (xlib:drawable-height (window-parent win)) (- (stumpwm::frame-display-height (window-group win) frame)
                                                                  (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                  oh))))
        ;; update the "extents"
        (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                              (list wx
                                    (- (xlib:drawable-width (window-parent win)) width wx)
                                    wy
                                    (- (xlib:drawable-height (window-parent win)) height wy))
                              :cardinal 32))
      (stumpwm::update-decoration win)
      (update-configuration win))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (mapcar #'stumpwm::maximize-window
          (stumpwm::only-tile-windows (stumpwm:screen-windows (current-screen)))))

(defun stumpwm::resize-mode-line (mode-line)
  "Redefined gaps aware resize-mode-line function."
  (when (eq (stumpwm::mode-line-mode mode-line) :stump)
    (setf (xlib:drawable-height (stumpwm::mode-line-window mode-line))
          (+ (* 2 stumpwm::*mode-line-pad-y*)
             (nth-value 1 (stumpwm::rendered-size
                           (stumpwm::split-string (stumpwm::mode-line-contents mode-line)
                                                    (string #\Newline))
                           (stumpwm::mode-line-cc mode-line))))))
  (with-accessors ((window stumpwm::mode-line-window)
                   (head stumpwm::mode-line-head)
                   (position stumpwm::mode-line-position)
                   (height stumpwm::mode-line-height)
                   (factor stumpwm::mode-line-factor))
      mode-line
    (let* ((gap (if *gaps-on* *head-gaps-size* 0))
           (border (* 2 (xlib:drawable-border-width window)))
           (win-height (min (xlib:drawable-height window)
                            (truncate (stumpwm::head-height head) 4)))
           (total-height (+ win-height border gap)))
      (setf (xlib:drawable-width window) (- (stumpwm::head-width head)
                                            border
                                            (* 2 gap))
            (xlib:drawable-height window) win-height
            height total-height
            factor (- 1 (/ total-height
                           (stumpwm::head-height head)))
            (xlib:drawable-x window) (+ (stumpwm::head-x head) gap)
            (xlib:drawable-y window) (if (eq position :top)
                                          (+ (stumpwm::head-y head) gap)
                                          (- (+ (stumpwm::head-y head)
                                                (stumpwm::head-height head))
                                             total-height))))))


(defun refresh-mode-lines-gaps ()
  "Recompute geometry and redraw all mode-lines."
  (dolist (ml stumpwm::*mode-lines*)
    (stumpwm::resize-mode-line ml)
    (stumpwm::sync-mode-line ml)
    (stumpwm::redraw-mode-line ml t)))

(defun refresh-all-head-frames ()
  "Refresh frame layouts for all heads."
  (dolist (screen stumpwm::*screen-list*)
    (dolist (group (stumpwm::screen-groups screen))
      (dolist (head (stumpwm::screen-heads screen))
        (stumpwm::group-before-resize-head group head head)
        (stumpwm::group-after-resize-head group head))
      (stumpwm::sync-all-frame-windows group))))

(defcommand toggle-gaps () ()
  "Toggle gaps"
  (if (null *gaps-on*)
      (toggle-gaps-on)
      (toggle-gaps-off)))

(defcommand toggle-gaps-on () ()
  "Turn gaps on"
  (setf *gaps-on* t)
  (refresh-mode-lines-gaps)
  (refresh-all-head-frames)
  (stumpwm:refresh-heads)
  (reset-all-windows))

(defcommand toggle-gaps-off () ()
  "Turn gaps off"
  (setf *gaps-on* nil)
  (refresh-mode-lines-gaps)
  (refresh-all-head-frames)
  (stumpwm:refresh-heads)
  (reset-all-windows))
