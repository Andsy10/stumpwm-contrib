(in-package #:globalwindows)

(export '(goto-window with-global-windowlist
          global-windowlist global-pull-windowlist))

(defun global-windows ()
  "Returns a list of the names of all the windows in the current screen."
  (let ((groups (sort-groups (current-screen)))
        (windows nil))
    (dolist (group groups)
      (dolist (window (group-windows group))
        ;; Don't include the current window in the list
        (when (not (eq window (current-window)))
          (push window windows))))
    windows))

(defun goto-window (window)
  "Focus the window, switching to its group and screen if necessary."
  (focus-all window))

(define-stumpwm-type :global-window-names (input prompt)
  (labels
      ((global-window-names ()
         (mapcar (lambda (window) (window-name window)) (global-windows))))
    (or (argument-pop input)
        (completing-read (current-screen) prompt (global-window-names)))))

(defmacro with-global-windowlist (name docstring &rest args)
 `(defcommand ,name (&optional (fmt *window-format*)) (:rest)
   ,docstring
   (let ((global-windows-list (global-windows)))
     (labels
         ((sort-windows (windowlist)
            (sort1 windowlist 'string-lessp :key 'window-name)))
       (if (null global-windows-list)
           (message "No other windows on screen ;)")
           (let ((window (select-window-from-menu (sort-windows global-windows-list) fmt)))
             (when window
               (progn ,@args))))))))

(with-global-windowlist global-windowlist "Like windowlist, but for all groups not just the current one."
  (goto-window window))

(with-global-windowlist global-pull-windowlist
  "Global windowlist for pulling windows to the current frame."
  (let ((current-group (current-group)))
    (when (not (equalp (window-group window) current-group))
      (move-window-to-group window current-group))
    (if (and (typep current-group 'stumpwm::tile-group)
             (typep window 'stumpwm::tile-window))
        (pull-window window)
        (group-focus-window current-group window))))
