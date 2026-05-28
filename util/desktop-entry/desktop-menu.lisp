;;;; desktop-menu.lisp

(in-package #:desktop-entry)

(defvar *main-categories*
  (list
   "AudioVideo"
   "Audio"
   "Video"
   "Development"
   "Education"
   "Game"
   "Graphics"
   "Network"
   "Office"
   "Settings"
   "System"
   "Utility"))
(defvar *favorite-category* "Favorite")
(defvar *entry-paths*
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
(defvar *entry-list* '())
(defvar *favorite-list* '())

(defun longest-common-prefix (strings)
  "Return the longest common prefix of STRINGS, case-insensitive."
  (when strings
    (let* ((first (first strings))
           (min-len (reduce #'min (mapcar #'length strings)))
           prefix-len)
      (loop for i from 0 below min-len
            when (loop for s in (rest strings)
                       always (char-equal (char first i) (char s i)))
              do (setf prefix-len (1+ i))
            else do (return))
      (subseq first 0 (or prefix-len 0)))))

(defun desktop-menu-complete (menu)
  "Complete the current input to the longest common prefix of visible menu items."
  (let* ((items (stumpwm::menu-table menu))
         (names (mapcar #'stumpwm::menu-element-name items)))
    (when names
      (let ((prefix (longest-common-prefix names)))
        (when (and prefix (> (length prefix) 0))
          (let ((input (stumpwm::single-menu-current-input menu)))
            (setf (fill-pointer input) 0)
            (loop for c across prefix
                  do (vector-push-extend c input))
            (stumpwm::typing-action menu nil)))))))

(defvar *desktop-menu-keymap* (stumpwm:make-sparse-keymap))
(stumpwm:define-key *desktop-menu-keymap* (stumpwm:kbd "TAB") 'desktop-menu-complete)

(defgeneric add-favorite-entry (entry)
  (:documentation "add entry as favorite"))

(defmethod add-favorite-entry ((entry desktop-entry))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry pathname))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry-name string))
  (let ((entry-index (position
                      entry-name *entry-list*
                      :test #'(lambda (name entry)
                                (string= name (name entry))))))
    (when entry-index
      (add-favorite-entry (nth entry-index *entry-list*)))))


(defun init-entry-list (&optional (entry-paths *entry-paths*))
  (setf *entry-list* nil)
  (dolist (entry-path entry-paths)
    (dolist (entry-file (list-entry-files entry-path))
      (setf *entry-list*
            (add-to-entry-list *entry-list* entry-file)))))

(defun build-menu (categories)
  (let* ((min-entries-in-category (if (not categories) nil 5))
         (favorite-p (if (string= (first categories) *favorite-category*)
                         t
                         nil))
         (entry-list
          (find-entries
           (if favorite-p *favorite-list* *entry-list*)
           :test #'(lambda (entry)
                     (and (not (no-display entry))
                          (not (only-show-in entry))
                          (string= "Application" (entry-type entry))
                          (entry-in-categories-p
                           entry
                           (if favorite-p
                               (cdr categories)
                               categories))))))
         (menu
          (group-entries
           entry-list
           :categories
           (if categories
               (loop for item in (find-categories entry-list)
                  when (not (member item categories :test #'string=))
                  collect item)
               *main-categories*)
           :min-count min-entries-in-category))
         (menu (loop for item in menu
                  when (not (first item))
                  append (loop for entry in (rest item)
                            collect (list (name entry)
                                          entry))
                  else
                  collect (list (first item) (first item))))
         (menu (sort-menu menu))
         (menu (if categories
                   menu
                   (append
                    (list (list *favorite-category*
                                *favorite-category*))
                    menu
                    (loop for entry in entry-list
                          collect (list (name entry) entry))))))
    menu))

(defun sort-menu (menu)
  (sort menu
        #'(lambda (x y)
            (cond
              ((and (typep x 'desktop-entry)
                    (stringp y))
               nil)
              ((and (stringp x)
                    (typep y 'desktop-entry))
               T)
              ((and (stringp x)
                    (stringp y))
               (string-lessp x y))
              ((and (typep x 'desktop-entry)
                    (typep y 'desktop-entry))
               (string-lessp (name x) (name y)))
              (T nil))) :key #'second))


(stumpwm:defcommand show-desktop-menu ()
  ()
  "show the application menu"
  (let ((categories nil))
    (loop
       (let* ((menu (build-menu categories))
              (menu (if categories
                        (append menu (list (list ".." :up)
                                           (list "...." nil)))
                        (append menu (list (list ".." nil)))))
              (menu (loop for item in menu
                       collect
                         (if (stringp (second item))
                             (list (concatenate 'string (first item) " >>")
                                   (second item))
                             item)))
              (item (handler-case
                        (second (stumpwm:select-from-menu
                                  (stumpwm:current-screen)
                                  menu
                                  (format nil "/~{~A/~}:" (reverse categories))
                                  0
                                  *desktop-menu-keymap*))
                      (error (condition)
                        (stumpwm:message "~A" condition)
                        nil))))
         (cond
           ((not item) (return))
           ((stringp item) (push item categories))
           ((typep item 'desktop-entry)
            (stumpwm:run-shell-command (command-line item))
            (return))
           ((eq item :up)
            (pop categories)))))))
