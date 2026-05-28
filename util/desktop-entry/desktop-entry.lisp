;;;; desktop-entry.lisp

(in-package #:desktop-entry)

(defvar *main-section* "Desktop Entry")
;;"reference: https://developer.gnome.org/desktop-entry-spec/"
(defclass desktop-entry ()
  ((entry-type :initarg :entry-type
               :initform (error "Must supply a entry type")
               :accessor entry-type)
   (name :initarg :name
         :initform (error "Must supply a entry name")
         :accessor name)
   (exec :initarg :exec
         :initform (error "Must supply a exec command")
         :accessor exec)
   (path :initarg :path
         :initform nil
         :accessor path)
   (categories :initarg :categories
               :initform '()
               :accessor categories)
   (no-display :initarg :no-display
               :initform nil
               :accessor no-display)
   (only-show-in :initarg :only-show-in
                 :initform nil
                 :accessor only-show-in)
   (terminal :initarg :terminal
             :initform nil
             :accessor terminal)))

(defmethod print-object ((object desktop-entry) stream)
  (format stream "(:name ~S :categories ~S :no-display ~S)"
          (name object) (categories object) (no-display object)))

(defun load-desktop-file (path &optional &key (main-section *main-section*))
  (let ((current-section nil)
        (data (make-hash-table :test 'equal)))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((trimmed (string-trim '(#\space #\tab) line))
                      (len (length trimmed)))
                 (cond
                   ((or (zerop len)
                        (char= (char trimmed 0) #\#))
                    nil)
                   ((and (> len 2)
                         (char= (char trimmed 0) #\[)
                         (char= (char trimmed (1- len)) #\]))
                    (setf current-section (subseq trimmed 1 (1- len))))
                   ((and current-section (position #\= trimmed))
                    (let* ((pos (position #\= trimmed))
                           (key (string-trim '(#\space #\tab)
                                             (subseq trimmed 0 pos)))
                           (val (string-trim '(#\space #\tab)
                                             (subseq trimmed (1+ pos)))))
                      (when (string= current-section main-section)
                        (setf (gethash key data) val))))))))
    (flet ((get-value (key &optional (type nil))
             (let ((val (gethash key data)))
               (when (and val (string/= val ""))
                 (case type
                   (:boolean (or (string-equal val "true")
                                 (string-equal val "1")
                                 (string-equal val "yes")))
                   (t val))))))
      (let* ((name (get-value "Name"))
             (entry-type (get-value "Type"))
             (exec (get-value "Exec"))
             (path (get-value "Path"))
             (categories (get-value "Categories"))
             (no-display (get-value "NoDisplay" :boolean))
             (only-show-in (get-value "OnlyShowIn"))
             (terminal (get-value "Terminal" :boolean)))
        (list
         :name name
         :entry-type entry-type
         :exec exec
         :path path
         :categories (if categories (string-split ";" categories) nil)
         :no-display no-display
         :only-show-in (if only-show-in (string-split ";" only-show-in) nil)
         :terminal terminal)))))

(defgeneric make-desktop-entry (path &optional &key main-section)
  (:documentation "init entry from a .desktop file"))

(defmethod make-desktop-entry ((entry-content list)
                               &optional &key (main-section *main-section*))
  (make-instance 'desktop-entry
                 :name (getf entry-content :name)
                 :entry-type (getf entry-content :entry-type)
                 :exec (getf entry-content :exec)
                 :path (getf entry-content :path)
                 :categories (getf entry-content :categories)
                 :no-display (getf entry-content :no-display)
                 :only-show-in (getf entry-content :only-show-in)
                 :terminal (getf entry-content :terminal)))

(defmethod make-desktop-entry ((path pathname)
                               &optional &key (main-section *main-section*))
  (make-desktop-entry (load-desktop-file path :main-section main-section)))

(defgeneric command-line (entry)
  (:documentation "get command line from an entry"))

(defmethod command-line (entry)
  (let ((exec-string (exec entry))
        (path-string (path entry)))
    (concatenate 'string path-string
                 (string-replace-all "%f|%F|%u|%U|%d|%D|%n|%N|%i|%c|%k|%v|%m"
                                     exec-string ""))))

(defgeneric add-category (entry category)
  (:documentation "add a category to an entry"))

(defmethod add-category ((entry desktop-entry) (category string))
  (with-accessors ((categories categories)) entry
    (when
        (not (member category categories :test #'string=))
      (setf categories (nconc categories (list category))))))

(defgeneric desktop-entry-equalp (entry-a entry-b)
  (:documentation
   "compares two desktop-entrys and is true if they are the same"))

(defmethod desktop-entry-equalp ((entry-a desktop-entry)
                                (entry-b desktop-entry))
  (and (string= (name entry-a) (name entry-b))
       (string= (entry-type entry-a) (entry-type entry-b))
       (string= (exec entry-a) (exec entry-b))))

(defgeneric desktop-entry-equal (entry-a entry-b)
  (:documentation
   "compares two desktop-entrys and is true if they are the same"))

(defmethod desktop-entry-equal ((entry-a desktop-entry)
                                (entry-b desktop-entry))
  (and (string= (name entry-a) (name entry-b))
       (string= (entry-type entry-a) (entry-type entry-b))
       (string= (exec entry-a) (exec entry-b))
       (equalp (path entry-a) (path entry-b))
       (equalp (categories entry-a) (categories entry-b))
       (equalp (no-display entry-a) (no-display entry-b))
       (equalp (only-show-in entry-a) (only-show-in entry-b))
       (equalp (terminal entry-a) (terminal entry-b))))

(defun entry-in-categories-p (entry category-sequence)
  (every #'(lambda (category)
             (some #'(lambda (entry-category)
                       (string= category entry-category))
                   (categories entry)))
         category-sequence))
