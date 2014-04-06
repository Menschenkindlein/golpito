(restas:define-module #:golpito.view
  (:use #:cl #:golpito.model)
  (:export #:tmpl-article
           #:tmpl-main
           #:tmpl-list-page
           #:tmpl-edit-article
           #:tmpl-edit-author
           #:tmpl-edit-category
           #:tmpl-edit-tag
           #:*main-page-symbol*
           #:*article-page-symbol*
           #:*category-page-symbol*
           #:*author-page-symbol*
           #:*tag-page-symbol*
           #:*edit-article-page-symbol*
           #:*edit-category-page-symbol*
           #:*edit-author-page-symbol*
           #:*edit-tag-page-symbol*
           #:*bootstrap.css*
           #:*bootstrap.js*
           #:*jquery.js*))

(in-package #:golpito.view)

(defvar *main-page-symbol*)
(defvar *article-page-symbol*)
(defvar *category-page-symbol*)
(defvar *author-page-symbol*)
(defvar *tag-page-symbol*)

(defvar *edit-article-page-symbol*)
(defvar *edit-category-page-symbol*)
(defvar *edit-author-page-symbol*)
(defvar *edit-tag-page-symbol*)

(defvar *bootstrap.css*)
(defvar *bootstrap.js*)
(defvar *jquery.js*)

(defmacro local-pathname (&rest args)
  `(merge-pathnames (make-pathname ,@args)
                    (asdf:component-pathname
                     (asdf:find-system :golpito))))

(restas:define-route custom-css ("css/:(name).css"
                                 :content-type "text/css")
  (local-pathname :name name
                  :type "css"
                  :directory '(:relative "static")))
(restas:define-route custom-js ("js/:(name).js"
                                :content-type "application/javascript")
  (local-pathname :name name
                  :type "js"
                  :directory '(:relative "static")))

(restas:define-route bootstrap.css ("css/bootstrap.css"
                                    :content-type "text/css")
  (local-pathname :name "bootstrap"
                  :type "css"
                  :directory '(:relative "static")))
(restas:define-route bootstrap.js ("js/bootstrap.js"
                                    :content-type "application/javascript")
  (local-pathname :name "bootstrap"
                  :type "js"
                  :directory '(:relative "static")))
(restas:define-route jquery.js ("js/jquery.js"
                             :content-type "application/javascript")
  (local-pathname :name "jquery"
                  :type "js"
                  :directory '(:relative "static")))
(restas:define-route jquery.js ("js/jquery.js"
                             :content-type "application/javascript")
  (local-pathname :name "jquery"
                  :type "js"
                  :directory '(:relative "static")))
(restas:define-route fonts ("static/fonts/:(name).:type"
                                 :content-type "application/xml")
  (local-pathname :name name
                  :type type
                  :directory '(:relative "static")))

(defun time-to-string (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (format nil "~2,'0d.~2,'0d.~a ~d:~2,'0d" day month year hour min)))

(closure-template:define-print-syntax printDate "date" (:constant t))

(closure-template:register-print-handler
 :common-lisp-backend 'printDate
 :function #'(lambda (params env value)
               (declare (ignore params env))
               (time-to-string value)))

(closure-template:define-print-syntax printTextMarkdown "markdown" (:constant t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template
   :common-lisp-backend
   (local-pathname :name "helper" :type "soy1" :directory '(:relative "view"))))

(defun preprocess-markdown (text)
  (labels ((extract-crsl (stream)
             (loop
                with item
                with result
                for line = (read-line stream nil)
                until (or (not line) (string= line ":end-crsl"))
                do (if (search "http://" line
                               :end2 (min (length line) #.(length "http://")))
                       (progn
                         (when item
                           (setf (getf item :text)
                                 (with-output-to-string (out)
                                   (cl-markdown:markdown
                                    (format nil "~{~a~^~%~}" (getf item :text))
                                    :stream out)))
                           (push item result)
                           (setf item nil))
                         (setf (getf item :image) line))
                       (push line (getf item :text)))
                finally (progn
                          (when item
                            (setf (getf item :text)
                                  (with-output-to-string (out)
                                    (cl-markdown:markdown
                                     (format nil "~{~a~^~%~}" (getf item :text))
                                     :stream out)))
                            (push item result))
                          (return (nreverse result))))))
    (with-output-to-string (out)
      (with-input-from-string (in text)
        (loop
           for line = (read-line in nil)
           until (not line)
           do (write-line
               (cond
                 ((string= line ":crsl")
                  (golpito.view.helper:crsl (list :items (extract-crsl in))))
                 (t line))
               out))))))

(closure-template:register-print-handler
 :common-lisp-backend 'printTextMarkdown
 :function #'(lambda (params env value)
               (declare (ignore params env))
               (with-output-to-string (out)
                 (cl-markdown:markdown (preprocess-markdown value) :stream out))))

(closure-template:define-print-syntax printLink (and "link" ":" (or "article" "edit-article" "author" "category" "tag"))
  (:destructure (link |:| type)
                (declare (ignore link |:|))
                (intern (string-upcase type))))

(closure-template:register-print-handler
 :common-lisp-backend 'printLink
 :function #'(lambda (type env name)
               (declare (ignore env))
               (case type
                 (article (restas:genurl *article-page-symbol* :article name))
                 (article-edit (restas:genurl *edit-article-page-symbol*))
                 (author (restas:genurl *author-page-symbol* :author name))
                 (category (restas:genurl *category-page-symbol* :category name))
                 (tag (restas:genurl *tag-page-symbol* :tag name)))))

(loop for template in (directory (local-pathname :name :wild :type "soy" :directory '(:relative "view"))) do
     (closure-template:compile-template :common-lisp-backend template))