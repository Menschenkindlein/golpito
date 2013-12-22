(in-package #:golpito.view)

(defun make-environment (&rest options)
  `(:head (list :bootstrap-css (restas:genurl *bootstrap.css*)
                :bootstrap-js (restas:genurl *bootstrap.js*)
                :jquery-js (restas:genurl *jquery.js*))
    ,@options))

(defun tmpl-article (article)
  (golpito.view.tmpl:article (make-environment :article article)))

(defun tmpl-main (lists-of-articles)
  (golpito.view.tmpl:main
   (apply #'make-environment
          lists-of-articles)))

(defun tmpl-list-page (category list-of-articles)
  (if (listp category)
      (golpito.view.tmpl:list-page (make-environment :categories category :articles list-of-articles))
      (golpito.view.tmpl:category (make-environment :category category :articles list-of-articles))))
