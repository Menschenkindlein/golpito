(in-package #:golpito.view)

(defun make-environment (body &optional head)
  `(:head (:bootstrap-css ,(restas:genurl *bootstrap.css*)
           :bootstrap-js ,(restas:genurl *bootstrap.js*)
           :jquery-js ,(restas:genurl *jquery.js*)
           ,@head)
    ,@body))

(defun tmpl-article (article)
  (golpito.view.tmpl:article (make-environment (list :article article))))

(defun tmpl-main (lists-of-articles)
  (golpito.view.tmpl:main
   (make-environment
    lists-of-articles)))

(defun tmpl-list-page (category list-of-articles)
  (if (listp category)
      (golpito.view.tmpl:list-page (make-environment
                                    (list :categories category
                                          :articles list-of-articles)))
      (golpito.view.tmpl:category (make-environment
                                   (list :category category
                                         :articles list-of-articles)))))
