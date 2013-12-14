(in-package #:golpito.view)

(defun tmpl-article (article)
  (golpito.view.tmpl:article (make-environment :article article)))

(defun tmpl-main (list-of-articles)
  (golpito.view.tmpl:main (make-environment :super-articles list-of-articles :articles list-of-articles)))

(defun tmpl-list-page (category list-of-articles)
  (if (listp category)
      (golpito.view.tmpl:list-page (make-environment :categories category :articles list-of-articles))
      (golpito.view.tmpl:category (make-environment :category category :articles list-of-articles))))
