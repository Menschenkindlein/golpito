(in-package #:golpito.view)

(defun tmpl-edit-article (article)
  (golpito.view.tmpl:edit-article-tmpl
   (make-environment (list :article article))))

(defun tmpl-edit-author (author)
  (golpito.view.tmpl:edit-category-tmpl
   (make-environment (list :category author
                           :name "author"
                           :name-label "Имя автора"
                           :name-description "Краткое описание особенностей автора"))))

(defun tmpl-edit-category (category)
  (golpito.view.tmpl:edit-category-tmpl
   (make-environment (list :category category
                           :name "category"
                           :name-label "Название категории"
                           :name-description "Краткое описание категории"))))

(defun tmpl-edit-tag (tag)
  (golpito.view.tmpl:edit-category-tmpl
   (make-environment (list :category tag
                           :name "tag"
                           :name-label "Полное имя тега"
                           :name-description "Краткое описание тега"))))
