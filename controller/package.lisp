(restas:define-module #:golpito
  (:use #:cl))

(in-package #:golpito)

(restas:mount-module view (#:golpito.view)
  (:url "static/"))

(setf golpito.view:*main-page-symbol* 'main)
(setf golpito.view:*article-page-symbol* 'article)
(setf golpito.view:*category-page-symbol* 'category-page)
(setf golpito.view:*author-page-symbol* 'author-page)
(setf golpito.view:*tag-page-symbol* 'tag-page)
(setf golpito.view:*edit-article-page-symbol* 'edit-article-save)
(setf golpito.view:*edit-category-page-symbol* 'edit-category-save)
(setf golpito.view:*edit-author-page-symbol* 'edit-author-save)
(setf golpito.view:*edit-tag-page-symbol* 'edit-tag-save)
(setf golpito.view:*bootstrap.css* 'view.bootstrap.css)
(setf golpito.view:*bootstrap.js* 'view.bootstrap.js)
(setf golpito.view:*jquery.js* 'view.jquery.js)