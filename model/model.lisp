(defpackage #:golpito.model
  (:use #:cl #:golpito.model.abstraction)
  (:export #:author
           #:tag
           #:category
           #:article
           #:get-author
           #:get-tag
           #:get-category
           #:get-article
           #:save
           #:get-bunch-of-articles
           #:title
           #:description
           #:name
           #:text
           #:date
           #:authors
           #:primary-author
           #:moreauthorsp
           #:tags))

(in-package #:golpito.model)

(defmodel entity ()
  ((name)
   (title)
   (description))
  (:key name))

(defmodel author (entity) ())
(defmodel tag (entity) ())
(defmodel category (entity) ())
(defmodel article (entity)
  ((authors :list author :has-first)
   (tags :list tag)
   (category category)
   (date :date)
   (text :text)))
