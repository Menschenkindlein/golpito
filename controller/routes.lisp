(in-package #:golpito)

(restas:define-route main ("/"
                           :content-type "text/html")
  (:render-method #'golpito.view:tmpl-main)
  (golpito.model:get-bunch-of-articles))

(restas:define-route main-page ("/:page/"
                           :content-type "text/html")
  (:render-method #'golpito.view:tmpl-main)
  (:sift-variables (page #'parse-integer))
  (golpito.model:get-bunch-of-articles :page page))

(restas:define-route article ("/article/:article"
                              :content-type "text/html")
  (:render-method #'golpito.view:tmpl-article)
  (:sift-variables (article #'golpito.model:get-article))
  article)

(restas:define-route author-page ("/author/:author"
                                :content-type "text/html")
  (:apply-render-method #'golpito.view:tmpl-list-page)
  (:sift-variables (author #'golpito.model:get-author))
  (list author
        (golpito.model:get-bunch-of-articles
         :author (slot-value author 'golpito.model:name))))

(restas:define-route category-page ("/category/:category"
                                    :content-type "text/html")
  (:apply-render-method #'golpito.view:tmpl-list-page)
  (:sift-variables (category #'golpito.model:get-category))
  (list category
        (golpito.model:get-bunch-of-articles
         :category (slot-value category 'golpito.model:name))))

(restas:define-route tag-page ("/tag/:tag"
                               :content-type "text/html")
  (:apply-render-method #'golpito.view:tmpl-list-page)
  (:sift-variables (tag #'golpito.model:get-tag))
  (list tag
        (golpito.model:get-bunch-of-articles
         :tag (slot-value tag 'golpito.model:name))))

(restas:define-route list-page ("/list/"
                                :content-type "text/html")
  (:additional-variables
   (category (hunchentoot:get-parameter "category"))
   (tag (hunchentoot:get-parameter "tag"))
   (author (hunchentoot:get-parameter "author")))
  (:apply-render-method #'golpito.view:tmpl-list-page)
  (list
   (list
    (when author (golpito.model:get-author name))
    (when tag (golpito.model:get-tag name))
    (when category (golpito.model:get-category name)))
   (golpito.model:get-bunch-of-articles :category category
                                        :tag tag
                                        :author author)))