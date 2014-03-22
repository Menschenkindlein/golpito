(in-package #:golpito)

(defclass http-auth-route (routes:proxy-route) ())

(defun string-to-seq (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defmethod routes:route-check-conditions ((route http-auth-route) bindings)
  (and (call-next-method)
       (multiple-value-bind (user password) (hunchentoot:authorization)
         (or (and (string= user "admin")
                  (equal
                   (ironclad:digest-sequence
                    'ironclad:md5
                    (string-to-seq password))
                   #(95 77 204 59 90 167 101 214 29
                     131 39 222 184 130 207 153)))
             (hunchentoot:require-authorization)))))

(defun @http-auth-require (route)
  (make-instance 'http-auth-route :target route))

(restas:define-route edit-article ("/edit/article/:name"
                                   :method :GET)
  (:decorators #'@http-auth-require)
  (:render-method #'golpito.view:tmpl-edit-article)
  (or (golpito.model:get-article name)
      (make-instance 'golpito.model:article
                     :name name
                     :title "заголовок"
                     :logo "http://www.weblogcartoons.com/cartoons/i-have-nothing-to-say.gif"
                     :authors (list (make-instance 'golpito.model:author
                                                   :name "default-author"))
                     :description "описание"
                     :text "Текст"
                     :category (make-instance 'golpito.model:category
                                              :name "default-category")
                     :tags (list (make-instance 'golpito.model:tag
                                                :name "default-tag"))
                     :date (get-universal-time))))

(restas:define-route edit-author ("/edit/author/:name"
                                   :method :GET)
  (:decorators #'@http-auth-require)
  (:render-method #'golpito.view:tmpl-edit-author)
  (or (golpito.model:get-author name)
      (make-instance 'golpito.model:author
                     :name name
                     :title "Имя"
                     :description "описание")))

(restas:define-route edit-category ("/edit/category/:name"
                                   :method :GET)
  (:decorators #'@http-auth-require)
  (:render-method #'golpito.view:tmpl-edit-category)
  (or (golpito.model:get-category name)
      (make-instance 'golpito.model:category
                     :name name
                     :title "Название"
                     :description "описание")))

(restas:define-route edit-tag ("/edit/tag/:name"
                                   :method :GET)
  (:decorators #'@http-auth-require)
  (:render-method #'golpito.view:tmpl-edit-tag)
  (or (golpito.model:get-tag name)
      (make-instance 'golpito.model:tag
                     :name name
                     :title "Название"
                     :description "описание")))

(defun string-to-time (timestring)
  (destructuring-bind (day month year hour minute)
      (mapcar #'parse-integer
              (split-sequence:split-sequence-if-not
               #'alphanumericp timestring))
    (encode-universal-time 1 minute hour day month year)))

(restas:define-route edit-article-save ("/edit/article/"
                                        :method :POST)
  (:decorators #'@http-auth-require)
  (:additional-variables
   (name (hunchentoot:post-parameter "name"))
   (logo (hunchentoot:post-parameter "logo"))
   (title (hunchentoot:post-parameter "title"))
   (featured (hunchentoot:post-parameter "featured"))
   (description (hunchentoot:post-parameter "description"))
   (authors (hunchentoot:post-parameter "authors"))
   (tags (hunchentoot:post-parameter "tags"))
   (category (hunchentoot:post-parameter "category"))
   (date (hunchentoot:post-parameter "date"))
   (text (hunchentoot:post-parameter "text")))
  (golpito.model:save
   (make-instance 'golpito.model:article
                  :name name
                  :logo logo
                  :title title
                  :featured featured
                  :description description
                  :authors (mapcar (lambda (name) (make-instance 'golpito.model:author
                                                                 :name name))
                                   (split-sequence:split-sequence #\Space authors))
                  :tags (mapcar (lambda (name) (make-instance 'golpito.model:tag
                                                                 :name name))
                                   (split-sequence:split-sequence #\Space tags))
                  :category (make-instance 'golpito.model:category :name category)
                  :date (string-to-time date)
                  :text text))
  (restas:redirect 'edit-article :name name))

(restas:define-route edit-author-save ("/edit/author/"
                                        :method :POST)
  (:decorators #'@http-auth-require)
  (:additional-variables
   (name (hunchentoot:post-parameter "name"))
   (title (hunchentoot:post-parameter "title"))
   (description (hunchentoot:post-parameter "description")))
  (golpito.model:save
   (make-instance 'golpito.model:author
                  :name name
                  :title title
                  :description description))
  (restas:redirect 'edit-author :name name))
(restas:define-route edit-category-save ("/edit/category/"
                                         :method :POST)
  (:decorators #'@http-auth-require)
  (:additional-variables
   (name (hunchentoot:post-parameter "name"))
   (title (hunchentoot:post-parameter "title"))
   (description (hunchentoot:post-parameter "description")))
  (golpito.model:save
   (make-instance 'golpito.model:category
                  :name name
                  :title title
                  :description description))
  (restas:redirect 'edit-category :name name))
(restas:define-route edit-tag-save ("/edit/tag/"
                                    :method :POST)
  (:decorators #'@http-auth-require)
  (:additional-variables
   (name (hunchentoot:post-parameter "name"))
   (title (hunchentoot:post-parameter "title"))
   (description (hunchentoot:post-parameter "description")))
  (golpito.model:save
   (make-instance 'golpito.model:tag
                  :name name
                  :title title
                  :description description))
  (restas:redirect 'edit-tag :name name))
