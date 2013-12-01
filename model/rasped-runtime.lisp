(in-package #:golpito.model)

(defvar *database* (make-hash-table))

(setf (gethash 'article *database*) (make-hash-table :test #'equal))
(setf (gethash 'author *database*) (make-hash-table :test #'equal))
(setf (gethash 'tag *database*) (make-hash-table :test #'equal))
(setf (gethash 'category *database*) (make-hash-table :test #'equal))

(defun first-n (n list)
  (if (or (= n 0)
          (null list))
      '()
      (cons (car list)
            (first-n (1- n) (cdr list)))))

(defun hash-table-values (hash-table)
  (let (result)
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value result))
             hash-table)
    result))

(defun get-bunch-of-articles (&key (page 1) (number 20) category tag author)
  (declare (ignore page))
  (first-n
   number
   (sort
    (remove-if-not
     (lambda (article)
       (and (or (null category)
                (eq (get-category category)
                    (slot-value article 'category)))
            (or (null author)
                (find (get-author author)
                      (slot-value article 'authors)))
            (or (null tag)
                (find (get-tag tag)
                      (slot-value article 'tags)))))
     (hash-table-values (gethash 'article *database*)))
    #'>
    :key (lambda (article) (slot-value article 'date)))))

(defun get-instance (type name)
  (gethash name (gethash type *database*)))

(defun set-instance (type name instance)
  (setf (gethash name (gethash type *database*))
        instance))

(defun get-article (name)
  (get-instance 'article name))
(defun get-author (name)
  (get-instance 'author name))
(defun get-category (name)
  (get-instance 'category name))
(defun get-tag (name)
  (get-instance 'tag name))

(defgeneric save (instance))

(defmethod save ((instance entity))
  (with-slots (name title description logo) instance
    (let ((real-instance (get-instance (class-name (class-of instance))
                                       name)))
      (if real-instance
          (setf (slot-value real-instance 'title) title
                (slot-value real-instance 'logo) logo
                (slot-value real-instance 'description) description)
          (set-instance (class-name (class-of instance))
                        name
                        instance)))))

(defmethod save ((instance article))
  (with-slots (name title description authors tags category date text logo) instance
    (let ((r-authors (mapcar (lambda (author) (get-author (slot-value author 'name))) authors))
          (r-tags (mapcar (lambda (tag) (get-tag (slot-value tag 'name))) tags))
          (r-category (get-category (slot-value category 'name))))
      (funcall
       (if (get-article name)
           #'update-article
           #'insert-article)
       name title description r-authors r-tags r-category date text logo))))

(defun insert-article (name title description authors tags category date text logo)
  (set-instance
   'article
   name
   (make-instance 'article
                  :name name
                  :title title
                  :authors authors
                  :primary-author (car authors)
                  :moreauthorsp (not (null (cdr authors)))
                  :description description
                  :category category
                  :tags tags
                  :date date
                  :logo logo
                  :text text)))

(defun update-article (name newtitle newdescription newauthors newtags newcategory newdate newtext newlogo)
  (with-slots (title description
                     authors primary-author moreauthorsp
                     tags category date text logo)
      (get-article name)
    (setf title newtitle
          description newdescription
          authors newauthors
          primary-author (car newauthors)
          moreauthorsp (not (null (cdr newauthors)))
          tags newtags
          category newcategory
          date newdate
          text newtext
          logo newlogo)))