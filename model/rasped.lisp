(in-package #:golpito.model)

(defvar *connection* '("golpito" "maximo" "maximo" "localhost"))

(defmacro with-connection (&body body)
  `(postmodern:with-connection *connection*
     ,@body))

(defun get-bunch-of-articles (&key (page 1) (number 20) category tag author featured)
  (with-connection
    (mapcar
     (lambda (row)
       (destructuring-bind
             (name title logo description
              primary-author primary-author-fullname moreauthorsp
              category category-title
              date)
           row
         (make-instance 'article
                        :name name
                        :title title
                        :logo logo
                        :description description
                        :primary-author (make-instance
                                         'author
                                         :name primary-author
                                         :title primary-author-fullname)
                        :moreauthorsp (not (zerop moreauthorsp))
                        :category (make-instance
                                   'category
                                   :name category
                                   :title category-title)
                        :date (* date 60))))
    (postmodern:query
     (format nil "
SELECT DISTINCT article.name,article.title,article.logo,article.description,
       article.author,author.title,moreauthorsp,
       category,category.title,
       date
  FROM article
  JOIN linkauthor
    ON article.name=linkauthor.article
  LEFT JOIN linktag
    ON article.name=linktag.article
  JOIN author
    ON article.author=author.name
  JOIN category
    ON category=category.name
       WHERE ~:[category <> 'news'~;category = '~:*~a'~]
         AND ~:[1=1~;linktag.tag = '~:*~a'~]
         AND ~:[1=1~;linkauthor.author = '~:*~a'~]
         AND ~:[1=1~;featured = TRUE~]
       ORDER BY date DESC LIMIT ~a OFFSET ~a;"
             category tag author featured
             number (* (1- page) number))))))

(defgeneric populate (instance))

(defmethod populate ((instance article))
  (with-slots (name title logo description featured
                    primary-author
                    moreauthorsp
                    authors tags
                    category
                    date
                    text) instance
    (with-connection
      (let ((row (car (postmodern:query
                       (format nil "
SELECT DISTINCT article.name,article.title,article.logo,article.description,
       featured,
       article.author,author.title,moreauthorsp,
       category,category.title,
       date,
       text
  FROM article
  JOIN author
    ON article.author=author.name
  JOIN category
    ON category=category.name
 WHERE article.name = '~a'" name)))))
        (when row
          (destructuring-bind
                (name newtitle newlogo newdescription newfeatured
                      newprimary-author newprimary-author-fullname newmoreauthorsp
                      newcategory newcategory-title
                      newdate
                      newtext)
              row
            (setf title newtitle)
            (setf logo newlogo)
            (setf description newdescription)
            (setf featured newfeatured)
            (setf primary-author (make-instance
                                  'author
                                  :name newprimary-author
                                  :title newprimary-author-fullname))
            (setf moreauthorsp (not (zerop newmoreauthorsp)))
            (setf category (make-instance
                            'category
                            :name newcategory
                            :title newcategory-title))
            (setf date (* newdate 60))
            (setf text newtext)
            (setf authors
                  (get-assotiations instance 'author))
            (setf tags
                  (get-assotiations instance 'tag)))
          instance)))))

(defun get-assotiations (from to)
  (mapcar (lambda (row)
            (destructuring-bind (name fullname) row
              (make-instance to
                             :name name
                             :title fullname)))
          (postmodern:query (format nil "
SELECT name, title
  FROM ~a
  JOIN link~:*~a
    ON ~:*~a.name=link~:*~a.~:*~a
       WHERE link~:*~a.article = '~a';"
                   (string-downcase (string to))
                   (slot-value from 'name)))))

(defmethod populate ((instance entity))
  (with-slots (name title logo description) instance
    (with-connection
      (let ((row (car
                  (postmodern:query
                   (format nil
                           "SELECT * FROM ~a WHERE name = '~a'"
                           (class-name (class-of instance))
                           name)))))
        (when row
          (destructuring-bind (name newtitle newlogo newdescription)
              row
            (setf title newtitle)
            (setf logo newlogo)
            (setf description newdescription))
          instance)))))

(defun get-article (name)
  (populate (make-instance 'article :name name)))
(defun get-author (name)
  (populate (make-instance 'author :name name)))
(defun get-category (name)
  (populate (make-instance 'category :name name)))
(defun get-tag (name)
  (populate (make-instance 'tag :name name)))

(defgeneric save (instance))

(defmethod save ((instance entity))
  (with-slots (name title description) instance
    (with-connection
      (let ((model-name (string-downcase (string (class-name (class-of instance))))))
        (if (car (postmodern:query (format nil "SELECT * FROM ~a WHERE name = '~a'" model-name name)))
            (postmodern:execute (format nil "UPDATE ~a SET title = '~a', description = '~a' WHERE name = '~a'" model-name title description name))
            (postmodern:execute (format nil "INSERT INTO ~a (name,title,description) VALUES ('~a','~a','~a')" model-name name title description)))))))

(defmethod save ((instance article))
  (with-slots (name title logo featured description authors tags category date text) instance
    (let ((authors-names (mapcar (lambda (author) (slot-value author 'name)) authors))
          (tags-names (mapcar (lambda (tag) (slot-value tag 'name)) tags))
          (category-name (slot-value category 'name)))
      (with-connection
        (funcall
         (if (car (postmodern:query (format nil "SELECT * FROM article WHERE name = '~a'" name)))
            #'update-article
            #'insert-article)
         name title logo featured description authors-names tags-names category-name (floor date 60) text)))))

(defun insert-article (name title logo featured description authors tags category date text)
  (with-connection
    (postmodern:execute (format nil "
INSERT INTO article (name,title,logo,        featured,author,moreauthorsp,description,category,date, text)
             VALUES ('~a', '~a','~a',~:[FALSE~;TRUE~],  '~a',          ~a,       '~a',    '~a',  ~a, '~a')"
                                name
                                title
                                logo
                                featured
                                (car authors)
                                (if (null (cdr authors)) 0 1)
                                description
                                category
                                date
                                text))
    (loop for author in authors do
         (postmodern:execute (format nil "
INSERT INTO linkauthor (article, author)
                VALUES (   '~a',   '~a');"
                                     name
                                     author)))
    (loop for tag in tags do
         (postmodern:execute (format nil "
INSERT INTO linktag (article, tag)
             VALUES (   '~a','~a');"
                                     name
                                     tag)))))

(defun update-article (name title logo featured description authors tags category date text)
  (with-connection
    (postmodern:execute (format nil "
UPDATE article
   SET title = '~a',
       logo = '~a',
       featured = ~:[FALSE~;TRUE~],
       author = '~a',
       moreauthorsp = ~a,
       description = '~a',
       category = '~a',
       date = ~a,
       text = '~a'
 WHERE name = '~a';"
                                title
                                logo
                                featured
                                (car authors)
                                (if (null (cdr authors)) 0 1)
                                description
                                category
                                date
                                text
                                name))
    (let ((old-authors
           (mapcar #'car
                   (postmodern:query (format nil "
SELECT name
FROM author
JOIN linkauthor ON linkauthor.author = author.name
WHERE linkauthor.article = '~a';"
                                             name)))))
      (loop for author in (set-difference authors old-authors :test #'equal) do
           (postmodern:execute (format nil "
INSERT INTO linkauthor (article, author)
                VALUES (   '~a',   '~a');"
                                       name
                                       author)))
      (loop for badauthor in (set-difference old-authors authors :test #'equal) do
           (postmodern:execute (format nil "
DELETE FROM linkauthor
WHERE article = '~a' AND author = '~a';"
                                       name
                                       badauthor))))

    (let ((old-tags
           (mapcar #'car
                   (postmodern:query (format nil "
SELECT name
FROM tag
JOIN linktag ON linktag.tag = tag.name
WHERE linktag.article = '~a'"
                                             name)))))
      (loop for tag in (set-difference tags old-tags :test #'equal) do
           (postmodern:execute (format nil "
INSERT INTO linktag (article, tag)
             VALUES (   '~a','~a');"
                                       name
                                       tag)))
      (loop for badtag in (set-difference old-tags tags :test #'equal) do
           (postmodern:execute (format nil "
DELETE FROM linktag
WHERE article = '~a' AND tag = '~a';"
                                       name
                                       badtag))))))