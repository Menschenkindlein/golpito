(in-package #:golpito.model)

(defvar *database-path*
  (merge-pathnames "database.sqlite3"
                   (asdf:component-pathname
                    (asdf:find-system :golpito))))

(defgeneric ensure-tables (model))
(defmethod ensure-tables ((model entity))
  (sqlite:with-open-database (db *database-path*)
    (let ((model-name (string-downcase (string (class-name (class-of model))))))
      (unless (sqlite:execute-to-list db "SELECT sql FROM sqlite_master WHERE tbl_name = ?"
                                      model-name)
        (sqlite:execute-non-query db 
                                  (format nil "
CREATE TABLE ~a (name     VARCHAR PRIMARY KEY,
                 title    VARCHAR NOT NULL,
                 description TEXT NOT NULL);"
                                          model-name))))))

(defmethod ensure-tables ((model article))
  (sqlite:with-open-database (db *database-path*)
    (unless (sqlite:execute-to-list db "SELECT sql FROM sqlite_master WHERE tbl_name = 'article'")
      (sqlite:execute-non-query db "
CREATE TABLE article (name     VARCHAR PRIMARY KEY,
                      title    VARCHAR NOT NULL,
                      description TEXT NOT NULL,
                      author   VARCHAR NOT NULL,
                      moreauthorsp INTEGER NOT NULL,
                      category VARCHAR NOT NULL,
                      date     INTEGER NOT NULL,
                      text     TEXT NOT NULL,
                      FOREIGN KEY(category) REFERENCES category(name),
                      FOREIGN KEY(author) REFERENCES author(name));")
      (sqlite:execute-non-query db "
CREATE TABLE linktag (article  VARCHAR NOT NULL,
                      tag      VARCHAR NOT NULL,
                      UNIQUE(article, tag),
                      FOREIGN KEY(article) REFERENCES article(name),
                      FOREIGN KEY(tag) REFERENCES tag(name));")
      (sqlite:execute-non-query db "
CREATE TABLE linkauthor (article  VARCHAR NOT NULL,
                         author   VARCHAR NOT NULL,
                         UNIQUE(article, author),
                         FOREIGN KEY(article) REFERENCES article(name),
                         FOREIGN KEY(author) REFERENCES author(name));"))))

(defun get-bunch-of-articles (&key (page 1) (number 20) category tag author)
  (sqlite:with-open-database (db *database-path*)
    (mapcar
     (lambda (row)
       (destructuring-bind
             (name title description
              primary-author primary-author-fullname moreauthorsp
              category category-title
              date)
           row
         (make-instance 'article
                        :name name
                        :title title
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
    (sqlite:execute-to-list db
                            (format nil "
SELECT DISTINCT article.name,article.title,article.description,
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
       WHERE ~:[1=1~;category = '~:*~a'~]
         AND ~:[1=1~;linktag.tag = '~:*~a'~]
         AND ~:[1=1~;linkauthor.author = '~:*~a'~]
       ORDER BY date DESC LIMIT ? OFFSET ?;" category tag author)
                            number (* (1- page) number)))))

(loop for model in (list 'category 'tag 'author 'article) do
     (ensure-tables (make-instance model)))

(defgeneric populate (instance))

(defmethod populate ((instance article))
  (with-slots (name title description
                    primary-author
                    authors tags
                    category
                    date
                    text) instance
    (sqlite:with-open-database (db *database-path*)
      (let ((row (car (sqlite:execute-to-list db "
SELECT DISTINCT article.name,article.title,article.description,
       article.author,author.title,moreauthorsp,
       category,category.title,
       date,
       text
  FROM article
  JOIN author
    ON article.author=author.name
  JOIN category
    ON category=category.name
 WHERE article.name = ?" name))))
        (when row
          (destructuring-bind
                (name newtitle newdescription
                      newprimary-author newprimary-author-fullname newmoreauthorsp
                      newcategory newcategory-title
                      newdate
                      newtext)
              row
            (setf title newtitle)
            (setf description newdescription)
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
                  (get-assotiations db instance 'author))
            (setf tags
                  (get-assotiations db instance 'tag)))
          instance)))))

(defun get-assotiations (db from to)
  (mapcar (lambda (row)
            (destructuring-bind (name fullname) row
              (make-instance to
                             :name name
                             :title fullname)))
          (sqlite:execute-to-list
           db 
           (format nil "
SELECT name, title
  FROM ~a
  JOIN link~:*~a
    ON ~:*~a.name=link~:*~a.~:*~a
       WHERE link~:*~a.article = ?;"
                   (string-downcase (string to)))
           (slot-value from 'name))))

(defmethod populate ((instance entity))
  (with-slots (name title description) instance
    (sqlite:with-open-database (db *database-path*)
      (let ((row (car
                  (sqlite:execute-to-list
                   db
                   (format nil
                           "SELECT * FROM ~a WHERE name = ?"
                           (class-name (class-of instance)))
                   name))))
        (when row
          (destructuring-bind (name newtitle newdescription)     
              row
            (setf title newtitle)
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
    (sqlite:with-open-database (db *database-path*)
      (let ((model-name (string-downcase (string (class-name (class-of instance))))))
        (if (car (sqlite:execute-to-list db (format nil "SELECT * FROM ~a WHERE name = ?" model-name) name))
            (sqlite:execute-non-query db (format nil "UPDATE ~a SET title = ?, description = ? WHERE name = ?" model-name) title description name)
            (sqlite:execute-non-query db (format nil "INSERT INTO ~a (name,title,description) VALUES (?,?,?)" model-name) name title description))))))

(defmethod save ((instance article))
  (with-slots (name title description authors tags category date text) instance
    (let ((authors-names (mapcar (lambda (author) (slot-value author 'name)) authors))
          (tags-names (mapcar (lambda (tag) (slot-value tag 'name)) tags))
          (category-name (slot-value category 'name)))
      (sqlite:with-open-database (db *database-path*)
        (funcall
         (if (car (sqlite:execute-to-list db "SELECT * FROM article WHERE name = ?" name))
            #'update-article
            #'insert-article)
         name title description authors-names tags-names category-name (floor date 60) text)))))

(defun insert-article (name title description authors tags category date text)
  (sqlite:with-open-database (db *database-path*)
    (sqlite:execute-non-query db "
INSERT INTO article (name,title,author,moreauthorsp,description,category,date,text)
             VALUES (   ?,    ?,     ?,           ?,          ?,       ?,   ?,   ?)"
                              name
                              title
                              (car authors)
                              (if (null (cdr authors)) 0 1)
                              description
                              category
                              date
                              text)
    (loop for author in authors do
         (sqlite:execute-non-query db "
INSERT INTO linkauthor (article, author)
                VALUES (      ?,      ?);"
                                   name
                                   author))
    (loop for tag in tags do
         (sqlite:execute-non-query db "
INSERT INTO linktag (article, tag)
             VALUES (      ?,   ?);"
                                   name
                                   tag))))

(defun update-article (name title description authors tags category date text)
  (sqlite:with-open-database (db *database-path*)
    (sqlite:execute-non-query db "
UPDATE article
   SET title = ?,
       author = ?,
       moreauthorsp = ?,
       description = ?,
       category = ?,
       date = ?,
       text = ?
 WHERE name = ?;"
                              title
                              (car authors)
                              (if (null (cdr authors)) 0 1)
                              description
                              category
                              date
                              text
                              name)
    (let ((old-authors
           (mapcar #'car
                   (sqlite:execute-to-list db
                                           "
SELECT name
FROM author
JOIN linkauthor ON linkauthor.author = author.name
WHERE linkauthor.article = ?;"
                                           name))))
      (loop for author in (set-difference authors old-authors :test #'equal) do
           (sqlite:execute-non-query db "
INSERT INTO linkauthor (article, author)
                 VALUES (      ?,      ?);"
                                     name
                                     author))
      (loop for badauthor in (set-difference old-authors authors :test #'equal) do
           (sqlite:execute-non-query db "
DELETE FROM linkauthor
WHERE article = ? AND author = ?;"
                                     name
                                     badauthor)))

    (let ((old-tags
           (mapcar #'car
                   (sqlite:execute-to-list db
                                           "
SELECT name
FROM tag
JOIN linktag ON linktag.tag = tag.name
WHERE linktag.article = ?"
                                           name))))
      (loop for tag in (set-difference tags old-tags :test #'equal) do
           (sqlite:execute-non-query db "
INSERT INTO linktag (article, tag)
              VALUES (      ?,   ?);"
                                     name
                                     tag))
      (loop for badtag in (set-difference old-tags tags :test #'equal) do
           (sqlite:execute-non-query db "
DELETE FROM linktag
WHERE article = ? AND tag = ?;"
                                     name
                                     badtag)))))