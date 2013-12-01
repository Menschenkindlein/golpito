(in-package #:golpito.view)

(defun tmpl-edit-article (article)
  (with-slots
        (name title description
              authors
              tags category
              date text)
      article
    (format nil
            "<html>
  <head>
    <title>Edit article</title>
  </head>
  <body>
    <h1>Edit article</h1>
    <form action=\"~a\" method=\"post\">
      <input type=\"hidden\" value=\"~a\" name=\"name\" />
      <label><input type=\"text\" value=\"~a\" name=\"title\" /> Название статьи</label><br />
      <label><input type=\"text\" value=\"~{~a~^ ~}\" name=\"authors\" /> Авторы статьи, разделенные пробелами</label><br />
      <label><input type=\"text\" value=\"~{~a~^ ~}\" name=\"tags\" /> Теги статьи, разделенные пробелами</label><br />
      <label><input type=\"text\" value=\"~a\" name=\"date\" /> Дата в формате ДД.ММ.ГГГГ ЧЧ:мм</label><br />
      <label><input type=\"text\" value=\"~a\" name=\"category\" /> Категория</label><br />
      <label><textarea name=\"description\">~{~a~^~%~}</textarea> Краткое описание статьи</label><br />
      <label><textarea name=\"text\">~{~a~^~%~}</textarea> Текст статьи</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-article-page-symbol*)
            name
            title
            (mapcar (lambda (author) (slot-value author 'name)) authors)
            (mapcar (lambda (tag) (slot-value tag 'name)) tags)
            (time-to-string date)
            (slot-value category 'name)
            (split-sequence:split-sequence #\Newline description :remove-empty-subseqs t)
            (split-sequence:split-sequence #\Newline text :remove-empty-subseqs t))))

(defun tmpl-edit-author (author)
  (with-slots
        (name title description)
      author
    (format nil
            "<html>
  <head>
    <title>Edit author</title>
  </head>
  <body>
    <h1>Edit author</h1>
    <form action=\"~a\" method=\"post\">
      <input type=\"hidden\" value=\"~a\" name=\"name\" />
      <label><input type=\"text\" value=\"~a\" name=\"title\" /> Имя автора</label><br />
      <label><textarea name=\"description\">~{~a~^~%~}</textarea> Краткое описание особенностей автора</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-author-page-symbol*)
            name
            title
            (split-sequence:split-sequence #\Newline description :remove-empty-subseqs t))))
(defun tmpl-edit-category (category)
  (with-slots
        (name title description)
      category
    (format nil
            "<html>
  <head>
    <title>Edit category</title>
  </head>
  <body>
    <h1>Edit category</h1>
    <form action=\"~a\" method=\"post\">
      <input type=\"hidden\" value=\"~a\" name=\"name\" />
      <label><input type=\"text\" value=\"~a\" name=\"title\" /> Название категории</label><br />
      <label><textarea name=\"description\">~{~a~^~%~}</textarea> Краткое описание особенностей категории</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-category-page-symbol*)
            name
            title
            (split-sequence:split-sequence #\Newline description :remove-empty-subseqs t))))
(defun tmpl-edit-tag (tag)
  (with-slots
        (name title description)
      tag
    (format nil
            "<html>
  <head>
    <title>Edit tag</title>
  </head>
  <body>
    <h1>Edit tag</h1>
    <form action=\"~a\" method=\"post\">
      <input type=\"hidden\" value=\"~a\" name=\"name\" />
      <label><input type=\"text\" value=\"~a\" name=\"title\" /> Полное имя тега</label><br />
      <label><textarea name=\"description\">~{~a~^~%~}</textarea> Краткое описание тега</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-tag-page-symbol*)
            name
            title
            (split-sequence:split-sequence #\Newline description :remove-empty-subseqs t))))
