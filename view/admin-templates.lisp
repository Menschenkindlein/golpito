(in-package #:golpito.view)

(defun tmpl-edit-article (article)
  (golpito.view.tmpl:edit-article-tmpl (make-environment :article article)))

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
      <label><textarea name=\"description\">~a</textarea> Краткое описание особенностей автора</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-author-page-symbol*)
            name
            title
            description)))

(defun tmpl-edit-category (category)
  (golpito.view.tmpl:edit-category-tmpl (make-environment :category category)))

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
      <label><textarea name=\"description\">~a</textarea> Краткое описание тега</label><br />
      <input type=\"submit\" />
    </form>
  </body>
</html>"
            (restas:genurl *edit-tag-page-symbol*)
            name
            title
            description)))
