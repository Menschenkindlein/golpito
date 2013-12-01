(restas:define-module #:golpito.view
  (:use #:cl #:golpito.model)
  (:export #:tmpl-article
           #:tmpl-main
           #:tmpl-list-page
           #:tmpl-edit-article
           #:tmpl-edit-author
           #:tmpl-edit-category
           #:tmpl-edit-tag
           #:*main-page-symbol*
           #:*article-page-symbol*
           #:*category-page-symbol*
           #:*author-page-symbol*
           #:*tag-page-symbol*
           #:*edit-article-page-symbol*
           #:*edit-category-page-symbol*
           #:*edit-author-page-symbol*
           #:*edit-tag-page-symbol*
           #:*bootstrap.css*
           #:*bootstrap.js*
           #:*jquery.js*))

(in-package #:golpito.view)

(defvar *main-page-symbol*)
(defvar *article-page-symbol*)
(defvar *category-page-symbol*)
(defvar *author-page-symbol*)
(defvar *tag-page-symbol*)

(defvar *edit-article-page-symbol*)
(defvar *edit-category-page-symbol*)
(defvar *edit-author-page-symbol*)
(defvar *edit-tag-page-symbol*)

(defvar *bootstrap.css*)
(defvar *bootstrap.js*)
(defvar *jquery.js*)

(defun nav-bar ()
  (format nil "
     <div class=\"navbar navbar-default navbar-fixed-top\" role=\"navigation\">
       <div class=\"container\">
         
         <div class=\"navbar-header\">
           <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\">
             <span class=\"sr-only\">Toggle navigation</span>
             <span class=\"icon-bar\"></span>
             <span class=\"icon-bar\"></span>
             <span class=\"icon-bar\"></span>
           </button>
           <a class=\"navbar-brand\" href=\"~a\">Golpito</a>
         </div>
         <div class=\"collapse navbar-collapse\">
         </div><!--/.nav-collapse -->
       </div>
     </div>
"
          (restas:genurl *main-page-symbol*)))

(defmacro local-pathname (&rest args)
  `(merge-pathnames (make-pathname ,@args)
                    (asdf:component-pathname
                     (asdf:find-system :golpito))))

(restas:define-route bootstrap.css ("css/bootstrap.css"
                                    :content-type "text/css")
  (local-pathname :name "bootstrap"
                  :type "css"
                  :directory '(:relative "static")))
(restas:define-route bootstrap.js ("js/bootstrap.js"
                                    :content-type "application/javascript")
  (local-pathname :name "bootstrap"
                  :type "js"
                  :directory '(:relative "static")))
(restas:define-route jquery.js ("js/jquery.js"
                             :content-type "application/javascript")
  (local-pathname :name "jquery"
                  :type "js"
                  :directory '(:relative "static")))

(defun head ()
  (format nil "
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <!-- Bootstrap -->
    <link href=\"~a\" rel=\"stylesheet\">

    <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
      <script src=\"https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js\"></script>
      <script src=\"https://oss.maxcdn.com/libs/respond.js/1.3.0/respond.min.js\"></script>
    <![endif]-->
	<!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    <script src=\"~a\"></script>
    <!-- Include all compiled plugins (below), or include individual files as needed -->
    <script src=\"~a\"></script>
"
          (restas:genurl *bootstrap.css*)
          (restas:genurl *jquery.js*)
          (restas:genurl *bootstrap.js*)))

(defun time-to-string (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (format nil "~2,'0d.~2,'0d.~a ~d:~2,'0d" day month year hour min)))

(defun tmpl-article (article)
  (with-slots (title authors tags category date text) article
    (format nil
          "<!DOCTYPE html>
<html>
  <head>
    <title>~a</title>
    ~a
  </head>
 <body>
   
   <div id=\"wrap\">
     
     ~a

     <!-- Begin page content -->
     <div class=\"container\">
       <div class=\"page-header\">
	 <h1>~3:*~a~2*</h1>
	 <p class=\"lead\">~{~{<i><a href=\"~a\">~a</a></i>~}~^ ~}</p>
       </div>
       <p><b>~a</b></p>
~{       <p>~a</p>~^~%~}
       <p><b><a href=\"~a\">~a</a></b></p>
       <p>~{~{<a href=\"~a\">~a</a>~}~^ ~}</p>
     </div>
   </div>	
 </body>
</html>"
          title
          (head)
          (nav-bar)
          (mapcar (lambda (x)
                    (list (restas:genurl *author-page-symbol* :author (slot-value x 'name))
                           (slot-value x 'title)))
                  authors)
          (time-to-string date)
          (split-sequence:split-sequence #\Newline text :remove-empty-subseqs t)
          (restas:genurl *category-page-symbol* :category (slot-value category 'name))
          (slot-value category 'title)
          (mapcar (lambda (x)
                    (list (restas:genurl *tag-page-symbol* :tag (slot-value x 'name))
                          (slot-value x 'title)))
                  tags))))

(defun render-list-of-articles (articles stream)
  (loop for article in articles
     do
       (with-slots (name title description primary-author moreauthorsp category date) article
         (format stream
                 "
    <div style=\"margin:10px;padding:10px;border:1px solid black\">
      <p><b>~a</b></p>
      <p><b><a href=\"~a\">~a</a></b></p>
      <h2><a href=\"~a\">~a</a></h2>
      <p>~a</p>
      <p><i><a href=\"~a\">~a</a>~@[~* etc.~]</i></p>
    </div>"
                 (time-to-string date)
                 (restas:genurl *category-page-symbol* :category (slot-value category 'name))
                 (slot-value category 'title)
                 (restas:genurl *article-page-symbol* :article name)
                 title
                 description
                 (restas:genurl *author-page-symbol* :author (slot-value primary-author 'name))
                 (slot-value primary-author 'title)
                 moreauthorsp))))

(defun render-list-of-articles-for-main (articles stream)
  (loop for article in articles
     do
       (with-slots (name title logo description primary-author moreauthorsp category date) article
         (format stream
                 "
    <div class=\"col-6 col-sm-6 col-lg-4\">
      <a style=\"text-decoration: none; color: #000000;\" href=\"~a\" >
        <h2>~a</h2>
        <h4>~a</h4>
        <a href=\"~3:*~a~2*\" class=\"thumbnail\">
          <img src=\"~a\" alt=\"...\">
        </a>
        <p>~a</p>
        <h4 style=\"position:relative;\">
          <span class=\"label label-warning\" style=\"position:absolute; left:0;\"><a style=\"text-decoration: none; color: white;\" href=\"~a\">~a</a></span>
          <span class=\"label label-default\" style=\"position:absolute; right:0;\"><a style=\"text-decoration: none; color: white;\" href=\"~a\">~a</a>~@[~* etc.~]</span>
        </h4>
      </a>
    </div>"
                 (restas:genurl *article-page-symbol* :article name)
                 title
                 (time-to-string date)
                 logo
                 description
                 (restas:genurl *category-page-symbol* :category (slot-value category 'name))
                 (slot-value category 'title)
                 (restas:genurl *author-page-symbol* :author (slot-value primary-author 'name))
                 (slot-value primary-author 'title)
                 moreauthorsp))))

(defun tmpl-main (articles)
  (with-output-to-string (out)
    (format out "<!DOCTYPE html>
<html>
  <head>
    <title>Golpito - своевременный удар</title>
    ~a

  </head>
 <body>
   
   <div id=\"wrap\">
     
     <!-- Begin page content -->
     <div class=\"container\">
       <div class=\"page-header\">
         <h1>Golpito - своевременный удар</h1>
       </div>
     </div>
     <div id=\"myCarousel\" class=\"carousel slide\" data-ride=\"carousel\">
     <!-- Indicators -->
       <ol class=\"carousel-indicators\">
         <li data-target=\"#myCarousel\" data-slide-to=\"0\" class=\"active\"></li>
~{
         <li data-target=\"#myCarousel\" data-slide-to=\"~a\" class=\"\"></li>
~}
       </ol>
       <div class=\"carousel-inner\" >
~{
         <div class=\"item active\" style=\"height:400px; overflow:hidden;\">
           <img src=\"~a\" style=\"width: 100%;\" alt=\"logo\" />
           <div class=\"container\">
             <div class=\"carousel-caption\">
               <h4><a href=\"~a\">~a</a></h4>
               <p>~a</p>
             </div>
           </div>
         </div>
~}
~{~{
         <div class=\"item\" style=\"height:400px; overflow:hidden;\">
           <img src=\"~a\" style=\"width: 100%;\" alt=\"logo\" />
           <div class=\"container\">
             <div class=\"carousel-caption\">
               <h4><a href=\"~a\">~a</a></h4>
               <p>~a</p>
             </div>
           </div>
         </div>
~}~}
      </div>
      <a class=\"left carousel-control\" href=\"#myCarousel\" data-slide=\"prev\"><span></span></a>
      <a class=\"right carousel-control\" href=\"#myCarousel\" data-slide=\"next\"><span></span></a>
    </div>
     <div class=\"container\">
"
            (head)
            (loop for i from 1 to (length (cdr articles)) collect i)
            (with-slots (logo name title description) (car articles)
              (list logo (restas:genurl *article-page-symbol* :article name) title description))
            (mapcar (lambda (article)
                      (with-slots (logo name title description) article
                        (list logo (restas:genurl *article-page-symbol* :article name) title description)))
                    (cdr articles)))
    (render-list-of-articles-for-main articles out)
    (format out "
     </div>
    </div>
  </body>
</html>")))

(defun tmpl-list-page (category articles)
  (with-output-to-string (out)
    (if (listp category)
        (format out "<!DOCTYPE html>
<html>
  <head>
    <title>~a</title>
    ~a
  </head>
 <body>
   
   <div id=\"wrap\">
     
     ~a

     <!-- Begin page content -->
     <div class=\"container\">
       <div class=\"page-header\">
         <h1>~3:*~{~{~a: ~a~}~^ :: ~}~2*</h1>
       </div>
"
                (destructuring-bind (author tag category) category
                  (remove-if #'null
                             (list
                              (when author
                                (list "Автор" (slot-value author 'title)))
                              (when tag
                                (list "Тег" (slot-value tag 'title)))
                              (when category
                                (list "Категория" (slot-value category 'title))))))
                (head)
                (nav-bar))
        (format out "<!DOCTYPE html>
<html>
  <head>
    <title>~a</title>
    ~a
  </head>
 <body>
   
   <div id=\"wrap\">
     
     ~a

     <!-- Begin page content -->
     <div class=\"container\">
       <div class=\"page-header\">
         <h1>~3:*~a~2*</h1>
       </div>
~{       <p>~a</p>~%~}"
                (slot-value category 'title)
                (head)
                (nav-bar)
                (split-sequence:split-sequence #\Newline (slot-value category 'description) :remove-empty-subseqs t)))
    (render-list-of-articles articles out)
    (format out "
      </div>
    </div>
  </body>
</html>")))