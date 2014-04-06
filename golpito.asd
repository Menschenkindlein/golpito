(asdf:defsystem golpito
    :depends-on (#:restas
                 #:cl-ppcre
                 #:postmodern
                 #:closure-template
                 #:cl-markdown
                 #:ironclad)
    :serial t
    :components ((:module "model"
                          :serial t
                          :components
                          ((:file "abstraction")
                           (:file "model")
                           (:file "rasped")))
                 (:module "view"
                          :serial t
                          :components
                          ((:static-file "templates.soy")
                           (:static-file "admin-templates.soy")
                           (:static-file "helper.soy1")
                           (:file "compile-template")
                           (:file "templates")
                           (:file "admin-templates")))
                 (:module "controller"
                          :serial t
                          :components
                          ((:file "package")
                           (:file "routes")
                           (:file "admin-routes")))
                 ))