(asdf:defsystem golpito
    :depends-on (#:restas
                 #:postmodern
                 #:closure-template
                 #:sqlite)
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