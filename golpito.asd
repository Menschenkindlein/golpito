(asdf:defsystem golpito
    :depends-on (#:restas
;;                 #:closure-template
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
                          ((:file "templates")
                           (:file "admin-templates")))
                 (:module "controller"
                          :serial t
                          :components
                          ((:file "package")
                           (:file "routes")
                           (:file "admin-routes")))
                 (:file "init")
                 ))