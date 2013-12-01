(load "quicklisp/setup.lisp")
(push #p"./" asdf:*central-registry*)
(require :golpito-runtime)
(restas:start :golpito)
