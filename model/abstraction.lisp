(defpackage #:golpito.model.abstraction
  (:use #:cl)
  (:export #:defmodel))

(in-package #:golpito.model.abstraction)

(defmacro defmodel (name supermodels slots &rest options)
  (labels ((to-keyword (symbol)
             (intern (string symbol) :keyword))
           (mkslot (control-string symbol)
             (list (intern (format nil (string-upcase control-string) symbol))
                   :initarg (intern (format nil (string-upcase control-string) symbol) :keyword))))
    `(progn
       ;; DEFCLASS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defclass ,name ,supermodels
         ,(loop for (slot . options) in slots
             if (and options
                     (eq (first options) :list)
                     (eq (third options) :has-first))
             collect (mkslot "primary-~a" (second options))
             and collect (mkslot "more~asp" (second options))
             ;anyway
             collect (list slot :initarg (to-keyword slot))))
       ;; Make tables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       )))
