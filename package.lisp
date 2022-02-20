;;;
;;; package.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines the UNDERWEB package, its
;;; implicitly-included downstream dependencies,
;;; and its exported functions, variables, etc.
;;;

(in-package #:cl-user)
(defpackage #:underweb
  (:use :cl)
  (:export :start-server
           :request-header
           :request-json
           :request-text
           :get!
           :post!
           :put!
           :patch!
           :delete!))
