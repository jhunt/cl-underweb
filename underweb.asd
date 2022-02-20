#|
 | underweb.asd
 | A web framework built on Hunchentoot
 | © 2022 James Hunt
 | 19 Feb 2022
 |#
(let ((me "James Hunt <james@jameshunt.us>"))
  (asdf:defsystem #:underweb
    :version "0.0.1" :license "mit"
    :author me :maintainer me
    :description "A web framework built on Hunchentoot"
    :homepage "https://jameshunt.us/p/underweb"
    :serial t
    :depends-on (:cl-json
                 :hunchentoot)
    :components ((:file "package")
                 (:file "underweb"))))
