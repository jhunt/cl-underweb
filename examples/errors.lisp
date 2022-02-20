(require 'underweb)
(use-package 'underweb)

(start-server :port 5093)

(get! "/:a/x/:b" (a b)
  (* (parse-integer a)
     (parse-integer b)))

(get! "/safe/:a/x/:b" (a b)
  (handler-case
    (* (parse-integer a)
       (parse-integer b)) 
    (condition () `((oops . "that shouldn't have happened...")))))
