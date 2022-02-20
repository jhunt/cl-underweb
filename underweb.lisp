;;;
;;; underweb.lisp
;;; ©2022 James Hunt
;;;
;;; Underweb is a micro-framework sitting
;;; atop hunchentoot that gives a more
;;; Sinatra-like / MERB-ish feel to the
;;; definition of JSON-over-REST web APIs.
;;;

(in-package #:underweb)

(eval-when (:compile-toplevel :load-toplevel)
  (defun internf (fmt &rest args)
    (intern (apply #'format
                   (append (list nil fmt)
                           args)))))

(defvar *underweb-server-signature*
        (format nil "underweb/~A"
                (asdf:component-version
                  (asdf:find-system 'underweb))))

(defun split-path (in)
  (loop for i = 0 then (1+ j)
        as j = (position #\/ in :start i)
        collect (subseq in i j)
        while j))

(defun is-patharg? (arg)
  (eq (position #\: arg) 0))

(defun paths-match (got want &optional args)
  (cond ((and (null got)
              (null want))
         (values t (reverse args)))

        ((or (null got)
             (null want))
         (values nil nil))

        ((is-patharg? (car want))
         (paths-match (cdr got)
                      (cdr want)
                      (cons (car got) args)))

        ((equal (car want)
                (car got))
         (paths-match (cdr got)
                      (cdr want)
                      args))

        (t
         (values nil nil))))

(defun path-matches (got want)
  (paths-match (split-path got)
               (split-path want)))

(defun respond-in-json (with)
  "Set Content-Type: application/json and emit JSONified content"
  (setf (hunchentoot:content-type*) "application/json")
  (format nil "~A~%" (json:encode-json-to-string with)))

(defclass underweb-acceptor (hunchentoot:easy-acceptor)
  ((dispatch-table
    :initform '()
    :initarg :dispatch-table
    :accessor dispatch-table
    :documentation "Inbound request dispatcher functions"))
  (:default-initargs
   :dispatch-table
   (list (lambda (req)
           (declare (ignore req))
           (lambda (request)
             (declare (ignore request))
             (setf (hunchentoot:return-code*) 404)
             `((error . "endpoint-not-found")))))))

(defmethod
  hunchentoot:acceptor-dispatch-request
  ((underweb underweb-acceptor) request)
  (mapc
    (lambda (dispatcher)
      (let ((handler (funcall dispatcher request)))
        (when handler
          (setf (hunchentoot:header-out :server)
                *underweb-server-signature*)
          (return-from hunchentoot:acceptor-dispatch-request
                       (respond-in-json
                         (funcall handler request))))))
    (dispatch-table underweb))
  (call-next-method))

(defparameter *underweb* nil)

(defun start-server (&key (port 3001))
  (let ((server (make-instance 'underweb-acceptor
                               :port port)))
    (setf *underweb* server)
    (hunchentoot:start server)
    server))

(defun define-method+uri-dispatcher (method uri handler)
  (lambda (request)
    (when (eq method (hunchentoot:request-method request))
      (multiple-value-bind (match? args)
        (path-matches (hunchentoot:script-name request) uri)
        (if match?
          (lambda (request)
            (apply handler (cons request args))))))))

(defmacro set-handler (method uri args &body body)
  (let ((request (gensym)))
    `(push
       (define-method+uri-dispatcher
         ,method ,uri
         (lambda (,request ,@args)
           (declare (ignore ,request))
           (handler-case
             (progn ,@body)
             (t (c) `((error . ,(format nil "~A" c)))))))
       (dispatch-table *underweb*))))

(defmacro def-set-handlers (&rest methods)
  `(progn
     ,@(mapcar
         (lambda (method)
           (let ((macro-name  (internf "~A!" method)))
             `(defmacro ,macro-name (uri args &body body)
                `(set-handler ,,method ,uri ,args ,@body))))
         methods)))

(def-set-handlers :GET :PUT :POST :PATCH :DELETE :OPTIONS)

(defmacro auth-via (auth-handler &body body)
  `(let ((ok (funcall ,auth-handler)))
     (cond (ok ,@body)
           (t (setf (hunchentoot:return-code*)
                    (if (hunchentoot:header-in* :authorization)
                      403 401))
              `((error . "not authorized"))))))

(defmacro on (&body forms)
  (let ((c-type (gensym)))
    `(let ((,c-type (request-header :content-type)))
       (cond ,@(mapcar
                 #'(lambda (form)
                     `((equal ,c-type ,(car form))
                       ,@(cdr form)))
                 forms)
             (t `((un . known)))))))

(defun status (code)
  (setf (hunchentoot:return-code*) code))

(defun request-header (alias)
  (hunchentoot:header-in* alias))

(defun request-json ()
  (json:decode-json-from-string
    (hunchentoot:raw-post-data :force-text t)))

(defun request-text ()
  (hunchentoot:raw-post-data :force-text t))

(defun post-params ()
  (hunchentoot:post-parameters*))

(defun post-param (which)
  (hunchentoot:post-parameter which))
