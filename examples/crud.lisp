(require 'underweb)
(use-package 'underweb)

(start-server :port 5092)
(defvar *things* (make-hash-table :test #'equal))

(get! "/things" ()
  (loop for v being each hash-value in *things* collecting v))

(post! "/things" ()
  (let* ((thing (request-json))
         (id    (cdr (assoc :id thing))))
    (setf (gethash id *things*) thing)
    thing))

(get! "/things/:id" (id)
  (gethash id *things*))

(put! "/things/:id" (id)
  (let ((thing (request-json)))
    (setf (gethash id *things*) thing)
    thing))

(patch! "/things/:id" (id)
  (let ((thing    (request-json))
        (existing (gethash id *things*)))
    (loop for (k . v) in thing do
          (if (assoc k existing)
              (rplacd (assoc k existing) v)
              (setf existing (acons k v existing))))
    (setf (gethash id *things*) existing)
    existing))

(delete! "/things/:id" (id)
  (remhash id *things*)
  `((ok . deleted)))
