(defpackage #:graph-aux
  (:use #:cl)
  (:export #:check-cycle
           #:get-prefix-notation))


(in-package #:graph-aux)


(defvar *cycle-start*)
(defvar *cycle-end*)
(defvar *colors*)
(defvar *preds*)


(defun dfs (vertex graph)
  (let (to)
    (setf (aref *colors* (1- vertex)) :grey)
    (dolist (u (gethash vertex graph))
      (setq to (1- (car u)))
      (when (eql (aref *colors* to) :white)
        (setf (aref *preds* to) (1- vertex))
        (when (dfs (1+ to) graph)
          (return-from dfs t)))
      (when (eql (aref *colors* to) :grey)
        (setf *cycle-end* (1- vertex)
              *cycle-start* to)
        (return-from dfs t)))
    (setf (aref *colors* (1- vertex)) :black)
    (return-from dfs)))


(defun check-cycle (graph)
  (setf *cycle-start*  -1 *cycle-end* nil
        *colors*      nil *preds*     nil)
  (let* ((size (hash-table-count graph)) cycle)
    (setf *colors* (make-array size :initial-element :white)
          *preds*  (make-array size :initial-element -1))
    (loop for key being the hash-keys of graph
          do (when (dfs key graph)
               (return)))
    (when (= -1 *cycle-start*)
      (return-from check-cycle))
    (setq cycle (cons *cycle-start* cycle))
    (do ((v *cycle-end* (aref *preds* v)))
        ((= *cycle-start* v) (return-from check-cycle (cons *cycle-start* cycle)))
      (setq cycle (cons v cycle)))))


(defun gnlp (expr) ; gnlp -> get-non-lisp-prefix
  (flet ((wts (val)
           (write-to-string val))
         (conc (&rest args)
           (reduce #'(lambda (f s)
                       (concatenate 'string f s)) args)))
    (let ((f (first expr)) (s (second expr)) (th (third expr)))
      (conc (wts f) "("
            (if (null th)
                (if (atom s)
                    (wts s)
                    (gnlp s))
                (if (atom s)
                    (if (atom th)
                        (conc (wts s) ", " (wts th))
                        (conc (wts s) ", " (gnlp th)))
                    (if (atom th)
                        (conc (gnlp s) ", " (wts th))
                        (conc (gnlp s) ", " (gnlp th))))) ")"))))


(defun get-prefix-notation (graph)
  (let ((inverse-graph (make-hash-table)) final-vertex to)
    (maphash #'(lambda (key value)
                 (dolist (u value)
                   (if (gethash (setq to (car u)) inverse-graph)
                       (setf (gethash to inverse-graph)
                             (cons key (gethash to inverse-graph)))
                       (setf (gethash to inverse-graph) (list key)))
                   (unless (gethash key inverse-graph)
                     (setf (gethash key inverse-graph) nil)))) graph)
    (setq final-vertex (hash-table-count inverse-graph))
    (labels ((subs (vertex)
               (let ((adj (gethash vertex inverse-graph)))
                 (if (null adj)
                     vertex
                     (cons vertex (mapcar #'subs adj))))))
      (concatenate 'string "(" (gnlp (subs final-vertex)) ")"))))
