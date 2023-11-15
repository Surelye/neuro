(defpackage #:eval
  (:use #:cl)
  (:export #:eval-expression))


(in-package #:eval)


(defun eval-expression (prefix ops)
  (let ((vertex-mapping (make-hash-table)))
    (labels ((subs (expr)
               (if (listp expr)
                   (mapcar #'subs expr)
                   (gethash expr vertex-mapping))))
      (dolist (mapping (mapcar #'(lambda (op-str)
                                   (uiop:split-string op-str :separator ":")) ops)
                       (eval (subs prefix)))
        (destructuring-bind (vertex op-str) mapping
          (setf (gethash (parse-integer vertex) vertex-mapping)
                (cond ((find #\+ op-str) '+)
                      ((find #\* op-str) '*)
                      ((search "exp" op-str) 'exp)
                      (t (read-from-string op-str)))))))))
