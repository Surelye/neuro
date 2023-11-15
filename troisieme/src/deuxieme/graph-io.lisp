(defpackage #:graph-io
  (:use #:cl)
  (:export #:parse-xml-graph))


(in-package #:graph-io)


(defun parse-xml-graph (filename)
  (flet ((parse (line)
           (parse-integer
            (subseq line (1+ (position #\> line)) (1- (position #\/ line))))))
    (let ((xml-graph (uiop:read-file-lines filename)) arc
          (graph (make-hash-table)) from to)
      (dolist (line xml-graph graph)
        (cond ((or (search "from" line) (search "to" line) (search "order" line))
               (setq arc (cons (parse line) arc)))
              ((search "/arc" line)
               (progn
                 (setq from (car (last arc)) to (cadr arc))
                 (if (gethash from graph)
                     (setf (gethash from graph) (cons (list to (car arc))
                                                      (gethash from graph)))
                     (setf (gethash from graph) (list (list to (car arc)))))
                 (unless (gethash to graph)
                   (setf (gethash to graph) nil))
                 (setq arc nil)))
              (t 'SKIP))))))
