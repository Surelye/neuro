(defpackage #:nntask3
  (:use #:cl)
  (:export #:nntask3))


(in-package #:nntask3)


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (format out "~a~%" data)))


(defun nntask3 ()
  (let (prefix input ops res output)
    (when (not (setq input (nntask2:nntask2)))
      (return-from nntask3))
    (setq prefix (read-from-string (uiop:read-file-line input)))
    (format t "~%Введите имя файла, в котором содержатся арифметические операции: ")
    (setq ops (uiop:read-file-lines (read-line))
          res (eval:eval-expression prefix ops))
    (format t "~%Введите имя файла, в который будет записан результат вычислений: ")
    (setq output (read-line))
    (format t "~%Результат вычислений равен: ~a.~%" res)
    (write-to-file (concatenate 'string "(" (graph-aux:gnlp prefix) ")") input)
    (write-to-file res output) t))
