(defpackage #:nntask2
  (:use #:cl)
  (:export #:nntask2))


(in-package #:nntask2)


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (format out "~a~%" data)))


(defun nntask2 ()
  (let (output cyclic? graph prefix)
    (when (not (nntask1:nntask1))
      (return-from nntask2))
    (setq graph (graph-io:parse-xml-graph "temp")
          cyclic? (graph-aux:check-cycle graph))
    (ignore-errors (uiop:run-program (format nil "rm ~a" "temp")))
    (when cyclic?
      (format t "~2%В графе присутствует цикл: ~{~a ~^-~^> ~}"
              (mapcar #'1+ cyclic?))
      (format t "~2%Представление функции в префиксной нотации не может быть получено.")
      (return-from nntask2))
    (format t "~2%Введите имя файла, в который будет записан граф в префиксной нотации: ")
    (setq output (read-line)
          prefix (graph-aux:get-prefix-notation graph))
    (format t "~%Получившаяся префиксная нотация: ~a." prefix)
    (write-to-file prefix output) t))
