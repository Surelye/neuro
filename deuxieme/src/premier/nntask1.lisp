(defpackage #:nntask1
  (:use #:cl)
  (:export #:nntask1))

(in-package #:nntask1)


(defun nntask1 ()
  "Основная функция вызова. После парсинга и проверки корректности дуг сравниванет
   список *rows* -- список некорректных дуг -- с пустым списком. Если сравнение
   успешно, описание графа не содержит ошибок, и его возможно представить в
   формате xml."
  (setq err:*rows* nil)
  (let ((arcs (aux:parse-file-content (aux:read-from-file))))
    (err:ok-arcs? arcs)
    (format t "~2%~35t[ОШИБКИ]~%")
    (if err:*rows*
        (progn (err:print-errors)
               (format t "~2%Поскольку в текстовом файле с описанием графа были найдены ошибки, граф не может быть построен.")
               (return-from nntask1 nil))
        (format t "~%Ошибки в файле с описанием графа не найдены."))
    (xml:save-as-xml (reduce #'append arcs) "temp") t))
