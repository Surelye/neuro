(defpackage #:nntask4
  (:use #:cl)
  (:export #:nntask4))


(in-package #:nntask4)


(defun nntask4 ()
  (flet ((get-filename ()
           (let (filename)
             (loop
               (setq filename (read-line))
               (unless (uiop:file-exists-p filename)
                 (format t "~%Файла с указанным именем не существует! Попробуйте ввести имя файла снова: "))
               (return)) filename)))
    (let (matrix vector network result)
      (format t "~%Введите имя файла, в котором содержится набор матриц весов: ")
      (setq matrix (uiop:read-file-lines (get-filename)))
      (format t "~%Введите имя файла, в котором содержится входной вектор: ")
      (setq vector (uiop:split-string (uiop:read-file-line (get-filename)) :separator " ")
            network (net:build-network matrix))
      (handler-case (setq vector (mapcar #'parse-integer vector))
        (error ()
          (format t "~%Некорректное значение входного вектора!")
          (return-from nntask4)))
      (unless (setq result (net:feed-network network vector))
        (format t "~%Некорректно заданы измерения многослойной нейронной сети или её веса!")
        (return-from nntask4))
      (format t "~%Введите имя файла, в который будет записана НС: ")
      (net:save-network-as-xml network (read-line))
      (format t "~%Введите имя файла, в который будет записан результат вычисления вектора НС: ")
      (with-open-file (out (read-line) :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
        (format out "~{~a~^, ~}~%" result))) t))
