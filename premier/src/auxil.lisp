(defpackage #:aux
  (:use #:cl)
  (:export #:write-to-file
           #:get-output-filename
           #:read-from-file
           #:parse-file-content))

(in-package #:aux)


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (datum data)
      (format out "~a~%" datum))))


(defun get-output-filename ()
  (let ((output-filename))
    (format t "~%Введите имя файла, в который будет записан граф (по умолчанию output): ")
    (setq output-filename (read-line))
    (when (zerop (length output-filename))
      (setq output-filename "output")) output-filename))


(defun read-from-file ()
  (format t "~%Введите имя файла с описанием графа в виде списка дуг (по умолчанию input): ")
  (let ((filename))
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename))
         (setq filename "input"))
       (when (not (uiop:file-exists-p filename))
         (format t "~%Файла с заданным именем не существует! Введите имя файла снова: ")
         (go try-again)))
    (uiop:read-file-lines filename)))


(defun extract-arcs (str j)
  "Функция извлечения отдельной дуги из строки с дугами. После извлечения дуги
   из строки, осуществляется проверка корректности дуги согласно условиям 1-5.
   Переменные pos-#\( и pos-#\) содержат индексы символов ( и ) в текущей
   обрабатываемой строке."
  (let ((pos-#\() (pos-#\)) (arc) (arcs) (ok-arc?))
    (loop
      (setq pos-#\( (position #\( str)
            pos-#\) (position #\) str))
      (when (null pos-#\()
        (setq arcs (reverse arcs)) (return))
      (setq arc (read-from-string (subseq str pos-#\( (1+ pos-#\))))
            ok-arc? (err:ok-arc? arc arcs))
      (if (integerp ok-arc?) ; ok-arc? содержит либо код ошибки для текущей
                             ; проверяемой дуги, либо NIL. В случае, если код
                             ; ошибки, список *rows* пополняется соответствующей
                             ; записью.
          (setq err:*rows* (cons (list ok-arc? j arc) err:*rows*))
          (setq arcs (cons arc arcs)))
      (setq str (subseq str (1+ pos-#\))))) arcs))


(defun parse-file-content (content)
  "Построчный парсер содержимого файла с описанием графа."
  (setq content (mapcar #'(lambda (lst) (remove #\, lst)) content)
        content (mapcar #'extract-arcs content
                        (loop for j from 1 to (length content)
                              collect j))))
