(defpackage #:xml
  (:use #:cl)
  (:export #:save-as-xml))

(in-package #:xml)


(defun save-as-xml (arcs filename)
  "Функция сохранения графа в формате xml."
  (let ((vertices (remove-duplicates (mapcar #'car arcs) :from-end t)))
    (with-open-file (out filename :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
      (format out "<graph>~%")
      (dolist (vertex vertices)
        (format out "~4t<vertex>~d</vertex>~%" vertex))
      (dolist (arc arcs)
        (format out "~4t<arc>~%~8t<from>~d</from>~%~8t<to>~d</to>~%~8t<order>~d</order>~%~4t</arc>~%"
                (car arc) (cadr arc) (caddr arc)))
      (format out "</graph>~%")) t))
