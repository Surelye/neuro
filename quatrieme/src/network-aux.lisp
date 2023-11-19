(defpackage #:net
  (:use #:cl)
  (:export #:build-network
           #:save-network-as-xml
           #:check-correct-network
           #:display-errors
           #:feed-network))


(in-package #:net)


(defun f (x)
  (/ (* 1.0 x) (1+ (abs x))))


(defun display-errors (errors)
  (flet ((form (str param)
           (concatenate 'string (format nil "Ошибка в строке ~a: " (1+ param)) str)))
    (dolist (err errors)
      (if (atom err)
          (format t "Некорректное содержимое входного вектора.~%")
          (destructuring-bind (err-type aux) err
            (format t (cond ((equal 'VEC-DIM err-type) (format nil "Некорректная размерность входного вектора (должна быть ~a).~%" aux))
                            ((equal 'IN-LAYER-DIM err-type) (form "некорректная размерность массива связей внутри слоя.~%" aux))
                            ((equal 'WEIGHT err-type) (form "некорректное значение веса.~%" aux))
                            (t (form "несовпадение размерностей текущего и следующего слоя.~%" aux)))))))))


(defun save-network-as-xml (network filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (format out "<network>~%")
    (dolist (layer network)
      (format out "~4t<layer>~%")
      (dolist (connections layer)
        (format out "~8t<connections>~%")
        (dolist (weight connections)
          (format out "~12t<weight>~a</weight>~%" weight))
        (format out "~8t</connections>~%"))
      (format out "~4t</layer>~%"))
    (format out "</network>~%")))


(defun check-correct-network (net init-vec)
  (let (num-cols cur-layer (num-checks (length net)) errors)
    (when (/= (length init-vec) (length (caar net)))
      (setq errors (cons (list 'VEC-DIM (length (caar net))) errors)))
    (do ((j 0 (1+ j))) ((= j num-checks) errors)
      (setq cur-layer (nth j net)
            num-cols (length (car cur-layer)))
      (unless (every #'(lambda (seq)
                         (= num-cols (length seq)))
                     (cdr cur-layer))
        (setq errors (cons (list 'IN-LAYER-DIM j) errors)))
      (unless (every #'(lambda (seq)
                         (every #'integerp seq))
                     cur-layer)
        (setq errors (cons (list 'WEIGHT j) errors)))
      (when (/= j (1- num-checks))
        (unless (= (length cur-layer) (length (car (nth (1+ j) net))))
          (setq errors (cons (list 'LAYERS-DIM j) errors)))))))


(defun feed-network (net init-vec)
  (flet ((feed-layer (layer vec)
           (mapcar #'f
                   (mapcar #'(lambda (weights)
                               (apply #'+
                                      (mapcar #'* weights vec)))
                           layer))))
    (let ((vec (copy-list init-vec)))
      (dolist (layer net vec)
        (setq vec (feed-layer layer vec))))))


(defun build-network (matrix)
  (flet ((extract-layer (str-layer)
           (let ([-pos ]-pos connections layer)
             (loop
               (setq [-pos (position #\[ str-layer))
               (when (null [-pos)
                 (return))
               (setq ]-pos (position #\] str-layer)
                     connections (subseq str-layer (1+ [-pos) ]-pos)
                     str-layer (subseq str-layer (1+ ]-pos))
                     layer (cons (read-from-string (concatenate 'string "(" connections ")")) layer)))
             (reverse layer))))
    (mapcar #'extract-layer matrix)))
