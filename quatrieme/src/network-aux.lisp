(defpackage #:net
  (:use #:cl)
  (:export #:build-network
           #:save-network-as-xml
           #:feed-network))


(in-package #:net)


(defun f (x)
  (/ (* 1.0 x) (1+ (abs x))))


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
  (when (/= (length init-vec) (length (car net)))
    (return-from check-correct-network))
  (let (num-cols cur-layer (num-checks (1- (length net))))
    (do ((j 0 (1+ j))) ((= j num-checks) t)
      (setq cur-layer (nth j net)
            num-cols (length (car cur-layer)))
      (unless (every #'(lambda (seq)
                         (and (= num-cols (length seq))
                              (every #'integerp seq)))
                     (cdr cur-layer))
        (return-from check-correct-network))
      (unless (= num-cols (length (nth (1+ j) net)))
        (return-from check-correct-network)))))


(defun feed-network (net init-vec)
  (unless (check-correct-network net init-vec)
    (return-from feed-network))
  (flet ((feed-layer (layer vec)
           (let ((len-vec (length vec)) (len-conns (length (car layer)))
                 out-vec temp)
             (do ((j 0 (1+ j))) ((= j len-conns) (mapcar #'f (reverse out-vec)))
               (setq temp 0)
               (do ((k 0 (1+ k))) ((= k len-vec) (setq out-vec (cons temp out-vec)))
                 (setq temp (+ (* (nth k vec) (nth j (nth k layer))) temp)))))))
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
