(defpackage #:err
  (:use #:cl)
  (:export #:*rows*
           #:ok-arc?
           #:ok-arcs?
           #:print-errors))

(in-package #:err)


(defvar *rows* nil) ; В этой глобальной переменной будут храниться дуги, не
                    ; прошедшие проверку корректности согласно типам ошибок
                    ; 1-7. Формат элемента в *rows*: (тип, номер строки, дуга)


(defun print-errors ()
  "Функция печати соответствующего типа ошибки для каждой дуги, попавшей в
   *rows*."
  (let ((len (length *rows*)))
    (do ((j 0 (1+ j))) ((= len j))
      (destructuring-bind (type row-num arc) (nth j *rows*)
        (format t (cond ((= 1 type) "~%~4tОшибка в строке ~d: неправильно задана компонента: (~{~a~^, ~});")
                        ((= 2 type) "~%~4tОшибка в строке ~d: повторяющаяся дуга a -> b: (~{~a~^, ~});")
                        ((= 3 type) "~%~4tОшибка в строке ~d: некорректные данные (введены буквы вместо цифр): (~{~a~^, ~});")
                        ((= 4 type) "~%~4tОшибка в строке ~d: дуга с номером n в вершину b уже существует: (~{~a~^, ~});")
                        ((= 5 type) "~%~4tОшибка в строке ~d: вершины 0 быть не может: (~{~a~^, ~});")
                        ((= 6 type) "~%~4tОшибка в строке ~d: неправильная нумерация вершин. Номер вершины a больше количества вершин: (~{~a~^, ~});")
                        (t          "~%~4tОшибка в строке ~d: неправильно задан номер дуги: (~{~a~^, ~});"))
                row-num arc)))))


(defun check-error-type-1 (arc)
  "Функция проверки корректности компоненты. Компонента считается корректной,
   если её длина равна 3. Если компонента задана некорректно, то результатом
   работы функции является код соответствующей ошибки -- 1."
  (and (/= 3 (length arc))
       1))


(defun check-error-type-2 (arc arcs)
  "Функция проверки вхождения дуги arc в список уникальных дуг arcs. При
   вхождении возвращается код ошибки -- 2."
  (and (member (list (car arc) (cadr arc)) arcs
               :key #'(lambda (arc)
                        (list (car arc) (cadr arc)))
               :test #'equal)
       2))


(defun check-error-type-3 (arc)
  "Функция проверки корректности данных в дуге arc. Если хотя бы один из
   параметров дуги не является числом, возвращается код ошибки -- 3."
  (and (not (and (integerp (car   arc))
                 (integerp (cadr  arc))
                 (integerp (caddr arc))))
       3))


(defun check-error-type-4 (arc arcs)
  "Функция проверки вхождения дуги arc с номером n в вершину b в список дуг
   arcs. При вхождении возвращается код ошибки -- 4."
  (and (member (list (cadr arc) (caddr arc)) arcs
               :key #'(lambda (arc)
                        (list (cadr arc) (caddr arc)))
               :test #'equal)
       4))


(defun check-error-type-5 (arc)
  "Функция проверки равенства значения вершин дуги нулю. В случае, если хотя бы
   одна из вершин равна нулю, возвращается код ошибки -- 5."
  (and (or (zerop (car  arc))
           (zerop (cadr arc)))
       5))


(defun ok-arc? (arc arcs)
  "Функция проверки корректности дуги первым пяти условиям."
  (or (check-error-type-1 arc     )
      (check-error-type-2 arc arcs)
      (check-error-type-3 arc     )
      (check-error-type-4 arc arcs)
      (check-error-type-5 arc     )))


(defun check-error-type-6 (arcs)
  "Функция проверки корректности нумерации вершин. Параметр a дуги arc считается
   заданным корректно, если его значение не превосходит максимального значения
   параметра b среди всех дуг arc в списке arcs."
  (let* ((max-vertex (apply #'max (mapcar #'cadr (reduce #'append arcs)))) (rows)
         ;; Переменная arc-checker -- функция, осуществляющая сравнение параметра
         ;; а каждой дуги arc со значением max-vertex -- максимальным значением b.
         ;; Если сравнение выполняется, в список с некорректными строками rows
         ;; добавляется список в формате (6, row, arc), где 6 -- тип ошибки,
         ;; row -- номер строки с некорректной дугой, arc -- сама некорректная дуга.
         (arc-checker #'(lambda (arc j)
                          (when (> (car arc) max-vertex)
                            (setq rows (cons (list 6 j arc) rows))))))
    (mapcar #'(lambda (arc-row j) ; mapcar описывает правило применения некоторой функции
                                  ; (в этом случае lambda ...) к некоторому списку.
                (mapcar #'(lambda (arc)
                            (funcall arc-checker arc j)) arc-row)) ; внутри lambda происходит вызов arc-checker
            arcs (loop for j from 1 to (length arcs) collect j)) rows))


(defun check-error-type-7 (arcs)
  "Функция проверки корректности задания номеров дуг. Проверка осуществляется
   при помощи построения словаря, содержащего значения номеров дуг для каждой
   вершины b. Для вершины b номера дуг считаются заданными корректно, если
   список номеров дуг этой вершины содержит те же элементы, что и перечисление
   1, 2, ..., максимальный-номер-дуги-для-вершины-b."
  (let ((arc-nums (make-hash-table :test #'equal)) (len (length arcs))
        (len-row) (arc-row) (vertex) (nums) (rows))
    (do ((k 0 (1+ k))) ((= len k))
      (setq arc-row (nth k arcs) len-row (length arc-row))
      (do ((j 0 (1+ j))) ((= len-row j))
        (destructuring-bind (a b n) (nth j arc-row)
          (setq vertex b)
          (if (gethash vertex arc-nums)
              (setf (gethash vertex arc-nums) (cons (cons (list k a) n)
                                                    (gethash vertex arc-nums)))
              (setf (gethash vertex arc-nums) (list (cons (list k a) n)))))))
    (loop for key being the hash-keys of arc-nums using (hash-value value)
          do (setq nums (sort (mapcar #'cdr value) #'>))
             (unless (equal nums (loop for j from (car nums) downto 1
                                       collect j))
               (setq rows (cons (list 7 (1+ (caaar value))
                                      (list (cadaar value) key (cdar value))) rows))))
    rows))


(defun ok-arcs? (arcs)
  "Функция проверки корректности дуги условиям 6 и 7. Если были найдены некорректные
   дуги, то они добавляются в список некорректных дуг *rows*."
  (setq *rows*
        (append (check-error-type-7 arcs)
                (check-error-type-6 arcs)
                *rows*)))
