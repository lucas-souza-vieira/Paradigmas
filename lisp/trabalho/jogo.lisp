(defparameter *n* 0)
(defparameter *values* nil)
(defparameter *regions* nil)
(defparameter *region-cells* (make-hash-table))

(defun in-bounds-p (i j)
  (and (>= i 0) (< i *n*) (>= j 0) (< j *n*)))

(defun build-region-cells ()
  (clrhash *region-cells*)
  (dotimes (i *n*)
    (dotimes (j *n*)
      (let ((region (nth j (nth i *regions*))))
        (push (list i j) (gethash region *region-cells*))))))

(defun get-value (i j)
  (nth j (nth i *values*)))

(defun set-value (i j val)
  (setf (nth j (nth i *values*)) val))

(defun is-valid (i j num)
  (let* ((region (nth j (nth i *regions*)))
         (cells (gethash region *region-cells*)))
    ;; 1. Número não pode repetir na mesma região
    (when (some (lambda (pos)
                  (= (get-value (first pos) (second pos)) num))
                cells)
      (return-from is-valid nil))
    ;; 2. Células ortogonalmente adjacentes não podem ter mesmo número
    (dolist (dir '((-1 0) (1 0) (0 -1) (0 1)))
      (let ((ni (+ i (first dir)))
            (nj (+ j (second dir))))
        (when (and (in-bounds-p ni nj)
                   (= (get-value ni nj) num))
          (return-from is-valid nil))))
    ;; 3. Ordem vertical na mesma região
    ;; acima
    (when (and (> i 0)
               (char= (nth j (nth (- i 1) *regions*)) region)
               (/= (get-value (- i 1) j) 0)
               (<= (get-value (- i 1) j) num))
      (return-from is-valid nil))
    ;; abaixo
    (when (and (< i (- *n* 1))
               (char= (nth j (nth (+ i 1) *regions*)) region)
               (/= (get-value (+ i 1) j) 0)
               (>= (get-value (+ i 1) j) num))
      (return-from is-valid nil))
    t))

(defun solve (&optional (row 0) (col 0))
  (cond
    ((= row *n*) t) ; solução completa
    ((= col *n*) (solve (1+ row) 0))
    ((/= (get-value row col) 0) (solve row (1+ col)))
    (t
     (let* ((region (nth col (nth row *regions*)))
            (region-size (length (gethash region *region-cells*))))
       (loop for num from 1 to region-size do
            (when (is-valid row col num)
              (set-value row col num)
              (when (solve row (1+ col))
                (return-from solve t))
              (set-value row col 0))))
     nil)))

(defun print-solution ()
  (dolist (row *values*)
    (format t "~{~A~^ ~}~%" row)))

(defun main ()
  (format t "Digite o valor de N: ") (setf *n* (read))
  (format t "Digite os valores (como lista Lisp): ") (setf *values* (read))
  (format t "Digite as regiões (como lista Lisp): ") (setf *regions* (read))
  (build-region-cells)
  (if (solve)
      (print-solution)
      (format t "Sem solução.~%")))

  
(main)
