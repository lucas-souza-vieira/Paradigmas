(defparameter *n* 0)
(defparameter *values* nil)
(defparameter *regions* nil)
(defparameter *region-cells* (make-hash-table))

(defun in-bounds (i j)
  (and (>= i 0) (< i *n*) (>= j 0) (< j *n*)))

(defun get-value (i j)
  (nth j (nth i *values*)))

(defun set-value (i j val)
  (setf (nth j (nth i *values*)) val))

(defun region-id (i j)
  (nth j (nth i *regions*)))

(defun region-size (region)
  (length (gethash region *region-cells*)))

(defun cell-valid-p (i j num)
  (let ((region (region-id i j)))
    ;; não pode repetir na mesma região
    (when (some (lambda (cell)
                  (= (get-value (first cell) (second cell)) num))
                (gethash region *region-cells*))
      (return-from cell-valid-p nil))

    ;; ortogonalmente adjacentes diferentes
    (dolist (d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
      (let ((ni (+ i (car d)))
            (nj (+ j (cdr d))))
        (when (and (in-bounds ni nj)
                   (= (get-value ni nj) num))
          (return-from cell-valid-p nil))))

    ;; superior > inferior na mesma região
    (when (and (> i 0)
               (char= (region-id (1- i) j) region)
               (/= (get-value (1- i) j) 0)
               (<= (get-value (1- i) j) num))
      (return-from cell-valid-p nil))

    (when (and (< i (1- *n*))
               (char= (region-id (1+ i) j) region)
               (/= (get-value (1+ i) j) 0)
               (>= (get-value (1+ i) j) num))
      (return-from cell-valid-p nil))

    t))

(defun solve (&optional (row 0) (col 0))
  (cond
    ((= row *n*) t)
    ((= col *n*) (solve (1+ row) 0))
    ((/= (get-value row col) 0) (solve row (1+ col)))
    (t
     (let* ((region (region-id row col))
            (size (region-size region)))
       (loop for num from 1 to size do
            (when (cell-valid-p row col num)
              (set-value row col num)
              (when (solve row (1+ col))
                (return-from solve t))
              (set-value row col 0))))
     nil)))

(defun split-line-into-integers (line)
  (mapcar #'parse-integer (remove "" (split-sequence:split-sequence #\Space line))))

(defun split-line-into-chars (line)
  (remove "" (split-sequence:split-sequence #\Space line)))

(defun read-input ()
  (setf *values* nil
        *regions* nil
        *region-cells* (make-hash-table))
  (dotimes (i *n*)
    (let ((line (read-line)))
      (push (split-line-into-integers line) *values*)))
  (setf *values* (reverse *values*))

  (dotimes (i *n*)
    (let ((line (read-line))
          (row '()))
      (loop for j from 0
            for r in (split-line-into-chars line) do
              (push r row)
              (push (list i j) (gethash (char r 0) *region-cells*)))
      (push (reverse row) *regions*)))
  (setf *regions* (reverse *regions*)))

(defun print-solution ()
  (dolist (row *values*)
    (format t "~{~A~^ ~}~%" row)))

(defun main ()
  (format t "Digite o tamanho da grade: ")
  (setf *n* (parse-integer (read-line)))
  (read-input)
  (if (solve)
      (print-solution)
      (format t "Sem solução.~%")))
