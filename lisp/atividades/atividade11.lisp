(defun potencia()
    (setq x (read))
    (setq y (read))
    (expt x y)
)

(defun absoluto()
    (setq x (read))
    (if (< x 0)
        (* x -1)
        x
    )
)

(defun areaTriangulo()
    (setq base (read))
    (setq altura (read))
    (/ (* base altura) 2)

)

(defun and_not(x y)
    (and x (not y))
)

(defun xor_meu()
    (setq x (read))
    (setq y (read))
    (or (and_not x y) (and_not y x))
)

(defun main()
    (write (xor_meu))
)


(main)