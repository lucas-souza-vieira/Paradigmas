(defun soma(lista)
    (if (null lista)
        0
        (+ (car lista) (soma (cdr lista)))
    )
)

(defun media(lista)
    (/ (soma lista) (length lista))
)

(defun main()
  (let ((entrada (read))) ; Lê uma lista: (1 2 3 4)
    (write (media entrada)))
)


(main)