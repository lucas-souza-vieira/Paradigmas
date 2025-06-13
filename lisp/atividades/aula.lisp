(defun hello()
    "Oi!"
)
; Notação prefixada
(defun soma()
    (+ 2 4)
)

(defun igual()
    (= 2 2)
)

(defun concatena()
    (concatenate 'string "ufsc" "floripa")
    ;(concatenate 'list '(1 2 3) (4 5 6))
    ;(concatenate 'vector '(1 2 3) "abc")
    ;(concatenate 'string "Resultado: " (igual))   
)

(defun ehpar (n)
    (= (mod n 2) 0)
)

(defun fatorial (n)
    (if (= n 0)
        1 ;caso if seja verdadeiro
        (* n (fatorial (- n 1))) ; caso do else
    )
)

(defun seleciona (n)
    (cond 
        ((= n 1) "caso 1")
        ((= n 2) "caso 2")
        ((= n 3) "caso 3")
        ((= n 4) "caso 4") ; (eq c '@) para comarar char/string
        (t "outro") ; t é uma função que retorna verdade, logo se não for nenhumas dos outros casos esse caso é verdade e retorna
    )
)

(defun construir_lista()
    '(1 2 3 4) ; inteiros
    '(#\a #\b #\c #\) ;char
    (cons 2 3) ; constroi uma dupla = (2 . 3)
    (list 1 2 3 4) ; constroi uma lista a partir d e n argumentos
)

(defun comprimento (lista)
    (if (null lista)
        0
        (+ 1 (comprimeno (cdr lista))) ; cdr retorna a cauda, car a cabeça
    )
)

(defun lista_igual (lista 1 lista2)
    (cond
        ((and (null lista1) (null lista2)) T); se as duas for nula retorna T(true)
        ((or (null lista1) (null lista2)) NIL); se uma for nula e a outra não retorna NIL(false)
        ((= (car lista1) (car lista2)) (lista_igual (cdr lista1 ) (cdr lista2)))
        (t NIL);
    
    
    )

)

(defun main()

)

; Chamada de função:
(main)