;defparameter define variavel global, ** é padrão para lembrar que é global
(defparameter *n* 0)
(defparameter *values* nil)
(defparameter *regions* nil)
(defparameter *region-cells* (make-hash-table)) ; função padrão para criar um dict

;Verifico se está dentro do tabuleiro
(defun in-bounds-p (i j)
  (and (>= i 0) (< i *n*) (>= j 0) (< j *n*)))

;Construo o tabuleiro
(defun build-region-cells ()
  (clrhash *region-cells*) ; Limpo o tabuleiro
  (dotimes (i *n*) ; For do lisp, construo o tabuleiro com a entrada que recebo
    (dotimes (j *n*)
      (let ((region (nth j (nth i *regions*)))) ; pego o elemento i j de regions, e coloco em region
        (push (list i j) (gethash region *region-cells*)))))) ; Adiciono o par ij na lista de region

; Acesso e reorno o valor na posição i j
(defun get-value (i j)
  (nth j (nth i *values*)))

; Acesso e escrevo o novo valor na posiçã i j
(defun set-value (i j val)
  (setf (nth j (nth i *values*)) val))

; Verifica se é possivel colocar o valor naquela célula de acordo com as regras
(defun is-valid (i j num)
  (let* ((region (nth j (nth i *regions*)))  ;Pego a região daquela coordenada
         (cells (gethash region *region-cells*))) ; Pego as lista de coordenadas que já estão naquela região
    ; 1. Número não pode repetir na mesma região
    (when (some (lambda (pos) ; some aplica a função para toda lista de cells
                  (= (get-value (first pos) (second pos)) num)) ;Função lambda que compara se o valor naquela posição é igual ao que eu to tentando colocar
                cells)
      (return-from is-valid nil)) ; Se já tiver, retorna false
    ; 2. Células ortogonalmente adjacentes não podem ter mesmo número
    (dolist (dir '((-1 0) (1 0) (0 -1) (0 1))) ; itero para cada um dos vizinhos
      (let ((ni (+ i (first dir))) ; Calculo a coordenada do vizinho
            (nj (+ j (second dir))))
        (when (and (in-bounds-p ni nj) ; Verifico se está dentro do tabuleiro
                   (= (get-value ni nj) num)) ; Verifico se é igual ao valor que estou tentando colocar
          (return-from is-valid nil)))) ; Se for igual, retorno falso
    ; 3. Ordem vertical na mesma região
    ;acima
    (when (and (> i 0) ; Verifico se está dentro do tabuleiro
               (char= (nth j (nth (- i 1) *regions*)) region); Verifico se a celula acima é da mesma região
               (/= (get-value (- i 1) j) 0) ; Verifico se está vazia
               (<= (get-value (- i 1) j) num)) ; Verifico se ela é maior que o numero que estou tentando colocar
      (return-from is-valid nil)) ; Se não for, retorno falso
    ;abaixo
    (when (and (< i (- *n* 1)) ; Verifico se está dentro do tabuleiro
               (char= (nth j (nth (+ i 1) *regions*)) region); Verifico se a celula abaixo é da mesma região
               (/= (get-value (+ i 1) j) 0) ; Verifico se está vazia
               (>= (get-value (+ i 1) j) num)) ; Verifico se ela é menor que o numero que estou tentando colocar
      (return-from is-valid nil)) ; Se não for, retorno falso
    t))

;Função principal
(defun solve (&optional (row 0) (col 0))
  (cond
    ((= row *n*) t) ; Se chegou no final do tabuleiro, então retorna true pois deu certo
    ((= col *n*) (solve (1+ row) 0)); Se cehgou no final da linha, pula para a proxima
    ((/= (get-value row col) 0) (solve row (1+ col))) ; Caso a celula esteja preenchida, resolve a próxima
    (t ; Tenta preencher a celula atual
     (let* ((region (nth col (nth row *regions*))) ; Obtem a região e calcula o tamanho dela
            (region-size (length (gethash region *region-cells*))))
       (loop for num from 1 to region-size do ; Itera todos os valores possiveis para essa celula
            (when (is-valid row col num) ; Se for um numero valido, adiciona ele
              (set-value row col num) 
              (when (solve row (1+ col)) ; Tenta resolver para a proxima celula
                (return-from solve t)) ; Termina a recursão
              (set-value row col 0)))) ; Se o valro for invalido, reseta para 0 
     nil)))

(defun print-solution ()
  (dolist (row *values*)
    (format t "~{~A~^ ~}~%" row))) ; Função de formatação para imprimir

(defun main ()
  (format t "Digite o valor de N: ") (setf *n* (read))
  (format t "Digite os valores (como lista Lisp): ") (setf *values* (read))
  (format t "Digite as regiões (como lista Lisp): ") (setf *regions* (read))
  (build-region-cells)
  (if (solve)
      (print-solution)
      (format t "Sem solução.~%")))

  
(main)
