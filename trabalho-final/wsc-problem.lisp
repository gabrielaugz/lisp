; função para mudar o estado atual
(defun fazer-estado (F L C R) 
    (list F L C R)
)

; as funções abaixo retornam o lado do fazendeiro, lobo, cabra ou repolho
(defun estadoFazendeiro (estado)
    (nth 0 estado)
)

(defun estadoLobo (estado)
    (nth 1 estado)
)

(defun estadoCabra (estado)
    (nth 2 estado)
)

(defun estadoRepolho (estado)
    (nth 3 estado)
)

; verifica o caminho que está sendo realizado, alimentando o estado atual para a lista antiga
(defun caminho (estado meta esteveNaLista)
    (cond ((null estado) nil)
         ((equal estado meta) (reverse (cons estado esteveNaLista)))
         ((not (member estado esteveNaLista :test #'equal))
              (or (caminho (fazendeiro-eleMesmo estado) meta (cons estado esteveNaLista))
                  (caminho (fazendeiroLobo estado) meta (cons estado esteveNaLista))
                  (caminho (fazendeiroCabra estado) meta (cons estado esteveNaLista))
                  (caminho (fazendeiroRepolho estado) meta (cons estado esteveNaLista))))
    )
)

; as funções abaixo definem quais movimentos são permitidos no problema, retornando o estado feito
(defun fazendeiro-eleMesmo (estado)
    (seguro (fazer-estado (oposto (estadoFazendeiro estado))
        (estadoLobo estado)
        (estadoCabra estado)
        (estadoRepolho estado))
    )
)

(defun fazendeiroLobo (estado)
    (cond ((equal (estadoFazendeiro estado) (estadoLobo estado))
                (seguro (fazer-estado (oposto (estadoFazendeiro estado))
                    (oposto (estadoLobo estado))
                    (estadoCabra estado)
                    (estadoRepolho estado))))   
        (t nil)
    )
)

(defun fazendeiroCabra (estado)
    (cond ((equal (estadoFazendeiro estado) (estadoCabra estado))
        (seguro (fazer-estado (oposto (estadoFazendeiro estado))
                    (estadoLobo estado)
                    (oposto (estadoCabra estado))
                    (estadoRepolho estado))))
        (t nil)
    )
)

(defun fazendeiroRepolho (estado)
    (cond ((equal (estadoFazendeiro estado) (estadoRepolho estado))
        (seguro (fazer-estado (oposto (estadoFazendeiro estado))
            (estadoLobo estado)
            (estadoCabra estado)
            (oposto (estadoRepolho estado)))))
        
        (t nil)
    )
)

; função que retorna o lado oposto do rio 
(defun oposto (ladoDoRio)
    (cond ((equal ladoDoRio 'E) 'D)
        ((equal ladoDoRio 'D) 'E)
    )
)

; verifica se o estado atual está seguro. caso não esteja, retorna nil
(defun seguro (estado)
    (cond ((and (equal (estadoCabra estado) (estadoLobo estado))
            (not (equal (estadoFazendeiro estado) (estadoLobo estado)))) nil)
        ((and (equal (estadoCabra estado) (estadoRepolho estado))
            (not (equal (estadoFazendeiro estado) (estadoCabra estado)))) nil)
        (t estado)
    )
)

; função apenas para facilitar o entendimento do que ocorreu nos parênteses de movimentação
(defun resposta()
    (terpri)
    (terpri)
    (princ "fazendeiro leva a cabra")
    (terpri)
    (princ "fazendeiro volta sozinho")
    (terpri)
    (princ "fazendeiro leva o lobo para o outro lado")
    (terpri)
    (princ "fazendeiro retorna com a cabra")
    (terpri)
    (princ "fazendeiro leva o repolho")
    (terpri)
    (princ "fazendeiro volta sozinho")
    (terpri)
    (princ "fazendeiro leva a cabra")
    (terpri)
    (princ "problema resolvido")
)

; função que irá resolver o problema. a ordem nos parênteses é: fazendeiro, lobo, cabra e repolho. 
; iniciamos com os 4 do lado esquerdo (inicio do problema), e em seguida é feita as movimentações respeitando os requisitos do problema. 
; no final espera-se que todos estejam com o estado na "D" (direita, outro lado do rio)
(defun resolver-problema (estado meta) (caminho estado meta nil))

(print(resolver-problema '(E E E E) '(D D D D)))
(resposta)
