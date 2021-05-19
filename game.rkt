#lang racket

; Structs
(struct verbo (sinonimos          ; lista de symbols
               desc               ; string
               complemento?)      ; bool
  #:transparent)

(struct lugar (desc               ; string
               [things #:mutable] ; lista de coisas
               actions))          ; lista de pares verbo-coisa

(struct coisa (nome               ; symbol
               [estado #:mutable] ; valor qualquer
               acoes))            ; lista de pares verbo-coisa


; Verbos

(define pegar
  (verbo (list 'pegar 'coletar 'adquirir 'tomar)
         "pegar"
         #true))

(define olhar
  (verbo (list 'olhar 'examinar 'observar 'ver 'analisar)
         "examinar"
         #false))

(define cima
  (verbo (list 'cima 'N 'norte)
         "ir para cima"
         #false))

(define baixo
  (verbo (list 'baixo 'S 'sul)
         "ir para baixo"
         #false))

(define direita
  (verbo (list 'direita 'L 'leste)
         "ir para direita"
         #false))

(define esquerda
  (verbo (list 'esquerda 'O 'oeste)
         "ir para esquerda"
         #false))

(define entrar
  (verbo (list 'entrar)
         "entrar"
         #true))

(define sair
  (verbo (list 'sair 'retirar 'leave)
         "sair"
         #false))

(define comer
  (verbo (list 'comer 'rangar)
         "comendo"
         #false))

(define consertar
  (verbo (list 'consertar 'reparar)
         "reparar"
         #true))

(define estudar
  (verbo (list 'ler 'estudar)
         "ler"
         #true))

(define quit
  (verbo (list 'quit 'exit)
         "quit"
         #false))

(define inventory
  (verbo (list 'inventory)
         "check inventory"
         #false))

(define help
  (verbo (list 'help)
         (symbol->string 'help)
         #false))

(define all-verbs (list pegar olhar cima baixo direita esquerda
                        entrar sair comer consertar estudar quit inventory help))

; Ações Globais
; Ações que podem ser executadas em qualquer lugar

(define acoes-globais
  (list
   (cons quit (lambda () (begin (printf "Saindo...\n") (exit))))
   (cons olhar (lambda () (show-current-place)))
   (cons inventory (lambda () (show-inventory)))
   (cons help (lambda () (show-help)))))

;; Coisas

(define ferramenta
  (coisa 'ferramenta
         #f
         (list
          (cons pegar
                (lambda ()
                  (if (have-thing? ferramenta)
                      "Você já pegou a ferramenta."
                      (begin
                        (take-thing! ferramenta)
                        "Você pegou a ferramenta. Agora, você pode consertar o Buzufba.")))))))

(define livro
  (coisa 'livro
         #f
         (list
          (cons pegar
                (lambda ()
                  (if (have-thing? livro)
                      "Você já pegou o livro."
                      (begin
                        (take-thing! livro)
                        "Você pegou o livro."))))
          (cons estudar
                (lambda ()
                  (if (eq? (coisa-estado livro) #f)
                      (begin
                        (set-coisa-estado! livro 'lido)
                        "Você aprendeu a consertar o ônibus, mas precisará de uma ferramenta.")
                      "Você já leu o livro e adquiriu os conhecimentos para consertar o ônibus utilizando uma ferramenta."))))))

(define mascara
  (coisa 'mascara
         #f
         (list
          (cons pegar
                (lambda ()
                  (if (have-thing? mascara)
                      "Você já está de máscara."
                      (begin
                        (take-thing! mascara)
                        "Você agora está de máscara.")))))))

(define vacina
  (coisa 'vacina
         #f
         (list
          (cons pegar
                (lambda ()
                  (if (have-thing? vacina)
                      "Você já foi vacinado."
                      (begin
                        (take-thing! vacina)
                        "Você foi vacinado e recebeu um comprovante de vacinação.")))))))

(define onibus
  (coisa 'onibus
         #f
         (list
          (cons consertar
                (lambda ()
                  (if (have-thing? ferramenta)
                      (if (eq? (coisa-estado livro) 'lido)
                          (if (eq? (coisa-estado onibus) #f)
                              (begin
                                (set-coisa-estado! onibus 'consertado)
                                "Usando a ferramenta e seu conhecimento de engenharia mecânica, você consertou o ônibus.\n
                            O ônibus agora está funcionando.")
                              "O ônibus já foi consertado.")
                          "Você não tem o conhecimento necessário para consertar o ônibus.")
                      "Você precisa de uma ferramenta para consertar o ônibus.")))
          (cons entrar
                (lambda ()
                  (if (eq? (coisa-estado onibus) 'consertado)
                      (if (eq? current-place estacao-buzufba)
                          (begin
                            (printf "Você entrou no ônibus.\nIndo para o Canela...\n")
                            faculdade-medicina)
                          (begin
                            (printf "Você entrou no ônibus.\nindo para Ondina...\n")
                            estacao-buzufba))
                      "O ônibus está quebrado.")))
          (cons pegar
                (lambda ()
                  (if (eq? (coisa-estado onibus) 'consertado)
                      (if (eq? current-place estacao-buzufba)
                          (begin
                            (printf "Você entrou no ônibus, indo para o Canela.\n")
                            faculdade-medicina)
                          (begin
                            (printf "Você entrou no ônibus, indo para Ondina.\n")
                            estacao-buzufba))
                      "O ônibus está quebrado."))))))


; Lugares

(define estacionamento
  (lugar
   "Você está no estacionamento."
   (list)
   (list
    (cons cima
          (lambda () morrinho))
    (cons direita
          (lambda () estacao-buzufba)))))

(define morrinho
  (lugar
   "Você está no morrinho."
   (list)
   (list
    (cons cima 
          (lambda () biblioteca-central))
    (cons esquerda
          (lambda () restaurante-universitario))
    (cons direita
          (lambda () paf-1))
    (cons baixo
          (lambda () estacionamento)))))

(define estacao-buzufba
  (lugar
   "Você está na estação do Buzufba."
   (list onibus)
   (list
    (cons esquerda
          (lambda () estacionamento)))))

(define restaurante-universitario
  (lugar
   "Você está no restaurante universitário."
   (list)
   (list
    (cons direita
          (lambda () morrinho)))))

(define paf-1
  (lugar
   "Você está na frente do PAF 1."
   (list)
   (list
    (cons entrar
          (lambda ()
            (if (have-thing? mascara)
                (if (have-thing? vacina)
                    sala-de-aula
                    "Você precisa estar vacinado para entrar.")
                "Você não pode entrar sem máscara.")))
    (cons esquerda
          (lambda () morrinho))
    (cons cima 
          (lambda () faculdade-farmacia)))))

(define sala-de-aula
  (lugar
   "Você está assistindo a aula. Fim de jogo."
   (list)
   (list
    (cons sair
          (lambda () paf-1)))))

(define faculdade-farmacia
  (lugar
   "Você está na Faculdade de Farmácia."
   (list mascara)
   (list
    (cons esquerda
          (lambda () biblioteca-central))
    (cons baixo
          (lambda () paf-1))
    (cons cima 
          (lambda () politecnica)))))

(define faculdade-medicina
  (lugar
   "Você está na Faculdade de Medicina."
   (list vacina onibus)
   (list)))

(define biblioteca-central
  (lugar
   "Você está na Biblioteca Central."
   (list livro)
   (list
    (cons direita 
          (lambda () faculdade-farmacia))
    (cons baixo
          (lambda () morrinho)))))

(define politecnica
  (lugar
   "Você está na Politécnica."
   (list ferramenta)
   (list
    (cons baixo
          (lambda () faculdade-farmacia)))))

; Estado do Jogo

; Inventário
(define inventario (list))

; Local Inicial
(define current-place estacionamento)

; Funções Gerais
(define (have-thing? thing) ; checa se item está no inventário
  (memq thing inventario))

(define (take-thing! thing) ; pega item do lugar e coloca no inventário
  (set-lugar-things! current-place
                     (remq thing (lugar-things current-place)))
  (set! inventario (cons thing inventario)))

(define (show-current-place)
  (printf "~a\n" (lugar-desc current-place)) ; imprime o lugar
  (for-each (lambda (thing)      ; imprime as coisas do lugar
              (printf "Tem um ~a aqui.\n" (coisa-nome thing)))
            (lugar-things current-place)))

(define (show-inventory)
  (printf "Seu inventário: ")
  (if (null? inventario)
      (printf "Você não tem itens.")
      (for-each (lambda (coisa)
                  (printf "\n  ~a" (coisa-nome coisa)))
                inventario))
  (printf "\n"))

(define (show-help)
  (printf "Escreva `inventory' para ver seus itens.\n"))

(define (do-place)
  (show-current-place) ; mostra lugar atual
  (do-verb))           ; executa comando

;; Main loop
;; Get and handle a command
(define (do-verb)
  (printf "> ")             ; imprime o prompt
  (flush-output)
  (let* ([line (read-line)] ; lê comando
         [input (if (eof-object? line)  ; vê se foi um comando de fim de arquivo
                    '(quit)             ; se sim, sai
                    (let ([port (open-input-string line)]) ; se não, coloca palavras
                      (for/list ([v (in-port read port)]) v)))])  ; em "input"
    (if (and (list? input)            ; se input é lista,
             (andmap symbol? input)   ; tem só símbolos,
             (<= 1 (length input) 2)) ; checa se 1 <= (número de símbolos) <= 2
        (let ([cmd (car input)]) ;; o comando principal, verbo, é o começo da lista
          (let ([response ;; monta resposta para verbos
                 (cond
                   [(= 2 (length input))
                    (handle-transitive-verb cmd (cadr input))] ;; transitivos
                   [(= 1 (length input))
                    (handle-intransitive-verb cmd)])])         ;; intransitivos
            (let ([result (response)]) ;; resposta é uma função, execute-a
              (cond
                [(lugar? result) ;; se o resultado for um lugar
                 (set! current-place result) ;; ele passa a ser o novo lugar
                 (do-place)]   ;; faça o processamento do novo lugar, loop
                [(string? result) ; se a resposta for uma string
                 (printf "~a\n" result)  ; imprima a resposta
                 (do-verb)]    ; volte a processar outro comando, loop
                [else (do-verb)])))) ; caso contrário, outro comando, loop
        (begin ; Comando incorreto
          (printf "Não entendi.\n")
          (do-verb)))))

;; Handle an intransitive-verb command:
;; retorna função para processar verbo intrasitivo

(define (handle-intransitive-verb cmd)
  (or
   ; considerando o lugar, retorna a ação associada ao verbo
   (find-verb cmd (lugar-actions current-place))
   ; se não achou no lugar, considerando o jogo todo, retorna a ação associada ao verbo
   (find-verb cmd acoes-globais)
   ; se não achou no lugar ou no geral, mas o verbo existe
   ; retorna uma função que dá uma mensagem de erro em contexto
   (using-verb  ; procura o verbo, obtem info descritiva, e retorna a função abaixo
    cmd all-verbs
    (lambda (verb)
      (lambda () ; função retornada por using-verb, mensagem de erro em contexto
        (if (verbo-complemento? verb)
            (format "~a o quê?" (string-titlecase (verbo-desc verb)))
            (format "Você não consegue ~a." (verbo-desc verb))))))
   (lambda () ; não achou o verbo no jogo
     (format "Você não sabe ~a." cmd))))

;; Handle a transitive-verb command:
(define (handle-transitive-verb cmd obj)
  (or (using-verb ; produz ação para verbo, retorna falso se não achar verbo no jogo
       cmd all-verbs
       (lambda (verb) ; função retornada
         (and ; retorna falso se alguma destas coisas for falsa
          (verbo-complemento? verb) ; verbo é transitivo? - funcão criada por struct 
          (cond
            [(ormap (lambda (thing) ; verifica se o objeto nomeado existe em contexto
                      (and (eq? (coisa-nome thing) obj)
                           thing))
                    ; na lista das coisas do lugar e das coisas que tenho (stuff)
                    (append (lugar-things current-place) 
                            inventario))
             => (lambda (thing) ; se existe, aplica esta função sobre a coisa/thing
                  (or (find-verb cmd (coisa-acoes thing)) ; retorna acão que se aplica a coisa
                      (lambda () ; se ação não encontrada, indica que não há ação
                        (format "Você não sabe ~a ~a."
                                (verbo-desc verb) obj))))]
            [else ; se objeto não existe
             (lambda ()
               (format "Aqui não tem ~a para ~a." obj 
                       (verbo-desc verb)))]))))
      (lambda ()  ; se não achou o verbo
        (format "Impossível ~a ~a." cmd obj))))

;; Look for a command match in a list of verb--response pairs,
;; and returns the response thunk if a match is found:
(define (find-verb cmd actions)
  (ormap (lambda (a)
           (and (memq cmd (verbo-sinonimos (car a)))
                (cdr a)))
         actions))

;; Looks for a command in a list of verbs, and
;; applies `success-k' to the verb if one is found:
(define (using-verb cmd verbs success-k)
  (ormap (lambda (vrb)
           (and (memq cmd (verbo-sinonimos vrb))
                (success-k vrb)))
         verbs))

(do-place)




