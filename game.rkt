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

; macros

(define-syntax-rule (define-verbos nomes
                      [id spec ...] ...)
  (begin
    (define-verbo id spec ...) ...
    (define nomes (list id ...))))

(define-syntax define-verbo
  (syntax-rules (= _)
    [(define-verbo id (= alias ...) desc)
     (define id (verbo (list 'id 'alias ...) desc #f))]
    [(define-verbo id _ (= alias ...) desc)
     (define id (verbo (list 'id 'alias ...) desc #t))]
    [(define-verbo id)
     (define id (verbo (list 'id) (symbol->string 'id) #f))]
    [(define-verbo id _)
     (define id (verbo (list 'id) (symbol->string 'id) #t))]))

(define-syntax-rule (define-coisa nome 
                      [verb expr] ...)
    (define nome 
      (coisa 'nome #f (list (cons verb (lambda () expr)) ...))))

(define-syntax-rule (define-lugar nome 
                      desc 
                      (coisa ...) 
                      ([verbo expr] ...))
    (define nome (lugar desc
                      (list coisa ...)
                      (list (cons verbo (lambda () expr)) ...))))

(define-syntax-rule (define-global nome ([verbo expr] ...))
  (define nome (list (cons verbo (lambda () expr)) ...)))

; Verbos

(define-verbos all-verbs
  [pegar _ (= coletar adquirir tomar) "pegar"]
  [olhar (= examinar observar ver analisar) "examinar"]
  [cima (= N norte) "ir para cima"]
  [baixo (= S sul) "ir para baixo"]
  [direita (= L leste) "ir para direita"]
  [esquerda (= O oeste) "ir para esquerda"]
  [entrar _ (=) "entrar"]
  [sair (= retirar leave) "sair"]
  [comer (= rangar) "comer"]
  [consertar _ (= reparar) "reparar"]
  [estudar (= ler) "ler"]
  [quit (= exit sair) "quit"]
  [inventory (= invantario) "checar inventário"]
  [ajuda (= help) "help"])

(define-global acoes-globais
  ([quit (begin (printf "Saindo...\n") (exit))]
   [olhar (show-current-place)]
   [inventory (show-inventory)]
   [ajuda (show-help)]))

;; Coisas

(define-coisa ferramenta
  [pegar (if (have-thing? ferramenta)
                      "Você já pegou a ferramenta."
                      (begin
                        (take-thing! ferramenta)
                        "Você pegou a ferramenta. Agora, você pode consertar o Buzufba."))])

(define-coisa livro
  [pegar (if (have-thing? livro)
                      "Você já pegou o livro."
                      (begin
                        (take-thing! livro)
                        "Você pegou o livro."))]
  [estudar (if (eq? (coisa-estado livro) #f)
                      (begin
                        (set-coisa-estado! livro 'lido)
                        "Você aprendeu a consertar o ônibus, mas precisará de uma ferramenta.")
                      "Você já leu o livro e adquiriu os conhecimentos para consertar o ônibus utilizando uma ferramenta.")])

(define-coisa mascara
  [pegar (if (have-thing? mascara)
                      "Você já está de máscara."
                      (begin
                        (take-thing! mascara)
                        "Você agora está de máscara."))])

(define-coisa vacina
  [pegar (if (have-thing? vacina)
                      "Você já foi vacinado."
                      (if (eq? fome #t)
                          "Você precisa estar de barriga cheia para receber a vacina."
                          (begin
                            (take-thing! vacina)
                            "Você foi vacinado e recebeu um comprovante de vacinação.")))])

(define-coisa onibus
  [consertar (if (have-thing? ferramenta)
                      (if (eq? (coisa-estado livro) 'lido)
                          (if (eq? (coisa-estado onibus) #f)
                              (begin
                                (set-coisa-estado! onibus 'consertado)
                                (printf "Usando a ferramenta e seu conhecimento de engenharia mecânica, você consertou o ônibus.\nO ônibus agora está funcionando.\n")
                                (set! fome #t)
                                "Você ficou com fome.")
                              "O ônibus já foi consertado.")
                          "Você não tem o conhecimento necessário para consertar o ônibus.")
                      "Você precisa de uma ferramenta para consertar o ônibus.")]
  [entrar (if (eq? (coisa-estado onibus) 'consertado)
                      (if (eq? current-place estacao-buzufba)
                          (begin
                            (printf "Você entrou no ônibus.\nIndo para o Canela...\n")
                            faculdade-medicina)
                          (begin
                            (printf "Você entrou no ônibus.\nIndo para Ondina...\n")
                            estacao-buzufba))
                      "O ônibus está quebrado.")]
  [pegar (if (eq? (coisa-estado onibus) 'consertado)
                      (if (eq? current-place estacao-buzufba)
                          (begin
                            (printf "Você entrou no ônibus\nIndo para o Canela...\n")
                            faculdade-medicina)
                          (begin
                            (printf "Você entrou no ônibus.\nIndo para Ondina...\n")
                            estacao-buzufba))
                      "O ônibus está quebrado.")])


; Lugares

(define-lugar estacionamento
  "Você está no estacionamento"
  []
  ([cima morrinho]
  [direita estacao-buzufba]))

(define-lugar morrinho
  "Você está no morrinho."
  []
  ([cima biblioteca-central]
   [esquerda restaurante-universitario]
   [direita paf-1]
   [baixo estacionamento]))

(define-lugar estacao-buzufba
  "Você está na estação do Buzufba."
  [onibus]
  ([esquerda estacionamento]))

(define-lugar restaurante-universitario
  "Você está no restaurante universitário.\nÉ possível comer aqui."
  []
  ([comer (if (eq? fome #t)
                (begin
                  (set! fome #f)
                  (printf "Você comeu e está de barriga cheia.\n"))
                "Você não está com fome.")]
   [direita morrinho]))

(define-lugar paf-1
  "Você está na frente do PAF 1."
  []
  ([entrar (if (have-thing? mascara)
                (if (have-thing? vacina)
                    sala-de-aula
                    "Você precisa estar vacinado para entrar.")
                "Você não pode entrar sem máscara.")]
   [esquerda morrinho]
   [cima faculdade-farmacia]))

(define-lugar sala-de-aula
  "Você está assistindo a aula. Fim de jogo."
  []
  ([sair paf-1]))

(define-lugar faculdade-farmacia
  "Você está na Faculdade de Farmácia."
  [mascara]
  ([esquerda biblioteca-central]
  [baixo paf-1]
  [cima politecnica]))

(define-lugar faculdade-medicina
  "Você está na Faculdade de Medicina."
  [vacina onibus]
  ())

(define-lugar biblioteca-central
  "Você está na Biblioteca Central."
  [livro]
  ([direita faculdade-farmacia]
   [baixo morrinho]))

(define-lugar politecnica
  "Você está na Politécnica."
  [ferramenta]
  ([baixo faculdade-farmacia]))

; Inventário
(define inventario (list))

; Local Inicial
(define current-place estacionamento)

; Fome
(define fome #f)

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
  (printf "Escreva `olhar' para examinar o local atual.\n")
  (printf "Escreva `inventario' para ver seus itens.\n")
  (printf "Escreva `quit' para sair do jogo.\n")
  )

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




