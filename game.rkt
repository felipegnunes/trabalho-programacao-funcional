#lang racket

; Tempo no começo do jogo
(define game-start-timestamp (current-seconds))

; Pontuação no começo do jogo
(define pontuacao 10000)

; Número de moedas coletadas até agora
(define moedas 0)

(define (fim-de-jogo pontuacao tempo-atual)
  (let ([end-string
         (let ([total-elapsed-time (- tempo-atual game-start-timestamp)])
           (format "Você entrou na sala de aula e fez a prova.\nFIM DE JOGO\nVocê terminou o jogo em ~amin~as.\nSua pontuação final foi ~a.\n"
                   (truncate (/ total-elapsed-time 60))
                   (modulo total-elapsed-time 60)
                   pontuacao))])
    (string-append end-string (format "Você coletou ~a/3 moedas.\nDigite qualquer coisa para sair do jogo.\n"
                                      moedas))))

; Structs
(struct verbo (sinonimos          ; lista de symbols
               desc               ; string
               complemento?)      ; bool
  #:transparent)

(struct lugar (desc               ; string
               [things #:mutable] ; lista de coisas
               actions))          ; lista de pares verbo-coisa

(struct coisa (nome               ; symbol
               genero             ; symbol
               separate-display   ; bool
               [estado #:mutable] ; valor qualquer
               acoes))            ; lista de pares verbo-coisa

; Macros

(define-syntax-rule (state name initial-value)
  (define name initial-value))

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
                      genero
                      separate-display
                      [verb expr] ...)
  (define nome 
    (coisa 'nome genero separate-display #f (list (cons verb (lambda () expr)) ...))))

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
  [olhar (= examinar observar ver analisar) "examinar arredores"]
  [cima (= N norte) "ir para cima"]
  [baixo (= S sul) "ir para baixo"]
  [direita (= L leste) "ir para direita"]
  [esquerda (= O oeste) "ir para esquerda"]
  [entrar _ (=) "entrar"]
  [sair (= retirar leave) "sair"]
  [comer (= rangar) "comer"]
  [consertar _ (= reparar) "consertar"]
  [estudar _ (= ler) "estudar"]
  [quit (= exit encerrar) "sair do jogo"]
  [inventory (= inventario) "checar inventário"]
  [ajuda (= help) "ajuda"]
  [fazer _ (=) "fazer"]
  [vacinar (=) "vacinar"]
  [devolver _ (=) "devolver"]
  [balancar _ (= mexer) "balançar"]
  [sentar _ (=) "sentar"]
  [combater _ (= lutar) "combater"])

(define-global acoes-globais
  ([quit (begin (printf "Saindo...\n") (exit))]
   [olhar (show-current-place)]
   [inventory (show-inventory)]
   [ajuda (show-help)]))

;; Coisas

(define-coisa individuo 'm #t
  [combater (begin
           (set! individuo-suspeito-derrotado #t)
           (set! pontuacao (+ pontuacao 10000))
           "Você derrotou o indivíduo suspeito!")])           
  

(define-coisa ferramenta 'f #f
  [pegar (if (have-thing? ferramenta)
             "Você já tem uma ferramenta."
             (begin
               (take-thing! ferramenta)
               "Você pegou a ferramenta. Isso pode ser útil para consertar um ônibus."))])

(define-coisa livro 'm #t
  [pegar (if (or (eq? (coisa-estado livro) 'lido) (have-thing? livro))
             "Você já pegou um livro."
             (begin
               (take-thing! livro)
               "Você pegou um livro aleatoriamente. Por acaso, é um livro de engenharia mecânica."))]
  [estudar (if (eq? (coisa-estado livro) #f)
               (begin
                 (set-coisa-estado! livro 'lido)
                 "Você leu o livro e aprendeu a consertar um ônibus, desde que você tenha uma ferramenta.")
               "Você já leu o suficiente por hoje.")]
  [devolver (if (eq? (coisa-estado livro) 'lido)
                (cond
                  [(eq? current-place biblioteca-central)
                   (take-thing! moeda-OG)
                   (set! moedas (+ moedas 1))
                   (set! inventario (remq livro inventario))
                   (set! pontuacao (+ pontuacao 10000))
                   (set-lugar-things! biblioteca-central (cons livro (lugar-things biblioteca-central)))
                   "Você devolveu o livro para a biblioteca. Ao se preparar para sair, você percebe algo brilhando no chão. É uma moeda colecionável, com as letras \"OG\" na frente!\nVocê pega a moeda."]
                  [else
                   "Você precisa estar na biblioteca para devolver o livro."])
                "Seria melhor ler o livro antes de o devolver.")])                               

(define-coisa mascara 'f #t
  [pegar (if (have-thing? mascara)
             "Você já está de máscara."
             (begin
               (take-thing! mascara)
               "Você agora está de máscara."))])

(define-coisa moeda-PR 'f #t
  [pegar (begin
           (take-thing! moeda-PR)
           "Você pegou a moeda.")])

(define-coisa moeda-OL 'f #t
  [pegar (begin
           (take-thing! moeda-OL)
           "Você pegou a moeda.")])

(define-coisa moeda-OG 'f #t
  [pegar (begin
           (take-thing! moeda-OG)
           "Você pegou a moeda.")])

(define-coisa banco 'm #f
  [sentar (cond
            [(have-thing? moeda-OL)
             "Você sentou no banco. Legal."]
            [else
             (set! pontuacao (+ pontuacao 10000))
             (take-thing! moeda-OL)
             (set! moedas (+ moedas 1))
             "Você sentou no banco por alguns segundos. Ao levantar, você percebe algo brilhante no chão. É uma moeda colecionável, com as letras \"OL\" na frente!\nVocê pega a moeda."])])              

(define-coisa vacina 'f #t
  [pegar (if (have-thing? vacina)
             "Você já foi vacinado."
             (if (eq? fome #t)
                 "Você precisa estar de barriga cheia para receber a vacina."
                 (begin
                   (take-thing! vacina)
                   "Você foi vacinado e recebeu um comprovante de vacinação.")))])

(define-coisa onibus 'none #t
  [consertar (if (have-thing? ferramenta)
                 (if (eq? (coisa-estado livro) 'lido)
                     (if (eq? (coisa-estado onibus) #f)
                         (begin
                           (set-coisa-estado! onibus 'consertado)
                           (printf "Usando a ferramenta e seu conhecimento de engenharia mecânica, você consertou o ônibus.\n")
                           (set! fome #t)
                           "Você ficou com fome.")
                         "O ônibus já foi consertado.")
                     "Você não tem o conhecimento necessário para consertar o ônibus.")
                 "Você precisa de uma ferramenta para consertar o ônibus.")]
  [entrar (if (eq? (coisa-estado onibus) 'consertado)
              (if (eq? current-place estacao-buzufba)
                  (begin
                    (printf "Você entrou no ônibus. Indo para o Canela...\n")
                    faculdade-medicina)
                  (begin
                    (printf "Você entrou no ônibus. Indo para Ondina...\n")
                    estacao-buzufba))
              "O ônibus está quebrado.")]
  [pegar (if (eq? (coisa-estado onibus) 'consertado)
             (if (eq? current-place estacao-buzufba)
                 (begin
                   (printf "Você entrou no ônibus. Indo para o Canela...\n")
                   faculdade-medicina)
                 (begin
                   (printf "Você entrou no ônibus. Indo para a Ondina...\n")
                   estacao-buzufba))
             "O ônibus está quebrado.")])

(define-coisa prova 'f #t
  [fazer sala-de-aula])

(define-coisa arvore 'f #f
  [balancar (cond
              [(have-thing? moeda-PR)
               "Você balança a árvore. Nada acontece."]
              [else
               (set! pontuacao (+ pontuacao 10000))
               (take-thing! moeda-PR)
               (set! moedas (+ moedas 1))
               "Você balança a árvore e algo brilhante cai dela. É uma moeda colecionável, com as letras \"PR\" na frente!\nVocê pega a moeda."])]
  [pegar "Você tenta pegar a árvore, mas ela não cabe na sua mochila."])               

; Lugares

(define-lugar estacionamento
  "Você está no estacionamento."
  []
  ([cima morrinho]
   [direita estacao-buzufba]))

(define-lugar escola-de-danca
  "Você está na Escola de Dança."
  [banco]
  ([direita restaurante-universitario]))

(define-lugar morrinho
  "Você está no morrinho."
  [arvore]
  ([cima biblioteca-central]
   [esquerda restaurante-universitario]
   [direita paf-1]
   [baixo estacionamento]))

(define-lugar estacao-buzufba
  "Você está na estação do Buzufba.\nTem um ônibus aqui, com destino ao canela."
  [onibus]
  ([esquerda estacionamento]
   [cima "Devido a uma obra, você não pode ir para o PAF 1 por esse caminho."]))

(define-lugar restaurante-universitario
  "Você está no restaurante universitário.\nÉ possível comer aqui."
  []
  ([comer (if (eq? fome #t)
              (begin
                (set! fome #f)
                (printf "Você comeu e está de barriga cheia.\n"))
              "Você não está com fome.")]
   [direita morrinho]
   [esquerda escola-de-danca]))

(define-lugar paf-1
  "Você está na frente do PAF 1."
  []
  ([entrar (if (have-thing? mascara)
               (if (have-thing? vacina)
                   paf-1-interior
                   (if (eq? primeira-vez #t)
                       (begin
                         (set! primeira-vez #f)
                         "Você tenta entrar no PAF 1, mas é barrado pelo segurança.\n\"Você precisa de uma máscara e de um comprovante de vacinação para entrar\", ele diz.")
                       "Você precisa estar vacinado para entrar."))
               (if (eq? primeira-vez #t)
                   (begin
                     (set! primeira-vez #f)
                     "Você tenta entrar no PAF 1, mas é barrado pelo segurança.\n\"Você precisa de uma máscara e de um comprovante de vacinação para entrar\", ele diz.")
                   "Você não pode entrar sem máscara."))]
   [esquerda morrinho]
   [cima faculdade-farmacia]
   [baixo "Devido a uma obra, você não pode ir para a estação do Buzufba por esse caminho."]))

(define-lugar paf-1-interior
  "Você está dentro do PAF 1. Finalmente, você pode fazer a prova."
  [prova]
  ([sair paf-1]))

(define-lugar sala-de-aula
  (fim-de-jogo pontuacao (current-seconds))
  []
  ([sair paf-1-interior]))

(define-lugar faculdade-farmacia
  "Você está na Faculdade de Farmácia.\nTem uma cesta de máscaras na entrada, com um aviso dizendo \"máscaras grátis!\""
  [mascara]
  ([esquerda biblioteca-central]
   [baixo paf-1]
   [cima escadao-politecnica]))

(define-lugar escadao-politecnica
  "Você está no escadão da Politécnica.\nVocê avista um indivíduo suspeito no meio do escadão."
  [individuo]
  ([cima (if individuo-suspeito-derrotado
             politecnica
             "Você precisar combater o indivíduo suspeito para subir o escadão.")]
   [baixo faculdade-farmacia]))

(define-lugar faculdade-medicina
  "Você está na Faculdade de Medicina.\nTem um ônibus aqui, com destino à ondina.\nVocê pode se vacinar aqui!"
  [vacina onibus]
  ([vacinar (if (have-thing? vacina)
                "Você já foi vacinado."
                (if (eq? fome #t)
                    "Você precisa estar de barriga cheia para receber a vacina."
                    (begin
                      (take-thing! vacina)
                      "Você foi vacinado e recebeu um comprovante de vacinação.")))]))

(define-lugar biblioteca-central
  "Você está na Biblioteca Central.\nVocê pode pegar um livro aqui."
  [livro]
  ([direita faculdade-farmacia]
   [baixo morrinho]))

(define-lugar politecnica
  "Você está na Politécnica."
  [ferramenta]
  ([baixo faculdade-farmacia]))

; Estado do jogo
(state inventario (list)) ; Inventário
(state current-place estacionamento) ; Local Inicial
(state fome #f) ; Fome
(state primeira-vez #t) ; Primeira vez tentando entrar no paf 1
(state individuo-suspeito-derrotado #f)


; Funções Gerais
(define (have-thing? thing) ; checa se item está no inventário
  (memq thing inventario))

(define (take-thing! thing) ; pega item do lugar e coloca no inventário
  (set-lugar-things! current-place
                     (remq thing (lugar-things current-place)))
  (set! inventario (cons thing inventario)))

(define (show-current-place)
  (printf "~a\n" (lugar-desc current-place)) ; imprime o lugar
  (for-each (lambda (coisa)      ; imprime as coisas do lugar
              (if (coisa-separate-display coisa)
                  '()
                  (if (eq? (coisa-genero coisa) 'm)
                      (printf "Tem um ~a aqui.\n" (coisa-nome coisa))
                      (printf "Tem uma ~a aqui.\n" (coisa-nome coisa)))))
            (lugar-things current-place)))

(define (show-inventory)
  (printf "Seu inventário: ")
  (if (null? inventario)
      (printf "Você não tem itens.")
      (for-each (lambda (coisa)
                  (cond
                    [(eq? (coisa-genero coisa) 'm)
                     (printf "\n  um ~a" (coisa-nome coisa))]
                    [(eq? (coisa-genero coisa) 'f)
                     (printf "\n  uma ~a" (coisa-nome coisa))]
                    [else (printf "\n  ~a" (coisa-nome coisa))]))
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
                 (set! pontuacao (- pontuacao 100))
                 (set! current-place result) ;; ele passa a ser o novo lugar
                 (if (eq? result sala-de-aula)
                     (begin
                       (printf (fim-de-jogo pontuacao (current-seconds)))
                       (final))
                     (do-place))]   ;; faça o processamento do novo lugar, loop
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

(define (final)
  (printf "> ")
  (flush-output)
  (let ([line (read-line)])
    (exit)))
  
(printf "Você está no carro de um amigo indo para a UFBA, onde você tem uma prova de Prolog para fazer no PAF 1. Você chega na portaria 1 e sai do carro enquanto seu amigo procura uma vaga.\n\n")

(do-place)

