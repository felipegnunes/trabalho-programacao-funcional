#lang racket
; placeholder begin
(define pegar '())
(define consertar '())
(define entrar '())
(define faculdade-medicina '())

(define (have-thing? thing) #t)
(define (take-thing! thing) '())
; placeholder end

(struct coisa (nome
               [estado #:mutable]
               acoes))

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
                        "Você pegou o livro.")))))))

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

(define comprovante-vacina
  (coisa 'comprovante
         #f
         (list
          (cons pegar
                (lambda ()
                  (if (have-thing? comprovante-vacina)
                      "Você foi vacinado e recebeu um comprovante de vacinação."
                      (begin
                        (take-thing! comprovante-vacina)
                        "Você já foi vacinado.")))))))

(define onibus
  (coisa 'onibus
         #f
         (list
          (cons consertar
                (lambda ()
                  (if (have-thing? ferramenta)
                      (if (eq? (coisa-estado onibus) #f)
                          (begin
                            (set-coisa-estado! onibus 'consertado)
                            "Usando a ferramenta e seu conhecimento de engenharia mecânica, você consertou o ônibus.\n
                            O ônibus agora está funcionando.")
                          "O ônibus já foi consertado.")
                      "Você precisa de uma ferramenta para consertar o ônibus.")))
          (cons entrar
                (lambda ()
                  (if (eq? (coisa-estado onibus) 'consertado)
                      (begin
                        (printf "Você entrou no ônibus")
                        (lambda () faculdade-medicina))
                      "O ônibus está quebrado."))))))
                      