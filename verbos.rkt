#lang racket
;Definindo os verbos
(struct verbo
  (sinonimos
   desc
   complemento?)
  #:transparent)

(define pegar
  (verbo (list 'pegar 'coletar 'adquirir 'tomar)
                  "Item adquirido"
                  #true))
(define cima
  (verbo (list 'cima 'N)
                  "Indo para cima"
                  #false))
(define baixo
  (verbo (list 'baixo 'S)
                  "Indo para baixo"
                  #false))
(define direita
  (verbo (list 'direita 'L)
                  "Indo para direita"
                  #false))
(define esquerda
  (verbo (list 'esquerda 'O)
                  "Indo para esquerda"
                  #false))
(define entrar
  (verbo (list 'entrar)
                  "Entrando"
                  #true))
(define sair
  (verbo (list 'sair 'retirar 'leave)
                "saindo"
                #true))
(define comer
  (verbo (list 'comer 'rangar)
        "comendo"
        #f))
(define consertar
  (verbo (list 'consertar 'reparar)
        "reparando"
        #f))
(define estudar
  (verbo (list 'ler 'estudar)
        "estudando"
        #t))

;Fim dos verbos locais

;Verbos globais
(define quit
  (verbo (list 'quit 'exit)
        "quit"
        #f))
(define inventory
  (verbo (list 'inventory)
       "check inventory"
       #f))
(define help
  (verbo (list 'help)
  (symbol->string 'help)
  #f))
; Fim verbos globais

