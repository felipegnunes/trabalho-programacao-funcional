#lang racket
;Definindo os verbos
(struct verbo
  (sinonimos
   desc
   complemento?)
  #:transparent)

(define pegar
  (verbo (list 'pegar 'coletar 'adquirir)
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

;Fim dos verbos

;Definindo lugares
(struct lugar
      (descr
       itens
       acoes))

(define estacao-o
  (lugar   "Voce esta na estacao do buzufba no paf. Vai para o Canela ?"
   (list)
   (list 'entrar)
   ))

(define estacao-c
  (lugar   "Voce esta na estacao do buzufba no Canela. Vai para a Ondina ?"
   (list)
   (list 'entrar)
   ))

(define entrada
  (lugar   "Voce esta na entrada da UFBA. A direita ha o buzufba, a esquerda o PAF1"
   (list)
   (list 'direita 'esquerda)
   ))