#lang racket
(require racket/match)

(provide empty-queue)
(provide queue-empty?)
(provide enqueue)
(provide dequeue)
(provide top)

(provide (struct-out queue)) ; pentru testare

;; Lucrul cu o coadă implică multe operații de tip:
;; - enqueue (adăugare element la sfârșitul cozii)
;; - dequeue (scoatere element de la începutul cozii)
;; Când coada este o listă, complexitatea operațiilor este:
;; - O(n) la enqueue (dată de complexitatea unui append)
;; - O(1) la dequeue (dată de complexitatea unui cdr)
;; Dorim cost amortizat constant (O(1))
;; atât pentru enqueue cât și pentru dequeue.
;;
;; Soluție: reprezentăm coada folosind 2 stive (liste):
;; - stiva left: din left scoatem la dequeue
;;   (O(1) dacă left are elemente, altfel O(n))
;; - stiva right: în right adăugăm la enqueue (O(1))
;; |     |    |     |
;; |     |    |__5__|
;; |__1__|    |__4__|
;; |__2__|    |__3__|
;;
;; Singura operație costisitoare este dequeue
;; când stiva left este vidă.
;; Pe exemplu: Presupunem că am scos deja 1 și 2
;; din coadă și facem un nou dequeue.
;; În acest caz, complexitatea este O(n):
;; 1. mutăm (pop + push) toate elementele din right 
;;    în left (în ordine, extragem 5, 4, 3)
;; |     |    |     |      |     |    |     |      |     |    |     |
;; |     |    |     |      |     |    |     |      |__3__|    |     |
;; |     |    |__4__|  ->  |__4__|    |     |  ->  |__4__|    |     |
;; |__5__|    |__3__|      |__5__|    |__3__|      |__5__|    |_____|
;;
;; 2. pop din stiva left, eliminând valoarea 3
;; Fiecare element al cozii se mută maxim o dată din
;; right în left => cost amortizat O(1) per operație.


; Definim structura "coadă" prin:
; - left   (o stivă: dequeue = pop pe stiva left)
; - right  (o stivă: enqueue = push în stiva right)
; - size-l (numărul de elemente din stiva left)
; - size-r (numărul de elemente din stiva right)
; Obs: Listele Racket sunt practic stive (push = cons, pop = car).
(define-struct queue (left right size-l size-r) #:transparent) 


; TODO 1 (5p)
; Definiți valoarea care reprezintă o coadă goală.
(define empty-queue
  (make-queue '() '() 0 0))


; TODO 2 (5p)
; Implementați o funcție care verifică dacă o coadă este goală.
(define (queue-empty? q)
  (if (and (= (queue-size-l q) 0) (= (queue-size-r q) 0))
      #t
      #f))


; TODO 3 (5p)
; Implementați o funcție care adaugă un element la
; sfârșitul unei cozi. Întoarceți coada actualizată.
(define (enqueue x q)
  (make-queue (queue-left q)
              (cons x (queue-right q)) ; adaug x la varful stivei din dreapta (inceputul listei drepte)
              (queue-size-l q) ; dim stivei stangi ramane aceeasi
              (+ 1 (queue-size-r q)))) ; incrementez cu 1 dim stivei drepte (pt ca tocmai am adaugat un elem
              
; queue-left = imi da stiva (lista) din stanga
; queue-right = imi da stiva (lista) din dreapta

; cand apelez enqueue cu argumentul q, ii dau tot ce are q in el pt ca e de tip queue
; accesez ce e in queue queue-left, queue-right, queue-size-l, queue-size-r 


#|

Pentru task-urile urmatoare am creat functia ajutatoare (prepare-queue) care verifica
daca coada e dezechilibrata, adica toate elementele din left s-au terminat si daca are
elemente in right le muta inversate in left si imi intoarce o coada valida care sigur
are clienti in stiva stanga

|#

(define (prepare-queue q)
  (if (zero? (queue-size-l q))
      (make-queue (reverse (queue-right q)) ; fac reverse la stiva dreapta pt ca
                  ; primul venit sa fie primul servit (principiul cozii FIFO)
                  '()
                  (queue-size-r q)
                  0)
      q)) ; daca are deja elemente in left o las asa
    

; TODO 4 (10p)
; Implementați o funcție care scoate primul element
; dintr-o coadă nevidă. Întoarceți coada actualizată.
; Obs: dequeue pe coada vidă este firesc să dea eroare.
(define (dequeue q)                        ; acum stiu sigur ca ready-q are are oameni in stiva stanga, doar operez
  (let ((ready-q (prepare-queue q)))       ; cu ea nu mai fac verificare si transfer dr->stg (separarea responsabilitatilor)
    (make-queue (cdr (queue-left ready-q)) ; actualizez stiva stanga - fara primul om
                (queue-right ready-q)      ; stiva dreapta ramane neschimba
                (- (queue-size-l ready-q) 1) ; scad cu 1 marimea stivei stangi (omul care a plecat)
                (queue-size-r ready-q))))  ; marimea stivei drepte ramane la fel
                

; TODO 5 (5p)
; Implementați o funcție care obține primul element
; dintr-o coadă nevidă. Întoarceți elementul.
; Obs: top pe coada vidă este firesc să dea eroare.
(define (top q)
  (let ((ready-q (prepare-queue q)))  ; evalueaza (prepare-queue q) si rezultatului (o coada) spune-i ready-q
        (car (queue-left ready-q))))  ; corpul lui let. ia coada ready-q, scoate lista stanga si da-mi primul element

#| let syntax

(let ([id val-expr] ...) body ...+)

> (let ([x 5]) x)
5

|#


