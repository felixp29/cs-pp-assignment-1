#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un magazin.
;; C1 acceptă doar clienți care au cumpărat maxim ITEMS produse
;; (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întârzieri (delay).
;; La un moment dat, la fiecare casă există
;; 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă
;; timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei +
;; întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO 1 (10p)
; Implementați o funcție care întoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat
; o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO 2 (10p)
; Implementați o funcție care crește tt-ul unei case
; cu un număr dat de minute.
(define (tt+ C minutes)
  (struct-copy counter C [tt (+ (counter-tt C) minutes)]))


; TODO 3 (20p)
; Implementați o funcție care primește o listă nevidă 
; de case și întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt,
; este preferată casa cu indexul cel mai mic
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe coadă.
(define (min-tt counters)
  (min-tt-helper (cdr counters) (car counters)))

; definesc functia min-tt-helper cu 2 argumente: restul listei si prima casa ca best-so-far
(define (min-tt-helper remaining-counters best-so-far)
    ; fastest-so-far e acumulatorul - este o strucutra counter cu campurile definite mai sus
    (if (null? remaining-counters)                 ; cazul de baza
        ; then
        (cons (counter-index best-so-far) (counter-tt best-so-far))
        ;else
        ; verific daca prima casa din lista remaining-counters e mai buna decat ce am in acumulator
        (if (better-counter? (car remaining-counters) best-so-far)
            ; then
            (min-tt-helper (cdr remaining-counters) (car remaining-counters))
            ; else pastrez vechiul acc
            (min-tt-helper (cdr remaining-counters) best-so-far))))

; wishful thinking - mi-am imaginat functia better-counter care nu exista, dar va fi implementata acum
(define (better-counter? C1 C2)
  (or (< (counter-tt C1) (counter-tt C2))
      (and (= (counter-tt C1) (counter-tt C2))
           (< (counter-index C1) (counter-index C2)))))


; TODO 4 (20p)
; Implementați aceeași funcționalitate de mai sus,
; cu recursivitate pe stivă.
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe stivă.
(define (min-tt-stack counters)
  (if (null? (cdr counters))
           ; then - cazul de baza - o singura casa - o transform in pereche (index . tt)
           (cons (counter-index (car counters)) (counter-tt (car counters)))
           ; else
           (if (or (< (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
                   (and (= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
                        (< (counter-index (car counters)) (car (min-tt-stack (cdr counters))))))
               ;then
               (cons (counter-index (car counters)) (counter-tt (car counters)))
               ;else
               (min-tt-stack (cdr counters)))))


; TODO 5 (10p)
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei,
; n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii
; (name . n-items) la sfârșitul cozii de așteptare.

; desfac casa cu match, iau coada de clienti si adaug noua persoana la final + updatez timpul

(define (add-to-counter C name n-items)
  (match C [(counter i tt q) (make-counter i
                                           (+ tt n-items)
                                           (append q (list (cons name n-items))))]))


; TODO 6 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - așază persoana <name> la coadă la o casă
; - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (conform logicii implementate de min-tt)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  (define (choose-counter-and-place-client remaining-requests name n-items C1 C2 C3 C4)
    (place-client-at-counter remaining-requests name n-items C1 C2 C3 C4 (car (min-tt (if (<= n-items ITEMS)
                                                                                          ; then
                                                                                          (list C1 C2 C3 C4)
                                                                                          ; else
                                                                                          (list C2 C3 C4))))))

  (define (place-client-at-counter remaining-req name n-items C1 C2 C3 C4 best-index)
    (cond
      [(= best-index 1) (serve remaining-req (add-to-counter C1 name n-items) C2 C3 C4)]
      [(= best-index 2) (serve remaining-req C1 (add-to-counter C2 name n-items) C3 C4)]
      [(= best-index 3) (serve remaining-req C1 C2 (add-to-counter C3 name n-items) C4)]
      [(= best-index 4) (serve remaining-req C1 C2 C3 (add-to-counter C4 name n-items))]))


  ; Puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele
  ;   requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; Puteți să vă definiți funcții ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți testa fiecare funcție imediat ce ați implementat-o
  ; Nu este obligatoriu să definiți funcții ajutătoare.

  (if (null? requests)
      ; then:
      (list C1 C2 C3 C4)
      ; else:
      (match (car requests)
        ; tipul 1 - (delay <index> <minutes>)
        [(list 'delay index minutes) (cond
                                       [(= index 1) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4)]
                                       [(= index 2) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4)]
                                       [(= index 3) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4)]
                                       [(= index 4) (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))])]

        ; tipul 2 - (<name> <n-items>)
        [(list name n-items) (choose-counter-and-place-client (cdr requests) name n-items C1 C2 C3 C4)])))


; (match obiect-de-potrivit
;      [tipar-1    corp-1]
;      [tipar-2    corp-2])
