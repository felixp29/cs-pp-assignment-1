#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Este necesar să implementați întâi
;;          TDA-ul queue în fișierul queue.rkt.
;; Reveniți la acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Se modifică însă implementarea câmpului queue:
; - în loc de listă, acesta va fi o structură de tip queue
; - modificarea nu este vizibilă în definiția structurii,
;   ci în implementarea operațiilor tipului counter
(define-struct counter (index tt et queue) #:transparent)


; TODO 6 (20p)
; Actualizați funcțiile de mai jos conform cu 
; noua reprezentare a cozii de persoane.
; Elementele cozii rămân perechi (nume . nr_produse).
; RESTRICȚII (5p per abatere)
;  - Respectați "bariera de abstractizare", adică 
;    operați cu coada folosind exclusiv interfața:
;    - empty-queue
;    - queue-empty?
;    - enqueue
;    - dequeue
;    - top
; Obs: Doar câteva funcții necesită actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)   ; nu se modifica din etapa 2
  (map (λ (C) (if (= (counter-index C) index)
                  ; then
                  (f C)     ; aplic transformarea f pe casa C daca am gasit indexul
                  ; else
                  C))        ; nu am indexul -> las casa nemodificata
       counters))
; counters este argumentul functiei map
; C (cate un element din counters, pe rand) = argumentul functiei anonime λ
; map imi despacheteaza lista counters si verifica pt fiecare element al listei
; map imi face impachetarea inapoi in lista

(define ((tt+ minutes) C)  ; nemodificata
  (struct-copy counter C [tt (+ (counter-tt C) minutes)]))

; am inversat ordinea parametrilor fata de etapa 1 (acum minutes e primul)
; pentru ca prelucrez parametrul minutes in aceasta functie si rezultatul
; (tot o functie care asteapta alt argument) il pasez mai departe
; deci functia tt+ imi permite aplicare partiala
; am facut asta definind functia tt+ in stil curry ((tt+ minutes) C)

(define ((et+ minutes) C)  ; nemodificata 
  (struct-copy counter C [et (+ (counter-et C) minutes)]))

; struct-copy pentru ca trebuie sa creez o noua casa ca sa ii actualizez et-ul,
; nu ii modific et-ul casei existente (imutabilitate)

(define ((add-to-counter name items) C) ; testată de checker
                                        ; nu modificați signatura!
  ; in loc de null? si append folosesc queue-empty? si enqueue
  (match C
    [(counter index tt et queue)
     (if (queue-empty? queue)
         ; casa e goala, creste tt, et devine nr de produse, adaug in queue
         (make-counter index (+ tt items) (+ et items) (enqueue (cons name items) queue))
         ; casa are oameni: creste doar tt, et ramane la fel, adaug la finalul cozii
         (make-counter index (+ tt items) et (enqueue (cons name items) queue)))]))
         
    

(define (min-by-field field)
  (λ (counters)
    ; foldl e ca un accumulator: pleaca de la prima casa si o pastreaza pe cea mai mica
    (foldl (λ (C best)
             (if (< (field C) (field best)) C best))
           (car counters)
           (cdr counters))))

(define (min-tt counters)
  (let ((best ((min-by-field counter-tt) counters)))
    (cons (counter-index best) (counter-tt best))))

(define (min-et counters)
  (let ((best ((min-by-field counter-et) counters)))
    (cons (counter-index best) (counter-et best))))

(define (remove-first-from-counter C)   ; testată de checker
  (match C
    [(counter index tt et queue)
     (let ((new-queue (dequeue queue))) ; Folosim TDA-ul pentru a scoate omul
       (if (queue-empty? new-queue)
           ; cazul 1: Casa a ramas goala
           (make-counter index 0 0 empty-queue)
           ; cazul 2: Mai sunt oameni, noul et este timpul noului prim client
           (let ((next-client (top new-queue))) ; top imi da perechea (nume . produse)
             (make-counter index 
                           (- tt et)           ; TT scade cu timpul celui care a plecat
                           (cdr next-client)   ; noul ET este nr de produse al noului client
                           new-queue))))]))


; TODO 7 (10p)
; Implementați o funcție care calculează starea
; unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp
; nu iese nimeni din coadă, deci se modifică
; doar câmpurile tt și et.
; Este responsabilitatea utilizatorului să nu apeleze
; funcția cu minutes > et și coadă nevidă.
; La casele fără clienți, este responsabilitatea
; voastră să nu produceți timpi negativi.
(define ((pass-time-through-counter minutes) C)
  (match C
    [(counter index tt et queue)
     (make-counter index 
                   (max 0 (- tt minutes)) ; tt nu scade sub 0
                   (max 0 (- et minutes)) ; et nu scade sub 0
                   queue)]))              ; coada ramane intacta
  

; TODO 8 (60p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri:
;   3 moștenite din etapa 2:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   plus noutatea:
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
; Obs: Cererile (remove-first) din etapa 2 sunt înlocuite de un mecanism  
; mai sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul procesează cererile în ordine, astfel:
; - nicio modificare pentru cererile moștenite din etapa 2
; - când timpul prin sistem avansează cu <x> minute, starea caselor
;   se actualizează pentru a reflecta trecerea timpului;
;   ieșirile clienților din coadă se rețin în ordine cronologică.
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Sugestii:
; - gestionați cronologia folosind în mod repetat funcția min-et 
; - pentru a menține lista clienților plecați, definiți o funcție ajutătoare
; (cu un parametru în plus față de serve), pe care serve doar o apelează.
; RESTRICȚII (5p per abatere)
;  - Folosiți minim un let și un let* (care nu ar putea fi let). (2*5p)
;  - Respectați "bariera de abstractizare" oricând operați cu tipul queue.
(define (serve requests fast-counters slow-counters)
  (define (f-helper requests fast slow exits)
    (if (null? requests)
        (cons (reverse exits) (append fast slow)) ; daca a terminat requests, imi da (plecati . case)
        (match (car requests) 
          
          ; caz 2: intarziere (delay index minutes)
          [(list 'delay index minutes)
           (f-helper (cdr requests)
                   (update (et+ minutes) (update (tt+ minutes) fast index) index)
                   (update (et+ minutes) (update (tt+ minutes) slow index) index)
                   exits)]

          ; caz 3: adaugare case (ensure average)
          [(list 'ensure average)
           (let* ((all-counters (append fast slow))
                  (media-calculata (/ (apply + (map counter-tt all-counters)) (length all-counters))))
             (if (> media-calculata average)
                 ; daca media_calculata e prea mare, adaug case slow
                 (f-helper requests 
                         fast 
                         (append slow (list (empty-counter (+ 1 (length all-counters))))) 
                         exits)
                 ; daca media e ok, trec la urmatoarea cerere
                 (f-helper (cdr requests) fast slow exits)))]
          
          ; caz 1: client nou (nume n-items)
          [(list name n-items)
           (let* ((all-counters (append fast slow))
                  ; aleg unde il trimit: daca are nr_produse < ITEMS, ma uit la toate casele, altfel doar la slow
                  (target-index (car (if (<= n-items ITEMS) 
                                         (min-tt all-counters) 
                                         (min-tt slow))))) 
             (f-helper (cdr requests)
                       (update (add-to-counter name n-items) fast target-index)
                       (update (add-to-counter name n-items) slow target-index)
                       exits))]

          ; caz 4: trecerea timpului x
          [x (let* ((all-counters (append fast slow))
                    ; iau doar casele care au oameni (unde se poate termina timpul)
                    (busy (filter (λ (C) (not (queue-empty? (counter-queue C)))) all-counters))
                    ; gasesc cine e cel mai aproape de iesire
                    (m-et (if (null? busy)
                              #f
                              (min-et busy))))
               
               (cond
                 ; daca nu e nimeni la cozi sau timpul x e mai mic decat prima iesire
                 [(or (not m-et) (< x (cdr m-et)))
                  (f-helper (cdr requests)
                          (map (pass-time-through-counter x) fast)
                          (map (pass-time-through-counter x) slow)
                          exits)]
                 
                 ; daca un client termina (x >= timpul lui de iesire)
                 [else
                  ; aici m-et nu depinde de idx si t-min, pot folosi let simplu pt a respecta restrictia de minim un let
                  (let ((t-min (cdr m-et))  ; timpul pana la prima plecare
                        (idx (car m-et)))   ; indexul casei respective
                    ; gasesc casa pentru a-i afla numele clientului (folosind top din TDA-ul definit)
                    (let* ((target-c (findf (λ (c) (= (counter-index c) idx)) all-counters))
                           (client-name (car (top (counter-queue target-c)))))
                      ; la target-c am lasat let* deoarece client-name depinde de taget-c
                      ; fara el racket mi-ar fi dat eroare pt ca nu stia cine este target-c
                      ; la momentul definirii lui client-name
                      
                      ; "consum" timpul t-min si scot clientul
                      (f-helper (cons (- x t-min) (cdr requests)) ; restul de timp merge inapoi in coada
                                (update remove-first-from-counter (map (pass-time-through-counter t-min) fast) idx)
                                (update remove-first-from-counter (map (pass-time-through-counter t-min) slow) idx)
                                (cons (cons idx client-name) exits))))]))]))) ; adaug la lista de plecari
                    
  ; apelul initial al helper-ului cu lista de iesiri goala
  (f-helper requests fast-counters slow-counters '()))
