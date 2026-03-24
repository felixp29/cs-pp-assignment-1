#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul
;; până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client
;; + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat Ana cu 3 produse, apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute =>
;; et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO 1 (5p)
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO 2 (15p)
; Implementați o funcție care aplică o transformare f
; casei cu un anumit index.
; f = funcție unară cu un parametru de tip casă,
; counters = listă de case,
; index = indexul casei care trebuie transformată
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index,
; întoarceți lista nemodificată.
(define (update f counters index)
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

; TODO 3 (7.5p)
; Memento: tt+ crește tt-ul unei case cu un număr de minute.
; Obs: tt+ afectează doar câmpul tt, nu și câmpul et.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument
;   funcției update în cel mai facil mod
; Obs: Facil înseamnă că o aplicație parțială a funcției tt+ 
; va produce o funcție unară cu parametru de tip casă, fără
; să fie nevoie de funcții anonime sau funcții auxiliare.
; Scheletul nu menționează parametrii funcției tt+, întrucât
; trebuie să determinați voi înșivă cum este cel mai bine
; ca tt+ să își primească parametrii.
;
; Apoi implementați funcția checker-tt+, care apelează funcția
; tt+ pe o casă și un număr de minute.
; Funcția checker-tt își precizează clar parametrii și
; poate fi testată, acesta este singurul său rol.
; RESTRICȚII (5p)
;  - Implementați tt+ conform cerinței anterioare.
(define ((tt+ minutes) C)
  (struct-copy counter C [tt (+ (counter-tt C) minutes)]))

; am inversat ordinea parametrilor fata de etapa 1 (acum minutes e primul)
; pentru ca prelucrez parametrul minutes in aceasta functie si rezultatul
; (tot o functie care asteapta alt argument) il pasez mai departe
; deci functia tt+ imi permite aplicare partiala
; am facut asta definind functia tt+ in stil curry ((tt+ minutes) C)


(define (checker-tt+ C minutes)
  ((tt+ minutes) C))


; TODO 4 (7.5p)
; Implementați o funcție care crește et-ul unei case
; cu un număr dat de minute.
; Obs: et+ afectează doar câmpul et, nu și câmpul tt.
; Păstrați formatul folosit pentru tt+.
; Apoi implementați funcția checker-et+ care apelează
; et+, pentru testare.
; RESTRICȚII (5p)
;  - Implementați et+ conform cerinței anterioare.
(define ((et+ minutes) C)
  (struct-copy counter C [et (+ (counter-et C) minutes)]))

; struct-copy pentru ca trebuie sa creez o noua casa ca sa ii actualizez et-ul,
; nu ii modific et-ul casei existente (imutabilitate)
                        
(define (checker-et+ C minutes)
  ((et+ minutes) C))


; TODO 5 (10p)
; Memento: add-to-counter adaugă o persoană
; (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter din aceleași
; rațiuni pentru care ați actualizat funcția tt+.
; Atenție la cum se modifică tt și et!
; Apoi implementați funcția checker-add-to-counter
; care apelează add-to-counter, pentru testare.
; RESTRICȚII (5p)
;  - Implementați add-to-counter conform cerinței anterioare.
(define (add-to-counter name n-items)
  (match-lambda
    [(counter index tt et queue) (if (null? queue)
                                     ; caz 1: casa e goala tt creste, et devine et-ul actual + n-items, queue are un om - cel tocmai adaugat
                                     (make-counter index (+ tt n-items) (+ et n-items) (list (cons name n-items)))
         
                                     ; caz 2: casa are oameni, tt creste, et neschimbat, omul e pus la final de queue
                                     (make-counter index (+ tt n-items) et (append queue (list (cons name n-items)))))]))

; (match-lambda
;    [pattern-1    corp-1]
;    [pattern-2    corp-2]
;    ...)

(define (checker-add-to-counter C name n-items)
  ((add-to-counter name n-items) C))


; TODO 6 (15p)
; Întrucât vom folosi atât min-tt (implementat în etapa 1)
; cât și min-et (funcție nouă), definiți o funcție mai abstractă
; din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă nevidă de case și
; întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (la același et, este preferată casa cu indexul cel mai mic)
; Obs: în etapele 2-4, listele de case sunt sortate după index.
; RESTRICȚII (10p - 2*5p)
;  - min-tt și min-et vor fi aplicații parțiale ale funcției abstracte.
(define (min-by-field field) ; field va fi campul tt sau et din counter
  (λ (counters)
    (define (find-min remaining best-so-far)
      (if (null? remaining)
          best-so-far
          (if (< (field (car remaining)) (cdr best-so-far))
              (find-min (cdr remaining) (cons (counter-index (car remaining)) (field (car remaining))))
              (find-min (cdr remaining) best-so-far))))

    (find-min (cdr counters) (cons (counter-index (car counters)) (field (car counters))))))
                        

(define min-tt (min-by-field counter-tt)) ; folosind funcția de mai sus
(define min-et (min-by-field counter-et)) ; folosind funcția de mai sus


; TODO 7 (10p)
; Implementați o funcție care scoate prima persoană
; din coada unei case.
; Funcția presupune, fără să verifice, că există
; minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin
; modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva,
; înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (match C
    [(counter index _ _ (cons _ rest-queue)) (if (null? rest-queue)
                                                 ; cazul in care ramane goala
                                                 (make-counter index 0 0 '())
                                                 ; cazul in care mai raman oameni: calculez noile valori direct
                                                 (make-counter index
                                                               (apply + (map cdr rest-queue)) ; noul tt: suma produselor ramase
                                                               (cdr (car rest-queue)) ; noul et: produsele celui de-al doilea om
                                                               rest-queue))]))
            
; (match obiect-de-potrivit
;      [tipar-1    corp-1]
;      [tipar-2    corp-2])

; TODO 8 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a procesa liste de case)
; - requests conține 4 tipuri de cereri (două în plus față de etapa 1):
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care este
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (ca înainte, dar folosind fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc
;   (chiar dacă nu are clienți)
; - persoana cea mai avansată este prima persoană la casa cu et-ul minim
;   (dintre casele care au clienți)
;   (dacă nicio casă nu are clienți, ignoră cererea)
; - dacă tt-ul mediu pentru toate casele > <average>,
;   adaugă case slow până când media <= <average>
;   (puteți determina matematic de câte case noi este nevoie sau
;   să adăugați recursiv una câte una cât timp este necesar)
; Considerați casele indexate de la 1 și mereu sortate după index.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie adăugați întâi casa 16, apoi casa 17, etc.
; RESTRICȚII (25p - 5*5p)
;  - Folosiți minim două funcționale predefinite în Racket. (2*5p)
;  - Nu apelați checker-tt+, checker-et+, checker-add-to-counter,
;    ci doar tt+, et+, add-to-counter. (3*5p) 
(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        
        ; 2. urmatoarea cerere din lista requests - intarzierile
        [(list 'delay index minutes)
         (if (<= index (length fast-counters))
             (serve (cdr requests)
                    (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                    slow-counters)
             (serve (cdr requests)
                    fast-counters
                    (update (et+ minutes) (update (tt+ minutes) slow-counters index) index)))]

        ; 3. plecarea clientului
        [(list 'remove-first)
         ; folosesc filter pt a gasi casele cu persoane
         (if (null? (filter (match-lambda [(counter _ _ _ q) (not (null? q))]) (append fast-counters slow-counters)))
             ; trec peste daca toate casele sunt goale
             (serve (cdr requests) fast-counters slow-counters)
             ; else caut indexul cu et cel mai mic
             (if (<= (car (min-et (filter (match-lambda [(counter _ _ _ q) (not (null? q))]) (append fast-counters slow-counters)))) (length fast-counters))
                 (serve (cdr requests) (update remove-first-from-counter fast-counters (car (min-et (filter (match-lambda [(counter _ _ _ q) (not (null? q))]) (append fast-counters slow-counters))))) slow-counters)
                 (serve (cdr requests) fast-counters (update remove-first-from-counter slow-counters (car (min-et (filter (match-lambda [(counter _ _ _ q) (not (null? q))]) (append fast-counters slow-counters))))))))]

        ; 4. ensure
        [(list 'ensure average)
         (if (> (/ (apply + (map counter-tt (append fast-counters slow-counters)))
                   (length (append fast-counters slow-counters)))
                average)
             ; media e mai mare decat average, adaug o casa si reprocesez aceeasi cerere
             (serve requests
                    fast-counters
                    (append slow-counters (list (make-counter(+ 1 (length (append fast-counters slow-counters))) 0 0 '()))))
             ; media e mai mica
             (serve (cdr requests) fast-counters slow-counters))]

        ; 1. asez persoana la casa
        [(list name n-items)
         (if (<= n-items ITEMS)
             ; are mai putine produse decat ITEMS -> poate merge la oricce casa
             ; caut casa cu tt cel mai mic dintre toate
             (if (<= (car (min-tt (append fast-counters slow-counters))) (length fast-counters))
                 ; merge la o casa fast
                 (serve (cdr requests)
                        (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                        slow-counters)
                 ; merge la o casa slow
                 (serve (cdr requests)
                        fast-counters
                        (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters))))))
             ; are mai multe produse decat ITEMS -> merge la un slow counter
             (serve (cdr requests)
                    fast-counters
                    (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))])))

; l-am pus pe 1. la final deoarece daca (list name n-items) era la inceputul match-ului ar fi luat si
; cererea (ensure x) ca fiind un client
; ordinea de procesare a cererilor e asigurata de (car requests)
; ordinea clauzelor din match previne incurcarea lui ensure cu numele clientilor

