#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue
(define (empty-counter index)
  'your-code-here)
  
  
; TODO 7 (70p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 6 tipuri de cereri:
;   4 moștenite din etapa 3:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă deschisă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al caselor deschise depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
;   plus 2 noi:
;   - (close <index>) - închide casa cu indexul <index> (casa există deja)
;   - (open <index>) - deschide casa cu indexul <index> (casa există deja)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa DESCHISĂ cu tt minim la care are voie;
;   se garantează că persoana poate fi distribuită la o casă
; - nicio modificare pentru situația când o casă suferă o întârziere
; - dacă tt-ul mediu pentru toate casele DESCHISE > <average>,
;   adaugă case slow până când media <= <average>
; - nicio modificare în modelarea trecerii timpului
; - o casă care se închide nu mai primește clienți noi și:
;   - primul client (dacă există) își continuă treaba la această casă
;   - restul clienților se redistribuie la celelalte case,
;     în ordinea în care erau așezați la coadă
; - o casă care se deschide redevine disponibilă pentru clienți
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista cozilor nevide în starea finală, sortată după indexul casei
;   - elementele listei au forma (index_casă . coadă) (coada este de tip queue)
(define (serve requests fast-counters slow-counters)
  'your-code-here)