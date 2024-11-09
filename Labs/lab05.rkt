(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

;; 1. Дефинирайте функция (sum l) чрез foldr/foldl
(define (sum l) (foldl + 0 l))

;; 2. Дефинирайте функция (len l) чрез foldr/foldl
(define (len l)
  (foldr (lambda (first second) (+ 1 second)) 0 l))

;; 3. Дефинирайте функците (any? p l) и (all? p l) чрез foldr/foldl
(define (any? p? l) (foldr (lambda (first second) (or (p? first) second)) #f l))

(define (all? p? l) (foldr (lambda (first second) (and (p? first) second)) #t l))

;; 4. Дефинирайте функциите foldr1 и foldl1, които са подобни на foldr/foldl,
;;но не приемат първичен елемент (гърмят при празен списък)
(define (foldl1 op l) (foldl op (car l) (cdr l)))

;; 5. Дефинирайте функциите minimum и maximum чрез foldr1/foldl1
(define (minimum l) (foldl1 min l))

(define (maximum l) (foldl1 max l))

;; 6. Дефинирайте функция map чрез foldr/foldl
(define (maps f l)
  (foldr (lambda (first second) (cons (f second) first)) '() l))

;; 7. Дефинирайте функция filter чрез foldr/foldl
(define (filter p? l)
  (foldr (lambda (first second) (if (p? first)
                                    (cons first second)
                                    second)) '() l))

;; 8. Дефинирайте функция (reverse l) чрез foldr/foldl
(define (reverses l)
  (foldl (lambda (first second) (cons second first)) '() l))

;; 9. Дефинирайте функция (take n l)
(define (take n l)
  (define (helper counter l) (if (< counter n) (if (null? l) l (cons (car l) (helper (+ counter 1) (cdr l)))) '()))
  (helper 0 l))

;; 10. Дефинирайте функция (drop n l)
(define (drop n l)
  (define (helper counter l) (if (< counter n) (if (null? l) (list) (helper (+ 1 counter) (cdr l))) l))
  (helper 0 l))

;; 11. Дефинирайте функция (take-while p? l)
(define (take-while p? l)
  (if (p? (car l)) (cons (car l) (take-while p? (cdr l))) (list)))

;; 12. Дефинирайте функция (drop-while p? l)
(define (drop-while p? l)
  (if (p? (car l)) (drop-while p? (cdr l)) l))

;; 13. Дефинирайте функция (zip l1 l2)
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '() (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

;; 14. Дефинирайте функция (zip-with f l1 l2)
(define (zip-with f l1 l2)
  (if (or (null? l1) (null? l2)) '() (cons (f (car l1) (car l2)) (zip-with f (cdr l1) (cdr l2)))))




