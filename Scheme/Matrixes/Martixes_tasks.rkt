;;Дефинирайте следните функции:
;; 1. (mat-at m i j), която връща елементът на позиция i, j в матрицата m.
(define (mat-at m i j)
  (define (get_needed_bucket counter m) (if (< counter i) (if (null? m) (list) (get_needed_bucket (+ 1 counter) (cdr m))) (car m)))
  (define (get_needed_el bucket counter) (if (< counter j) (if (null? bucket) (list) (get_needed_el (cdr bucket) (+ 1 counter))) (car bucket)))
  (get_needed_el (get_needed_bucket 0 m) 0))

;; 2. (mat-map f m), която прилага функцията f върху всеки елемент на матрицата m.
(define (mat-map f m)
  (define (map-for-bucket bucket) (if (null? bucket) (list) (cons (f (car bucket)) (map-for-bucket (cdr bucket)))))
  (if (null? m) (list) (cons (map-for-bucket (car m)) (mat-map f (cdr m)))))

;; 3. (mat? m), която валидира, че m е матрица (всички елементи са числа; редовете са с равни дължини)
(define (mat? m)
  (define (lenFirst l) (if(null? l) 0 (+ 1 (lenFirst (cdr l)))))
  (let ((lenRow (lenFirst (if (null? m) (list) (car m)))))
  (define (isOnlyNumbers? l) (if (null? l) #t (if (number? (car l)) (isOnlyNumbers? (cdr l)) #f)))
  (define (validRow? l) (if (and (isOnlyNumbers? l) (= (lenFirst l) lenRow)) #t #f))
  (define (check-len m)
    (if (null? m) #t (if (and (validRow? (car m)) (isOnlyNumbers? (car m))) (check-len (cdr m)) #f)))
  (check-len m)))

;; 4. (scalmul x m), която умножава всеки елемент на матрицата m с числото x:
(define (scalmul x m)
  (define (mulRow l) (if (null? l) (list) (cons (* x (car l)) (mulRow (cdr l)))))
  (if (null? m) (list) (cons (mulRow (car m)) (scalmul x (cdr m))))
  )

;; 5. (transpose m), която транспонира матрицата m:
(define (remove-first-el-from-each-column m)
  (if (null? m) (list) (cons (cdr(car m)) (remove-first-el-from-each-column (cdr m)))))


(define (transpose m)
  (if (null? (car m)) (list) (cons (map car m) (transpose (remove-first-el-from-each-column m)))))

;; 6. (matmul m n), която умножава матриците m и n:
(define (get-first-row m)
  (if (null? (car m)) (list) (map car m)))

(define (multiply l1 l2 sumFinal)
  (if (or (null? l1) (null? l2)) sumFinal (multiply (cdr l1) (cdr l2) (+ sumFinal (* (car l1) (car l2))))))

(define (currRowNum m l)
  (if (or (null? l) (null? (car l))) (list) (cons (multiply m (get-first-row l) 0) (currRowNum m (map cdr l)))))

(define (matmul m n)
  (if (null? m) (list) (cons (currRowNum (car m) n) (matmul (cdr m) n))))