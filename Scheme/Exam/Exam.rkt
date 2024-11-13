;; често използвани функции
(define (id x) x)
(define (1+ x) (+ x 1))
(define (compose f g) (lambda (x) (f (g x))))

;; натрупване от по-висок ред
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; стандартни генератори на списъци
(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

;; фунции от по-висок ред за списъци
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

;; 1. Да се дефинира функция (sum-woth-position n), която приема неотрицателно число n и връща ново число,
;;което се получава като всяка цифра d на n се замести с числото, което се получава като d се събере с
;;номера на позицията си. Позициите се броят отдясно наляво, като цифрата на единиците е на позиция 1.
(define (get-num-len n)
  (if (= n 0)
      0
      (+ 1 (get-num-len (quotient n 10)))))

(define (helper n index len pow newList)
    (if (> index len)
        newList
        (helper (quotient n 10) (+ index 1) len (* pow 10) (append (list(+ (remainder n 10) index)) newList))))

(define (count-digits-of-list l res)
  (if (null? l)
      res
      (count-digits-of-list (cdr l) (+ res (get-num-len (car l))))))

(define (sum-with-position n)
  (define (helper1 multiply newNum list)
    (if (= multiply 0)
        newNum
        (helper1 (- multiply (get-num-len (car list))) (+ newNum (* (expt 10 (- multiply (get-num-len (car list)))) (car list))) (cdr list))))
  (helper1 (count-digits-of-list (helper n 1 (get-num-len n) 1 '()) 0) 0 (helper n 1 (get-num-len n) 1 '()) ))

;; 2. Да се дефинира функция (maxmin fm l), която по дадена непразна матрица от едноместни числови функции fm и непразен списък
;;от числа l, намира за всеки ред в матрицата минималното число, което може да се получи след прилагане на някоя функция в реда
;;над някой елемент на списъка l, и връща най-голямото такова минимално число. Матриицата се представя като списък от редовете си.
(define (2+ x) (+ 2 x))
(define (square x) (* x x))
(define (get-min-el-from-row l1 l2 minEl)
  (if (null? l1)
       minEl
       (if (< minEl (apply min (map (car l1) l2)))
           (get-min-el-from-row (cdr l1) l2 minEl)
           (get-min-el-from-row (cdr l1) l2 (apply min (map (car l1) l2))))))

(define (make-list-from-elements l1 l2 newList)
  (if (null? l1)
      newList
      (make-list-from-elements (cdr l1) l2 (append newList (list (get-min-el-from-row (car l1) l2 (apply min (map (caar l1) l2))))))))

(define (maxmin l1 l2)
  (+ 0.0 (apply max (make-list-from-elements l1 l2 '()))))

;; 3. Да се дефинира функция (minimum-odd-nodes-prod bt), която приема непразно двоично дърво от цели числа и връща
;;път от корена, в който произведението на нечетните елементи е минимално. Ако има няколко такива пътя, да се върне
;;такъв с максимална дължина.
(define (get-left-child t) (cadr t))
(define (get-right-child t) (caddr t))
(define (empty-tree? t) ((and (null? (cadr t)) (null? (caddr t)))))

(define (find-path t newPath)
  (if (and (null? (cadr t)) (null? (caddr t)))
      (list (append newPath (list (car t))))
      (append (list (append newPath (list (car t)))) (find-path (get-left-child t) (append newPath (list (car t)))) (find-path (get-right-child t) (append newPath (list(car t)))))))

(define (mult-list l)
  (if (null? l)
      1
      (if (even? (car l))
          (mult-list (cdr l))
          (* (car l) (mult-list (cdr l))))))

(define (find-min-odd-mult l minMult)
  (if (null? l)
      minMult
      (if (< minMult (mult-list (car l)))
          (find-min-odd-mult (cdr l) minMult)
          (find-min-odd-mult (cdr l) (mult-list (car l))))))

(define (get-min-mult-lists l2 newList)
  (define (helper l1 min-mult newList)
  (if (null? l1)
      newList
      (if (= min-mult (mult-list (car l1)))
          (helper (cdr l1) min-mult (append newList (list (car l1))))
          (helper (cdr l1) min-mult newList))))
  (helper (find-path l2 '()) (find-min-odd-mult (find-path l2 '()) (mult-list (car (find-path l2 '())))) '()))

(define (longest l)
  (apply max (map length l)))

(define (minimum-odd-nodes-prod l)
  (let ((len (longest (get-min-mult-lists l '()))))
  (filter (lambda (x) (= (length x) len)) (nz l '()))))