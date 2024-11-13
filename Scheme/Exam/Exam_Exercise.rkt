;;Помощни функции
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

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

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

;;Дефинирайте функцията (palindrome? n), която връща дали числото n е палиндром.
(define (make-num-list num)
  (if (= num 0) (list) (cons (remainder num 10) (make-num-list (quotient num 10)))))

(define (palindrome? num) (if (equal? (make-num-list num) (reverse (make-num-list num))) #t #f))

;;Дефинирайте функцията (count-palindromes a b), която връща броя на числата в интервала [a .. b], който са палиндроми.
(define (1+ x) (+ 1 x))
(define (count-palindromes a b)
  (if (> a b) 0 (if (palindrome? a) (+ 1 (count-palindromes (1+ a) b)) (count-palindromes (1+ a) b))))

;;Дефинирайте функцията (sum-primes n k), която намира сбора на първите n прости числа по-големи от k.
(define (prime? n)
  (define (for i)
    (if (>= i n) #t (if (= 0 (remainder n i)) #f (for (+ i 1)))))
  (for 2))

(define (sum-primes n k)
  (if (= n 0) 0 (if(prime? (+ k 1)) (+ (+ k 1) (sum-primes (- n 1) (+ k 1))) (sum-primes n (+ k 1)))))

;;Да се дефинира функцията (prime-factors n), която приема целочисления аргумент n и връща списък от двойки от тип (pi . ki),
;;където pi e i-тия прост делител на n, a ki степента на pi във факторизацията на n.
(define (times-dev i n)
  (if (or (not (= 0 (remainder n i))) (= n 0)) 0 (+ 1 (times-dev i (quotient n i)))))

(define (minNum i n)
  (if (not (= 0 (remainder n i))) n (minNum i (quotient n i))))

(define (prime-factors n)
  (define (for i n)
    (if (> i n) '() (if (and (prime? i) (= 0(remainder n i))) (cons (cons i (times-dev i n)) (for (+ i 1) (minNum i n))) (for (+ i 1) n))))
  (for 2 n))

;;Нека е даден списък l; да се дефинират следните функции:

;;1. (increasing? l), която проверява дали елементите на l са подредени в нарастващ ред
(define (increasing? l)
  (if (null? (cdr l))
      #t
      (if (< (cadr l) (car l))
          #f
          (increasing? (cdr l)))))

;;2. (progression? l), която проверява дали елементите на l образуват аритметична прогресия
(define (find-diff l) (if (= 1 (length l)) 0 (- (cadr l) (car l))))

(define (progression? l)
  (let ((diff (find-diff l)))
  (define (helper l)
    (if (or (= (length l) 1))
        #t
        (if (not (equal? (find-diff l) diff))
            #f
            (helper (cdr l)))))
  (helper l)))

;;3. (has-duplicates? l), която проверява дали l съдържа повтарящи се елементи
(define (contains? l n)
  (if (null? l)
      #f
      (if (equal? (car l) n)
          #t
          (contains? (cdr l) n))))

(define (has-duplicates? l)
  (if (null? l)
      #f
      (if (contains? (cdr l) (car l))
          #t
          (has-duplicates? (cdr l)))))

;;Да се дефинира фунцкия (dedup l), която премахва повтарящите се елементи от списъка l.
(define (dedup l)
  (if (null? l)
      (list)
      (if (contains? (cdr l) (car l))
          (dedup (cdr l))
          (cons (car l) (dedup (cdr l))))))

;;Нека са дадени два списъка l и s, които не съдържат повтарящи се елементи (т.е. l и s са множества).
;;Да се дефинират следните функции:

;;1. (union l s), която връща тяхното обединение
(define (union l1 l2)
   (if (and (null? l1) (null? l2))
       (list)
       (if (null? l1)
           (cons (car l2) (union l1 (cdr l2)))
           (cons (car l1) (union (cdr l1) l2)))))

;;2. (intersection l s), която връща тяхното сечение
(define (intersection l1 l2)
  (if (null? l1)
      (list)
      (if (contains? l2 (car l1))
          (cons (car l1) (intersection (cdr l1) l2))
          (intersection (cdr l1) l2))))

;;3. (product l s), която връщя тяхното картезианско произведение
(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (make-list l num)
  (if (null? l)
      (list)
      (cons (cons num (car l)) (make-list (cdr l) num))))

(define (product l1 l2)
  (if (null? l1)
      (list)
      (append (make-list l2 (car l1)) (product (cdr l1) l2))))

;;Да се дефинира функциятя (scalar-product xs ys), която приема два списъка xs и ys и връща тяхното скаларно произведение.
(define (scalar-product xs ys)
  (if (null? xs)
      0
      (+ (* (car xs) (car ys)) (scalar-product (cdr xs) (cdr ys)))))

;;Дефинирайте функцията (diagonal-product matrix), която приема числова квадратна матрица matrix и връща скаларното
;;произведение на двата и диагонала.
(define (get-num-at-index l ind)
  (if (= ind 0)
      (car l)
      (get-num-at-index (cdr l) (- ind 1))))

(define (get-diag l)
  (define (for l ind)
    (if (null? l)
        (list)
        (cons (get-num-at-index (car l) ind) (for (cdr l) (+ 1 ind)))))
  (for l 0))

(define (diagonal-product matrix)
  (scalar-product (get-diag matrix) (reverse (get-diag (reverse matrix)))))


;;Да се дефинира функцията (matrix-multiply m t), която приема матриците m и t (представени като списъци от списъци)
;;и връща тяхното матрично произведение.
(define (matrix-multiply m t)
  (define (nz m trans)
    (foldr (lambda (first second) (cons (foldr (lambda (n m) (append (list (scalar-product first n)) m)) '() trans) second)) '() m))
  (nz m (apply map list t)))

;;Да се дефинира функция (max-ordered-sublist lst), която намира най-дългия възходящо сортиран подсписък на списъка от числа lst.
(define (max-ordered-sublist l)
  (define (helper max-list l1 max-arr)
    (if (null? l1)
        max-arr
        (if (< (car l1) (car (reverse max-list)))
            (helper (list (car l1)) (cdr l1) (if (> (length max-list) (length max-arr)) max-list max-arr))
            (helper (append max-list (list (car l1))) (cdr l1) max-arr))))
  (helper (list (car l)) (cdr l) (list (car l))))

;;Дефинирайте функция (replace lst dict), която получава списък lst и асоциативен списък dict и връща нов списък,
;;в който елементите на lst са заменени с асоциацията им в dict, ако имa такава, a иначе не се променят.
(define (replace l dict)
  (foldr (lambda (first second) (cons (foldr (lambda (n m) (append (if (equal? (car n) first) (cdr n) first))) '() dict) second)) '() l))

;;Дефинирайте функцията (closest-point xys), която приема списък от точки в равнината (представени чрез двойки (x . y))
;;и връща едноаргументна функция, чиято стойност в дадена точка p e най-близката до p точка от xys.
(define (distance-points p1 p2)
  (sqrt (+ (* (- (car p2) (car p1)) (- (car p2) (car p1))) (* (- (cdr p2) (cdr p1)) (- (cdr p2) (cdr p1))))))

(define (closest-point xys)
  (define (find-it closest closes-path-len xys)
  (lambda (p)
    (if (null? xys)
        closest
        (if (< (distance-points p (car xys)) closes-path-len)
            ((find-it (car xys) (distance-points p (car xys)) (cdr xys)) p)
            ((find-it closest closes-path-len (cdr xys)) p)))))
  (lambda (point) ((find-it (car xys) (distance-points point (car xys)) (cdr xys)) point)))

;;Дефинирайте функцията (flatten l), която приема произволно дълбок списък l и го превръща в "плосък".
(define (flatten l)
  (foldr (lambda (first second) (append (if (number? first) (list first) (flatten first)) second)) '() l))

;;Нека е дадено роботче с първоначална позиция (0 . 0), както и списък с ходове moves представени като двойки (dx . dy).
;;Дефинирайте следните функции:
(define (make-pair-list pair)
  (if (null? pair)
      '()
      (list (car pair) (cdr pair))))

;;1. (position moves), която връща крайната позиция на робота след като изпълни всички ходове в moves.
(define (position moves)
    (foldl (lambda (first second) (map + first (make-pair-list second))) (list 0 0) moves))

;;2. (distance moves), която връща дължината на пътя изминат от робота. За целта може да приемете,
;;че по време на всеки ход, роботът се движи по права линия.
(define (distance moves)
  (define (nz sum moves)
    (if (null? (cdr moves))
        sum
        (nz (+ sum (distance-points (car moves) (cadr moves))) (cdr moves))))
  (nz 0 moves))

;;Дефинирайте функцията (adjacency-list nodes edges), която приема списък с върхове nodes и
;;списък с ребра edges на даден ориентиран граф (в който всяко ребро е представено като двойка (from . to))
;;и връща списъка на наследниците на съответния граф.
(define (create-successor-list node list-edges)
  (filter (lambda (x) (equal? (car x) node)) list-edges))

(define (adjacency-list l1 l2)
  (if (null? l1)
      '()
      (cons (append (cons (car l1) (list (map cdr (create-successor-list (car l1) l2))))) (adjacency-list (cdr l1) l2))))

;;Дефинирайте функцията (path? edges nodes), която приема списъс с ребрата edges на даден ориентиран граф и списък от върхове
;;nodes и връща дали списъкът nodes е път в графа описан от edges.
(define (path? edges nodes)
  (if (null? (cdr nodes))
      #t
      (if (member (cons (car nodes) (cadr nodes)) edges)
          (path? edges (cdr nodes))
          #f)))