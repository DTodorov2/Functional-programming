;;зад 1 -> Дефинирайте функция (len l), която намира дължината на списъкa l:
(define (len l)
  (if (null? l) 0 (+ 1 (len (cdr l)))))

;;зад 2 -> Дефинирайте функция (minimum l), която намира най-малкия елемент на списъкa l:
(define (minimum l)
  (define (helper min l) (if (null? l) min (if (> min (car l)) (helper (car l) (cdr l)) (helper min (cdr l)))))
  (helper (car l) (cdr l)))

;;зад 3 -> Дефинирайте функция (any? p l), която проверява дали съществува елемент в l, за който е изпълнен предикатът p:
(define (any? p l)
  (if (null? l)
      #f
      (if (p (car l)) #t (any? p (cdr l)))))

;;Дефинирайте аналогичния предикат (all? p l) чрез any?
(define (all? p l) (not (any? (lambda (i) (not (p i))) l)))

;;зад 4 -> Дефинирайте функция (member? x l), която проверява дали елементът x се съдържа в списъка l:
(define (member? x l) (if (null? l)
                          #f
                          (if (equal? x (car l)) #t
                              (member? x (cdr l)))))

;;зад 5 -> Дефинирайте функция (at n l), която връща елемента, намиращ се на позиция n (броим от 0) в списъка l, или #f, ако позицията е извън списъка:
(define (at n l)
  (if (null? l)
      #f
      (if (= n 0) (car l) (at (- n 1) (cdr l)))))

;;зад 6 -> Дефинирайте функция (push-back x l), която добавя елемента x на края на списъка l:
(define (push-back x l) (if (null? l)
                            (list x)
                            (cons (car l) (push-back x (cdr l)))))

;;зад 7 -> Дефинирайте функция (reverse l), която връща списък с елементите на l в обратен ред:
(define (rev l)
  (define (helper acc l) (if (null? l) acc (helper (cons (car l) acc) (cdr l))))
  (helper '() l))

;;зад 8 -> Дефинирайте функция (insert x n l), която вкарва елемента x на позиция n в списъка l (ако n е след края на l, вкарваме x накрая):
(define (insert x n l)
  (define (helper counter l)
    (if (null? l)
        (if (> counter n) '() (list x))
        (if (= counter n)
            (cons x (helper (+ counter 1) l))
            (cons (car l) (helper (+ counter 1) (cdr l))))))
  (helper 0 l))

;;зад 9 -> Дефинирайте функция (range a b), която генерира целочисления интервал [a, b]:
;;вариант без accumulate функцията
(define (range a b) (if (> a b) '() (cons a (range (+ a 1) b))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

;;вариант с accumulate функцията
(define (range1 a b) (accumulate cons '() a b (lambda (i) i) (lambda (i) (+ 1 i)) ))

;;зад 10 -> Дефинирайте функция (map f l), която прилага f върху всеки елемент на списъка l:
(define (map1 f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map1 f (cdr l)))))

;;зад 11 -> Дефинирайте функция (filter p l), която съставя нов списък, съдържащ само елементите на l, за които е изпълнен предикатът p:
(define (filter1 p? l)
  (if (null? l)
      '()
      (if (p? (car l))
          (cons (car l) (filter1 p? (cdr l)))
          (filter1 p? (cdr l)))))

;;зад 12 -> Дефинирайте функция (reduce op init l), която пресмята (op l[0] (op l[1] (op l[2] ... (op l[n] init)...)))
;;(ако имаме подаден празен списък, резултатът е init). Използвайте reduce, за да дефинирате функциите map, filter и accumulate.
(define (reduce op init l)
  (if (null? l)
      init
      (op (car l) (reduce op init (cdr l)))))

(define (mapRed f l) (reduce (lambda (first second) (cons (f first) second)) (list) l))

(define (filterRed p? l) (reduce (lambda (x second) (if (p? x) (cons x second) second)) '() l))