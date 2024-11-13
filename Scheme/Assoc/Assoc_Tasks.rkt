;;Дефинирайте Функцията (assoc-set l k v), която връща нов асоциативен списък, който представлява l с добавена двойка с ключ k и стойност v.
(define (contains? l k)
  (if (null? l) #f (if (equal? (car(car l)) k) #t (contains? (cdr l) k))))

(define (assoc-set l k v)
  (if (or (null? l) (equal? (car(car l)) k)) (cons (cons k v) (if (null? l) (list) (cdr l))) (cons (car l) (assoc-set (cdr l) k v))))

;;Дефинирайте Функцията (assoc-get l k), която по даден ключ връща стойността, асоциирана с него (ако такъв ключ не съществува, връща #f).
(define (assoc-get l k)
  (if (null? l) #f (if (equal? (caar l) k) (car l) (assoc-get (cdr l) k))))

;;Дефинирайте Функцията (assoc-map f l), която връща нов асоциативен списък, който е резултатът от прилагането на f върху стойностите в l.
(define (assoc-map f l)
  (if (null? l) (list) (cons (cons (caar l) (f (cdar l))) (assoc-map f (cdr l)))))

;;Дефинирайте Функцията (assoc-filter p l), която връща нов асоциативен списък, който съдържа само двойките, за чиято стойност p връща "истина".
(define (assoc-filter p l)
  (if (null? l)
      (list)
      (if (p (cdar l))
          (cons (car l) (assoc-filter p (cdr l)))
          (assoc-filter p (cdr l)))))

;;Дефинирайте Функцията (assoc-merge l1 l2), която връща нов асоциативен списък, за който:
;;множеството от ключове е обединението на ключовете на l1 и l2
;;ако (k . v) се среща в l2, то (k . v) се среща в резултата
;;иначе, ако (k . v) се среща в l1, то (k . v) се среща в резултата.
(define (assoc-merge l1 l2)
  (if (null? l2) l1 (assoc-merge (assoc-set l1 (caar l2) (cdar l2)) (cdr l2))))