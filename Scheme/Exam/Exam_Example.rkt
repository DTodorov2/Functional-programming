;;Примерно контролно
;;Едно естествено число наричаме свършено, ако е с 2 по-малко от сумата на всичките си делители по-малки от него.
;;a) Да се реализира функция done?, която проверява дали дадено число е свършено.
;;б) Да се реализира функция sum-almost-done, която по подадени естествени числа a и b намира сумата на всички числа в интервала [a; b],
;;които са по-близко до свършено число, отколкото до краищата на интервала.
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (sum-of-der num)
  (accumulate + 0 1 (- num 1) (lambda (x) (if (= 0 (remainder num x)) x 0)) (lambda (i) (+ i 1))))

(define (done? num)
  (if (= (+ num 2) (sum-of-der num))
      #t
      #f))

(define (get-closest-done-lower num)
  (if (= num 0)
      -1
      (if (done? num)
          num
          (get-closest-done-lower (- num 1)))))

(define (get-closest-done-upper upper-limit counter)
  (if (> counter upper-limit)
      counter
      (if (done? counter)
          counter
          (get-closest-done-upper upper-limit (+ counter 1)))))

(define (sum-almost-done lower-limit upper-limit)
  (define (helper num sum)
    (let ((closest-done-lower (get-closest-done-lower num))
          (closest-done-upper (get-closest-done-upper upper-limit num)))
      (if (> num upper-limit)
          sum
          (if (or (and (< (- num closest-done-lower) (- num lower-limit)) (< (- num closest-done-lower) (- upper-limit num)))
                  (and (< (- closest-done-upper num) (- upper-limit num)) (< (- closest-done-upper num) (- num lower-limit))))
              (helper (+ num 1) (+ sum num))
              (helper (+ num 1) sum)))))
  (helper lower-limit 0))

;; Разглеждаме стекова машина, която представя паметта си като списък от числа и символи и приема списък от инструкции,
;;които интерпретира по следния начин:
;; - ако поредната инструкция е число или символ, то се добавя на върха на стека.
;; - ако поредната инструкция е функция, тя се прилага над всички числа в стека (допуска се, че функцията приема само един параметър),
;;променяйки стойностите им в стека.
;; - ако поредната инструкция е наредена двойка от операция (двуместна функция) и число n, то горните две числа на стека се изваждат и
;;обратно на върха на стека се записва резултат от прилагането на операцията над тях.
;;Прилагането се повтаря до изчерпване на стека или достигане до символ, но не повече от n пъти.
;; - всички останали инструкции се игнорират.
(define (add-at-beggining l el)
  (append (list el) l))

(define (apply-to-nums func l)
  (if (null? l)
      (list)
      (if (number? (car l))
          (append (list (func (car l))) (apply-to-nums func (cdr l)))
          (append (list (car l)) (apply-to-nums func (cdr l))))))

(define (op-pair l func n)
  (if (or (null? (cdr l)) (= n 0) (symbol? (car l)) (symbol? (cadr l)))
      l
     (op-pair (append (list (func (car l) (cadr l))) (list-tail l 2)) func (- n 1))))

(define (run-machine l)
  (define (create-new-list l newL)
    (if (null? l)
        newL
        (if (or (number? (car l)) (symbol? (car l)))
            (create-new-list (cdr l) (add-at-beggining newL (car l)))
            (if (procedure? (car l))
                (create-new-list (cdr l) (apply-to-nums (car l) newL))
                (if (and (pair? (car l)) (procedure? (caar l)) (number? (cdar l)))
                    (create-new-list (cdr l) (op-pair newL (caar l) (cdar l)))
                    (create-new-list (cdr l) newL))))))
    (create-new-list l '()))

;;Казваме, че един списък е подсписък на друг, ако елементите на първия списък се срещат непосредствено последователно във втория.
;;Например, '(2 4) не е подсписък на '(1 2 3 4 5), но '(2 3 4) е. Казваме, че един списък от числа a се мажорира от списъка b,
;;ако двата списъка са с еднаква дължина n и ai ≤ bi за всяко i ∈ [0; n). Списък от списъци ll наричаме мажорен, ако е вярно,
;;че li се мажорира от подсписък на li+1 за всеки два съседни списъка li и li+1 в ll.
;;а) Да се реализира функция is-major?, която проверява дали даден списък от списъци от числа е мажорен.
(define (are-elements-lower l1 l2)
  (if (null? l1)
      #t
      (if (null? l2)
          #f
          (if (<= (car l1) (car l2))
              (nz (cdr l1) (cdr l2))
              #f))))

(define (is-first-major l1 l2)
  (if (null? l1)
      #t
      (if (null? l2)
          #f
          (if (are-elements-lower l1 l2)
              #t
              (majorira-li-se-purviq l1 (cdr l2))))))


(define (is-major? l)
  (if (null? (cdr l))
      #t
      (if (is-first-major (car l) (cadr l))
          (is-major? (cdr l))
          #f)))

;;Да се реализира функция find-longest-major, която намира най-дългия мажорен подсписък на даден списък от списъци от числа.
(define (longest l fromL longestL)
  (if (null? l)
      longestL
      (if (is-first-major fromL (car l))
          (longest (cdr l) (car l) (append longestL (list (car l))))
          longestL)))

(define (find-longest-major l)
  (define (helper l maxL)
    (if (null? (cdr l))
        maxL
        (if (> (length (longest l (car l) '())) (length maxL))
            (helper (cdr l) (longest l (car l) '()))
            (helper (cdr l) maxL))))
  (helper l '()))