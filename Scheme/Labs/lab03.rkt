
;;helper functions
(define (id x) x)
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

;;Дефинирайте следните функции:
;; 1. (compose f g), която връща композицията на f и g, т.е. ((compose f g) x) е еквивалентно на (f (g x))
(define (1+ x) (+ x 1))
(define (compose f g) (lambda (x) (f (g x))))

;; 2. (const c), която приема константа c и връща функция, чиято стойност винаги е c.
(define (const c) (lambda(x) c))

;; 3. (fmax f g), която приема две едноаргументни числови функции f и g и връща функция,
;;чиято стойност в дадена точка x е по-голямата от стойностите на f и g в x.
(define (fmax f g) (lambda(x)
                     (let ((fx (f x))
                       (gx (g x)))
                       (if (> gx fx) gx fx)
                     )))

;; 4. (repeated n f x), която връща f, приложена n пъти върху x
(define (repeated n f x) (define (helper counter x) (if (< counter n) (helper (+ 1 counter) (f x)) x)) (helper 0 x))

;; 5. (repeat n f), която връща функция, чието действие има семантика на прилагане на f n пъти
(define (repeat n f) (lambda (x counter) (if (< counter n) ((repeat n f) (f x) (+ 1 counter)) x)))

;; 6. (count p? a b), която връща брой числа i от интервала [a, b], за които (p? i) --> #t
(define (count p? a b) (accumulate + 0 a b (lambda (i) (if (p? i) 1 0)) 1+))

;; 7. Предикат (any? p? a b), който проверява дали p връща истина за поне едно цяло число от интервала [a, b]
(define (any? p? a b) (accumulate (lambda (x y) (or x y)) #f a b p? 1+))

; 8. Предикат (all? p? a b), който проверява дали p връща истина за всяко цяло число от интервала [a, b]
(define (all? p? a b) (accumulate (lambda (x y) (and x y)) #t a b p? 1+))

;; 9. Функциите repeated и repeat от предния раздел
;;repeated
(define (repeated n f x) (accumulate (lambda (first second) (f second)) x 1 n (lambda (i) (f i)) 1+))

;;repeat
(define (repeat n f) (accumulate compose (lambda (i) i) 1 n (lambda (i) f) 1+))