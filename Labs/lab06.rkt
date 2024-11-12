;;1. Основни конструкции: (make-tree root left right), (root-tree tree), (left-tree tree),
;;(right-tree tree), (empty-tree? tree), (leaf-tree? tree), (tree? x)
(define (make-tree root left right)
  (list root left right))

(define (root-tree tree)
  (if (null? tree)
      (list)
      (car tree)))

(define (left-tree tree)
  (if (null? tree)
      (list)
      (cadr tree)))

(define (right-tree tree)
  (if (null? tree)
      (list)
      (caddr tree)))

(define (empty-tree? tree)
  (if (null? tree)
      #t
      #f))

(define (leaf-tree? tree)
  (if (null? tree)
      #t
      (if (and (null? (left-tree tree)) (null? (right-tree tree)))
          #t
          #f)))

(define (tree? tree)
  (if (null? tree)
      #t
      (if (and (list? tree) (= (length tree) 3))
          (if (and (tree? (right-tree tree)) (tree? (left-tree tree)))
              #t
              #f)
          #f)))
;;Обхождане: (pre-order t), (in-order t) и (post-order t),които връщат списък от елементите
;;на дървото, обходено съотвено корен-ляво-дясно, ляво-корен-дясно и ляво-дясно-корен.
(define (pre-order t)
  (if (null? t)
      (list)
      (append (list (car t)) (pre-order (cadr t)) (pre-order (caddr t)))))

(define (in-order t)
  (if (null? t)
      (list)
      (append (in-order (cadr t)) (list (car t)) (in-order (caddr t)))))

(define (post-order t)
  (if (null? t)
      (list)
      (append (post-order (cadr t)) (post-order (caddr t)) (list (car t)))))

;;3. Напишете функция (map-tree f t), която заменя всеки връх x от дървото t с (f x).
(define (map-tree f t)
  (if (null? t)
      (list)
      (if (leaf-tree? t)
          (append (list (f (car t)) (list) (list)))
          (append (list (f (car t))) (list (map-tree f (cadr t))) (list (map-tree f (caddr t)))))))

;;4. Напишете функция (height t), която намира височината на дървото t. Това е броят на върховете в най-дългия път.
(define (height tree)
  (if (null? tree)
      0
      (max (+ 1 (height (cadr tree))) (+ 1 (height (caddr tree))))))

;;5. Напишете функция (level n t), която връща списък с всички върхове от дървото с дълбочина n.
;;Дълбочината на корена е 0.
(define (level n t)
  (if (null? t)
      (list)
      (if (= n 0)
          (list (car t))
          (append (level (- n 1) (cadr t)) (level (- n 1) (caddr t))))))

;;6. Напишете функция (count-leaves t), която връща броя листа на t.
;;Листо е връх от дърво, който няма наследници.
(define (count-leaves t)
  (if (null? t)
      0
      (if (leaf-tree? t)
          1
          (+ (count-leaves (cadr t)) (count-leaves (caddr t))))))


;;7. Напишете функция (remove-leaves t), която връща дървото t, премахвайки листата му.
(define (remove-leaves t)
  (if (null? t)
      (list)
      (if (leaf-tree? t)
          (list)
          (append (list (car t)) (list (remove-leaves (cadr t))) (list (remove-leaves (caddr t)))))))

;;8. Напишете функция (invert t), която разменя левите поддървета на t с десните.
(define (invert t)
  (if (null? t)
      (list)
      (append (list (car t)) (list (invert (caddr t))) (list (invert (cadr t))))))

;;9. Напишете функция (bst? t), която намира дали t е двоично наредено дърво.
(define (bst? t)
  (define (helper sorted-tree)
    (if (null? (cdr sorted-tree))
        #t
        (if (> (car sorted-tree) (cadr sorted-tree))
            (helper (cdr sorted-tree))
            #f)))
  (or (helper (in-order t)) (helper (reverse (in-order t)))))