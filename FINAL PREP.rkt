;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |FINAL PREP|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(check-expect (list? (list 1 2 3)) #true)
(check-expect (list? 10) #false)

;accessing elements


(check-expect (list-ref (list 1 2 3) 0) 1)
(check-expect (list-ref (list 1 2 3) 1) 2)

(check-error (list-ref (list 1 2 3)-1))
(check-error (list-ref (list 1 2 3) 3))

(check-expect (first (list 1 2 3)) 1)
(check-expect (rest (list 1 2 3)) 2 3)
(check-expect (first (rest (list 1 2 3))) 2)

; purpose: access the element in the given index in the given list (parallel to list-ref)
; contract: getElement: l(list of any) i(number) -> any
; test
(check-expect (getElement (list 1 2 3) 1) 2)
(check-error (getElement (list 1 2 3) -1))
(check-error (getElement (list 1 2 3) 3))

;function

(define (getElement l i) (cond ((or (< i 0)(not (integer= i))(>= i (length l))) (error "invalid index..."))
                               ((= i 0) (first l)) ;termination
                               (else (getElement (rest l) (- i 1))) ;recursion
                               ))

; purpose: get length of the given list (parallel to length)
; contract: getLength: l(list of any) -> number
; test
(check-expect (getLength (list 1 2 3)) 3)
(check-expect (getLength '()) 0)
;function
(define (getLength l) (cond ((empty= l) 0)
                            (else (+ 1 (getLength (rest l))))
                            ));

; purpose: calculate square of all given numbers
; contract: calcSqr: l(list of numbers) -> list of numbers
; test
(check-expect (calcSqr (list 1 2 3)) (list 1 4 9))
; function
(define (calcSqr l) (cond ((empty? l) '())
                          (else (cons (sqr (first l)) (calcSqr (rest l))))
                          ))

; purpose: search the given number in the given list
; contract: findNumber: l(list of numbers) n(number) -> boolean
; test
(check-expect (findNumber (list 1 2 3) 2) #true)
(check-expect (findNumber (list 1 2 3) 4) #false)
;function
(define (findNumber l n) (cond ((empty? l) #false)
                               ((= n (first l)) #true)
                               (else (findNumber (rest l) n))
                               ))

; purpose: merge two list (parallel to append)
; contract: mergeList: l1(list of any) l2(list of any) -> list of any
; test
(check-expect (mergeList (list 1 2) (list 3 4)) (list 1 2 3 4))
;function
(define (mergeList l1 l2) (cond ((empty? l1) l2)
                                (else (cons (first l1) (mergeList (rest l1) l2)))
                                ))

; purpose: merge two list by combining elements in each index
; contract: combineList: l1(list of any) l2(list of any) -> list of any
; test
(check-expect (combineList (list 1 2 3) (list 4 5)) (list 1 4 2 5 3))
(check-expect (combineList (list 1 2) (list 3 4 5 6)) (list 1 3 2 4 5 6))
; function
(define (combineList l1 l2) (cond ((empty? l1) l2)
                                  ((empty? l2) l1)
                                  (else (mergeList (list (first l1) (first l2)) (combineList (rest l1) (rest l2))))
                                  ))

; structure: stu
; property: name(string)
; property: gpa(number)
(define-struct stu (name gpa))

; purpose: get students whose are more than the given gpa
; contract: getStuMoreGPA: class(list of stu) g(number) -> list of stu
; test
(check-expect (getStuMoreGPA (list (make-stu "a" 1.5) (make-stu "b" 3.2)) 2.5) (list (make-stu "b" 3.2)))
;function
(define (getStuMoreGPA class g) (cond ((empty? class) '())
                                      ((> (stu-gpa (first class)) g) (cons (first class) (getStuMoreGPA (rest class) g)))
                                      (else (getStuMoreGPA (rest class) g))
                                      ))

; purpose: search a number in the given list by using linear search
; contract: searchLinear: l(list of numbers) n(number) -> boolean
; test
(check-expect (searchLinear (list 1 2 3 4 5) 3) #true)
(check-expect (searchLinear (list 1 2 3 4 5) 7) #false)
; function
(define (searchLinear l n) (cond ((empty? l) #false )
                                 ((= (first l) n) #true)
                                 (else (searchLinear (rest l) n))
                                 ))

; purpose: search a number in the given list by using binary search
; contract: searchBinary: l(list of numbers) n(number) start(number) end(number) -> boolean
; test
(check-expect (searchBinary (list 1 2 3 4 5 6) 4 0 5) #true)
(check-expect (searchBinary (list 1 2 3 4 5 6 7) 5 0 6) #true)
(check-expect (searchBinary (list 1 2 3 4 5 6 7) 10 0 6) #false)
; function
(define (searchBinary l n start end) (cond ((>= start (- end 1)) (= (list-ref l start) n)) ; not found, check last element
                                           ((= n (list-ref l (floor (/ (+ start end) 2)))) #true) ; found termination
                                           ((< n (list-ref l (floor (/ (+ start end) 2)))) (searchBinary l n start (floor (/ (+ start end) 2)))) ; go left 
                                           ((> n (list-ref l (floor (/ (+ start end) 2)))) (searchBinary l n (floor (/ (+ start end) 2)) end))   ; go right
                                           ))

; purpose: insert a value to the correct position for an ascending order list
; contract: insert: l(list of numbers) val(number) -> list of numbers
; test
; (check-expect (insert (list 1 2 4 5 6) 3) (list 1 2 3 4 5 6))
; function

(define (insert l val) (cond ((empty? l) (list val))
                             ((< val (first l)) (cons val l)) ; insert it to the list
                             (else (cons (first l) (insert (rest l) val)))  ;do not insert the value, but add first of l not to remove element from the list
                             ))

; purpose: sort a given list to the ascending order by using insertion sort algorithm
; contract: sortInsertion: l(list of numbers) -> list of numbers
; test
(check-expect (sortInsertion (list 5 4 1 6 3 2 7 10 2)) (list 1 2 2 3 4 5 6 7 10))
(check-expect (sortInsertion (list 5 7 8 2 3 1 9)) (list 1 2 3 5 7 8 9))
(check-expect (sortInsertion (list 6 5 4 3 2 1)) (list 1 2 3 4 5 6))
; function
(define (sortInsertion l)(cond ((empty? l) '())
                               (else (insert (sortInsertion (rest l)) (first l)))
                               ))

; ABSTRACT Functions (continued)
; purpose: apply the given function to all elements in the list. parallel to map function
; contract: applyFunc: l(list of any) f(function) -> list of any
; test
(check-expect (applyFunc (list 1 2 3 4 5) sqr) (list 1 4 9 16 25))
(check-expect (applyFunc (list "santral" "bilgi" "university") string-length) (list 7 5 10))
;function
(define (applyFunc l f) (cond ((empty? l) '())
                              (else (cons (f (first l)) (applyFunc (rest l) f)))
                              ))

; MAP function
(check-expect (map sqr (list 1 2 3 4 5)) (list 1 4 9 16 25))
(check-expect (map string-length (list "santral" "bilgi" "university")) (list 7 5 10))
(check-expect (lambda (x) (+ 5 (string-length x))(list "bilgi" "santral" "university")) (list 12 10 15)) ; calculate all of the string length and add 5

; purpose: accumulate the results of the given function on the given list. parallel to foldr/foldl functions
; contract: accumulate: l(list of any) f(function) acc(any) -> any
; test
(check-expect (accumulate (list "Hello" "world") string-append "") "Helloworld")
(check-expect (accumulate (list 1 2 3 4 5) + 0) 15)
; function
(define (accumulate l f acc) (cond ((empty? l) acc)
                                   (else (accumulate (rest l) f (f acc (first l))))
                                   ))

; FOLDR / FOLDL functions
(check-expect (foldr + 0 (list 1 2 3 4 5)) 15)
(check-expect (foldr string-append "" (list "Hello" "world")) "Helloworld")
(chec-expect 



























                               
                                                                                