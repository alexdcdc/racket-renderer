#lang racket/gui
(require "sphere.rkt"
         rackunit)

;;QUADRATIC FORMULA TESTS
(check-equal? (solve-quad 1 -2 1) 1 "One positive solution")
(check-equal? (solve-quad 1 4 4) -1 "One negative solution")
(check-equal? (solve-quad 1 -10 21) 3 "Two positive solutions")
(check-equal? (solve-quad 1 10 21) -1 "Two negative solutions")
(check-equal? (solve-quad 1 -2 -3) 3 "One positive, one negative solution")
(check-equal? (solve-quad 1 1 1) -1 "No solutions")

;;SPHERE-RAY INTERSECTION TESTS