#lang racket/gui
(require rackunit
         "vectors.rkt"
         "renderer.rkt")

(define zero (vec3 0 0 0))
(define v1 (vec3 1 2 3))
(define v2 (vec3 -3 -2 -1))
(define v3 (vec3 1 -1 1))

;;RAY TESTS
(check-equal? (get-cam-ray zero zero) (ray zero zero) "Zero case ray")
(check-equal? (get-cam-ray v1 v2) (ray (vec3 1 2 3) (vec3 4 4 4)) "Non-zero case ray")


