#lang racket/gui
(require rackunit
         "vectors.rkt")

(define zero (vec3 0 0 0))
(define v1 (vec3 1 2 3))
(define v2 (vec3 -3 -2 -1))
(define v3 (vec3 1 -1 1))

(define corner1 zero)
(define corner2 (vec3 1 1 0))
(define corner3 (vec3 0 0 1))
(define corner4 (vec3 1 1 1))

;; STRING CONVERSION TESTS
(check-equal? (vec3-string zero) "0 0 0" "Basic string conversion")

;; VECTOR ADDITION TESTS
(check-equal? (vec3-add) zero "Addition base case")
(check-equal? (vec3-add v1) v1 "Addition with one vector")
(check-equal? (vec3-add v1 v2) (vec3 -2 0 2) "Addition with two vectors")
(check-equal? (vec3-add v1 v2 v3) (vec3 -1 -1 3) "Addition with more than two vectors")

;; VECTOR SCALING TESTS
(check-equal? (vec3-scale 10 zero) zero "Scaling zero vector")
(check-equal? (vec3-scale 0 v1) zero "Scaling by a factor of 0")
(check-equal? (vec3-scale 1 v1) v1 "Scaling by a factor of 1")
(check-equal? (vec3-scale 3 v1) (vec3 3 6 9) "Scaling by a nontrivial factor")

;; LERP TESTS
(check-equal? (vec3-lerp v1 v3 0) v1 "Lerp with alpha = 0")
(check-equal? (vec3-lerp v1 v3 1) v3 "Lerp with alpha = 1")
(check-equal? (vec3-lerp v1 v3 0.5) (vec3 1 0.5 2) "Lerp with alpha between 0 and 1")

;; BILERP TESTS
(check-equal? (vec3-bilerp corner1 corner2 corner3 corner4 0 0) corner1 "Bilerp with alpha, beta = 0")
(check-equal? (vec3-bilerp corner1 corner2 corner3 corner4 1 1) corner4 "Bilerp with alpha, beta = 1")
(check-equal? (vec3-bilerp corner1 corner2 corner3 corner4 0.5 0.5) (vec3 0.5 0.5 0.5) "Bilerp with alpha, beta between 0 and 1")

;; DOT PRODUCT TESTS
(check-equal? (vec3-dot v1 zero) 0 "Dot product with zero vector")
(check-equal? (vec3-dot v1 v2) -10 "Dot product with two nonzero vectors")

;; NORM TESTS
(check-equal? (vec3-sq-norm v1) 14 "Squared norm")
(check-equal? (vec3-norm v1) (sqrt 14) "Unsquared norm")

;; NORMALIZATION TESTS
(check-equal? (vec3-normalize (vec3 1 0 0)) (vec3 1 0 0) "Normalizing unit-length vector")
(check-equal? (vec3-normalize (vec3 1 2 2)) (vec3 1/3 2/3 2/3) "Normalizing non-unit-length vector")

;; DIRECTION TESTS
(check-equal? (vec3-get-dir (vec3 0 0 0) (vec3 0 0 3)) (vec3 0 0 1) "Getting direction, z-component only")
(check-equal? (vec3-get-dir (vec3 3 4 1) (vec3 12 16 1)) (vec3 3/5 4/5 0) "Getting direction, x and y-component only")

;; GET RAY TESTS
(check-equal? (get-ray (vec3 0 0 0) (vec3 0 0 0)) (ray (vec3 0 0 0) (vec3 0 0 0)) "Ray from origin to origin") 
(check-equal? (get-ray (vec3 1 2 3) (vec3 4 7 5)) (ray (vec3 1 2 3) (vec3 3 5 2)) "Ray between two distinct non-zero points")

;; RAY EVALUATION TESTS
(check-equal? (ray-evaluate (ray zero v1) 0) zero "Ray evaluation with origin zero, t = 0")
(check-equal? (ray-evaluate (ray zero v1) 3) (vec3 3 6 9) "Ray evaluation with origin zero, t > 0")
(check-equal? (ray-evaluate (ray v1 v3) 2) (vec3 3 0 5) "Ray evaluation with non-zero origin, t > 0")
