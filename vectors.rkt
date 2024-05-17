#lang racket

#|
Checks if two vectors are equal (their corresponding components are equal)

Args:
    v1: the first vector being compared
    v2: the second vector being compared
Returns:
    #true if the two vectors have equal components, #false otherwise
|#
(define (vec3=? v1 v2 recursive-equal?)
  (and (= (vec3-x v1) (vec3-x v2))
       (= (vec3-y v1) (vec3-y v2))
       (= (vec3-z v1) (vec3-z v2))))

#|
Computes the primary hash code of a vector.

Args:
    v: the vector to be hashed
Returns:
    the primary hash code of v
|#
(define (vec3-hash-code v recursive-equal-hash)
  (+ (* 10000 (vec3-x v))
     (* 100 (vec3-y v))
     (* 1 (vec3-z v))))

#|
Computes the secondary hash code of a vector.

Args:
    v: the vector to be hashed
Returns:
    the secondary hash code of v
|#
(define (vec3-secondary-hash-code v recursive-equal-hash)
  (+ (* 10000 (vec3-z v))
     (* 100 (vec3-x v))
     (* 1 (vec3-y v))))

#|
Structure for representing a vector with 3 components

Fields:
    x: the first component
    y: the second component
    z: the third component
|#
(struct vec3 (x y z)
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc vec3=?)
   (define hash-proc  vec3-hash-code)
   (define hash2-proc vec3-secondary-hash-code)]
  )

#|
Converts a vector to a string representation.

Args:
    vector: the vector to be converted
Returns:
    the vector's components in order, space-separated
|#
(define (vec3-string vector)
  (~a (vec3-x vector) " " (vec3-y vector) " " (vec3-z vector)))

#|
Scales a vector by a scalar value.

Args:
    vector: the vector to be scaled
    scalar: the scalar to scale by
Returns:
    the scaled vector
|#
(define (vec3-scale scalar vector)
  (vec3 (* (vec3-x vector) scalar) (* (vec3-y vector) scalar) (* (vec3-z vector) scalar)))

#|
Adds vectors together.

Args:
    vectors: the vectors to be added
Returns:
    the sum of all vectors
|#
(define vec3-add
  (lambda vectors 
    (if (empty? vectors)
        (vec3 0 0 0)
        (local
          [
           (define f (first vectors))
           (define r (apply vec3-add (rest vectors)))
           ]
          (vec3 (+ (vec3-x f) (vec3-x r)) (+ (vec3-y f) (vec3-y r)) (+ (vec3-z f) (vec3-z r)))))))

#|
Linearly interpolates between two vectors.

Args:
    v1: the start vector
    v2: the end vector
    alpha: the progress between the two vectors (between 0 and 1)
returns:
    the interpolated vector
|#
(define (vec3-lerp v1 v2 alpha)
  (vec3-add (vec3-scale (- 1 alpha) v1) (vec3-scale alpha v2)))

#|
Bilinearly interpolates between four vectors

Args:
    v1: the start vector
    v2: the vector corresp. to alpha = 1, beta = 0
    v3: the vector corresp. to alpha = 0, beta = 1
    v4: the vector corresp. to alpha = 1, beta = 1
    v
    alpha: the progress between x1/x2 and x3/x4
    beta: the progress between x1/x3 and x2/x4
returns:
    the interpolated vector
|#
(define (vec3-bilerp v1 v2 v3 v4 alpha beta)
  (vec3-lerp (vec3-lerp v1 v2 alpha) (vec3-lerp v3 v4 alpha) beta))


(provide (all-defined-out))
