#lang racket/gui

#|
vec3 vec3 (X X -> Boolean) -> Boolean
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
vec3 (X -> Number) -> Number
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
vec3 (X -> Number) -> Number
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
(vec3 Number Number Number)
Structure for representing a vector with 3 components.

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
vec3 -> String
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
(list vec3) -> vec3
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
vec3 vec3 -> vec3
Subtracts vectors.

Args:
    v1: the vector to be subtracted from
    v2: the vector to be subtracted
Returns:
    v1 - v2
|#
(define (vec3-subtract v1 v2) 
  (vec3 (- (vec3-x v1) (vec3-x v2)) (- (vec3-y v1) (vec3-y v2)) (- (vec3-z v1) (vec3-z v2))))

#|
vec3 vec3 Number -> vec3
Linearly interpolates between two vectors.

Args:
    v1: the start vector
    v2: the end vector
    alpha: the progress between the two vectors (between 0 and 1)
Returns:
    the interpolated vector
|#
(define (vec3-lerp v1 v2 alpha)
  (vec3-add (vec3-scale (- 1 alpha) v1) (vec3-scale alpha v2)))

#|
vec3 vec3 vec3 vec3 Number Number -> vec3
Bilinearly interpolates between four vectors.

Args:
    v1: the start vector
    v2: the vector corresp. to alpha = 1, beta = 0
    v3: the vector corresp. to alpha = 0, beta = 1
    v4: the vector corresp. to alpha = 1, beta = 1
    v
    alpha: the progress between x1/x2 and x3/x4
    beta: the progress between x1/x3 and x2/x4
Returns:
    the interpolated vector
|#
(define (vec3-bilerp v1 v2 v3 v4 alpha beta)
  (vec3-lerp (vec3-lerp v1 v2 alpha) (vec3-lerp v3 v4 alpha) beta))

#|
vec3 vec3 -> Number
Calculates the dot product of two vectors.

Args:
    v1: the first vector being multiplied
    v2: the second vector being multiplied
Returns:
    the dot product of v1 and v2
|#
(define (vec3-dot v1 v2)
  (+ (* (vec3-x v1) (vec3-x v2)) (* (vec3-y v1) (vec3-y v2)) (* (vec3-z v1) (vec3-z v2))))

#|
vec3 -> Number
Calculates the squared norm (magnitude squared) of a vector.

Args:
    v: the vector being considered
Returns:
    the norm of v, squared
|#
(define (vec3-sq-norm v)
  (vec3-dot v v))

#|
vec3 -> Number
Calculates the squared norm (magnitude squared) of a vector.

Args:
    v: the vector being considered
Returns:
    the norm of v, squared
|#
(define (vec3-norm v)
  (sqrt (vec3-sq-norm v)))

#|
vec3 -> vec3
Normalizes a vector (scales it down to magnitude 1)

Args:
    v: the vector being normalized
Returns:
    a vector in the same direction as v with magnitude 1
|#
(define (vec3-normalize v)
  (vec3-scale (/ 1 (vec3-norm v)) v))

#|
vec3 vec3 -> vec3
Gets the unit vector pointing from one vector to another.

Args:
    src: the source vector
    dest: the destination vector
Returns:
    a unit vector pointing from src to dest
|#
(define (vec3-get-dir src dest)
  (vec3-normalize (vec3-subtract dest src)))

#|
(ray vec3 vec3)
Structure for representing a ray with 3 components.

Fields:
    o: the origin of the ray
    dir: the direction of the ray
|#
(struct ray (o dir) #:transparent)

#|
vec3 vec3 -> ray
Produces a ray starting at the source vector and pointing toward the destination vector.

Args:
    src: the source vector
    dest: the destination vector
Outputs:
    A ray from src through dest
|#
(define (get-ray src dest)
  (ray src (vec3-subtract dest src)))

#|
ray Number -> vec3
Evaluates the expression o + t(d) for a ray to get a point on its corresponding line.

Args:
    r: the ray being considered
    t: the parameter of the line expression
Outputs:
   A point on the corresponding line of the ray given by the expression above.
|#
(define (ray-evaluate r t)
  (vec3-add (ray-o r) (vec3-scale t (ray-dir r))))




(provide vec3
         vec3-x
         vec3-y
         vec3-z
         vec3-string
         vec3-scale
         vec3-add
         vec3-subtract
         vec3-lerp
         vec3-bilerp
         vec3-dot
         vec3-sq-norm
         vec3-norm
         vec3-normalize
         vec3-get-dir
         ray
         ray-o
         ray-dir
         get-ray
         ray-evaluate)
