#lang racket/gui
#|
(sphere vec3 Number scolor)
Structure for representing a vector with 3 components.

Fields:
    x: the first component
    y: the second component
    z: the third component
|#

(struct sphere (center radius color) #:transparent)

#|
(solve-quad Number Number Number)
Finds the smallest positive solution to a quadratic in the form of ax^2 + bx + c = 0 using the quadratic formula.

Args:
    a: the squared coefficient
    b: the linear coefficient
    c: the constant
Returns:
    the smallest positive solution to the quadratic, or -1 if there is no positive solution
|#

(define (solve-quad a b c)
  (local [(define disc (- (* b b) (* 4 a c)))
          (define sol1 (/ (- (- b) (sqrt disc)) (* 2 a)))
          (define sol2 (/ (+ (- b) (sqrt disc)) (* 2 a)))]
    (cond [(and (>= disc 0) (> sol1 0)) sol1]
          [(and (>= disc 0) (> sol2 0)) sol2]
          [else -1])))

(provide sphere
         sphere-center
         sphere-radius
         sphere-color
         solve-quad)
