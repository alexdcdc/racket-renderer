#lang racket/gui
(require "vectors.rkt"
         "colors.rkt"
         "materials.rkt")
(define TOLERANCE 1E-6)

#|
(sphere vec3 Number scolor)
Structure for representing a colored sphere in space.

Fields:
    center: the center of the sphere
    radius: the radius of the sphere
    color: the color of the sphere
    mat: the material the sphere is made of
|#

(struct sphere (center radius mat) #:transparent)

#|
Number Number Number -> Number
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

#|
(sphere-intersect-ray sphere ray)
Finds the value t such that o * t(dir) (representing a ray) intersects a sphere.

Args:
    sphere: the sphere involved in the intersection
    ray: the ray involved in the intersection
Returns:
    the solution t, or -1 if there is no positive solution
|#

(define (sphere-intersect-ray r s)
  (local [(define sphere-to-ray (vec3-subtract (ray-o r) (sphere-center s)))
          (define a (vec3-sq-norm (ray-dir r)))
          (define b (* 2 (vec3-dot sphere-to-ray (ray-dir r))))
          (define c (- (vec3-sq-norm sphere-to-ray) (* (sphere-radius s) (sphere-radius s))))]
    (solve-quad a b c)))

#|
ray (list sphere) -> sphere or false
Finds the sphere (if any) that a ray first intersects.

Args:
    r: the ray being considered
    shapes: the list of spheres that could be intersected
Returns:
    the first sphere that the ray intersects, or false if the ray does not intersect any spheres.
|#
(define (find-intersecting-shape r shapes)
  (local [(define valid-intersections
            (filter (lambda (p) (> (cdr p) TOLERANCE))
                    (map (lambda (shape) (cons shape (sphere-intersect-ray r shape))) shapes)))] ;;finds all shapes that the ray actually intersects
    (if (empty? valid-intersections)
        false
        (car (argmin cdr valid-intersections))))) ;;finds shape with minimum t value (closest one)

#|
sphere vec3 light -> boolean
Determines whether an obstacle shape exists between two points.

Args:
    p1: the first point
    p2: the second point
    shapes: the list of all possible shapes
Returns:
    #true if the an obstacle exists, #false otherwise
|#
(define (obstacle-exists? p1 p2 shapes)
  (local [(define r (get-ray p1 p2))]
    (ormap (lambda (s)
             (local [(define t (sphere-intersect-ray r s))]
               (and (> t TOLERANCE) (< t 1))))
           shapes)))

#|
sphere ray -> vec3
Finds the (nearest) intersection point where a ray and sphere intersect.

Args:
    sphere: the sphere involved in the intersection
    ray: the ray involved in the intersection
Returns:
    the point on the sphere where the ray first intersects it
|#
(define (sphere-intersection-point r s)
  (ray-evaluate r (sphere-intersect-ray r s)))

#|
sphere vec3 -> vec3
Gets the surface normal of a sphere at a specified point (assumed to be on the sphere).

Args:
    s: the sphere
    pt: a point on the sphere
Returns:
    the unit surface normal of s at pt
|#
(define (sphere-surface-normal s pt)
  (vec3-get-dir (sphere-center s) pt))

(provide sphere
         sphere-center
         sphere-radius
         sphere-mat
         solve-quad
         sphere-intersect-ray
         find-intersecting-shape
         obstacle-exists?
         sphere-intersection-point
         sphere-surface-normal)
