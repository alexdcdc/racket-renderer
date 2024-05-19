#lang racket/gui
(require rackunit
         "vectors.rkt"
         "renderer.rkt"
         "sphere.rkt"
         "colors.rkt")

(define zero (vec3 0 0 0))
(define v1 (vec3 1 2 3))
(define v2 (vec3 -3 -2 -1))
(define v3 (vec3 1 -1 1))

(define r1 (ray zero (vec3 3 4 0)))
(define r2 (ray (vec3 1 1 1) (vec3 0 0 1)))
(define s1 (sphere (vec3 1 1 5) 2 (scolor 0 0 0)))
(define s2 (sphere (vec3 6 8 0) 5 (scolor 0 0 0)))
(define s3 (sphere (vec3 1 -1 5) 2 (scolor 0 0 0)))
(define s4 (sphere (vec3 1 -2 5) 2 (scolor 0 0 0)))
(define s5 (sphere (vec3 1 1 -4) 2 (scolor 0 0 0)))

;;RAY TESTS
(check-equal? (get-cam-ray zero zero) (ray zero zero) "Zero case ray")
(check-equal? (get-cam-ray v1 v2) (ray (vec3 1 2 3) (vec3 4 4 4)) "Non-zero case ray")

;;SPHERE-RAY INTERSECTION TESTS
(check-equal? (sphere-intersect-ray r2 s1) 2 "Sphere-ray 2 intersections #1")
(check-equal? (sphere-intersect-ray r1 s2) 1 "Sphere-ray 2 intersections #2")
(check-equal? (sphere-intersect-ray r2 s3) 4 "Sphere-ray 1 intersection")
(check-equal? (sphere-intersect-ray r2 s4) -1 "Sphere-ray no intersection")
(check-equal? (sphere-intersect-ray r2 s5) -1 "Sphere-ray interseciton behind ray")

;;FIRST SPHERE INTERSECTION TESTS
(check-equal? (find-intersecting-shape r2 (list s1)) s1 "Ray intersecting one sphere")
(check-equal? (find-intersecting-shape r2 (list s1 s3 s4 s5)) s1 "Ray intersecting multiple spheres")
(check-equal? (find-intersecting-shape r2 empty) false "Empty list of spheres to intersect")
(check-equal? (find-intersecting-shape r2 (list s4 s5)) false "Many spheres; none intersect")



