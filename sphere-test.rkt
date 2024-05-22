#lang racket/gui
(require "sphere.rkt"
         "vectors.rkt"
         "colors.rkt"
         "materials.rkt"
         rackunit)

(define zero (vec3 0 0 0))
(define r1 (ray zero (vec3 3 4 0)))
(define r2 (ray (vec3 1 1 1) (vec3 0 0 1)))

(define p-mat (mat (scolor 0.8 0.8 0.8) (scolor 0.6 0.6 0.6) (scolor 1 1 1) (scolor 0.2 0.2 0.2) 5))
(define s1 (sphere (vec3 1 1 5) 2 p-mat))
(define s2 (sphere (vec3 6 8 0) 5 p-mat))
(define s3 (sphere (vec3 1 -1 5) 2 p-mat))
(define s4 (sphere (vec3 1 -2 5) 2 p-mat))
(define s5 (sphere (vec3 1 1 -4) 2 p-mat))
(define s6 (sphere (vec3 5 0 0) 3 p-mat))

;;QUADRATIC FORMULA TESTS
(check-equal? (solve-quad 1 -2 1) 1 "One positive solution")
(check-equal? (solve-quad 1 4 4) -1 "One negative solution")
(check-equal? (solve-quad 1 -10 21) 3 "Two positive solutions")
(check-equal? (solve-quad 1 10 21) -1 "Two negative solutions")
(check-equal? (solve-quad 1 -2 -3) 3 "One positive, one negative solution")
(check-equal? (solve-quad 1 1 1) -1 "No solutions")

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

;;OBSTACLE DETECTION TESTS
(define shapes (list s6))
(check-equal? (obstacle-exists? (vec3 0 0 0) (vec3 1 0 0) shapes) #false "Obstacle exists but beyond second point")
(check-equal? (obstacle-exists? (vec3 0 0 0) (vec3 0 1 0) shapes) #false "Obstacle not in path")
(check-equal? (obstacle-exists? (vec3 0 0 0) (vec3 10 0 0) shapes) #true "Obstacle in path")

;;SPHERE INTERSECTION POINT TESTS
(check-equal? (sphere-intersection-point r2 s1) (vec3 1 1 3) "Sphere-ray 2 intersections #1 (point)")
(check-equal? (sphere-intersection-point r1 s2) (vec3 3 4 0) "Sphere-ray 2 intersections #2 (point)")
(check-equal? (sphere-intersection-point r2 s3) (vec3 1 1 5) "Sphere-ray 1 intersection (point)")

;;SURFACE NORMAL TESTS
(check-equal? (sphere-surface-normal s1 (vec3 1 1 7)) (vec3 0 0 1) "Surface normal with z component")
(check-equal? (sphere-surface-normal s2 (vec3 9 12 0)) (vec3 3/5 4/5 0) "Surface normal with x, y component")