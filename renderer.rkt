#lang racket/gui
(require "vectors.rkt"
         "colors.rkt"
         "sphere.rkt"
         "materials.rkt"
         "lights.rkt")

(define IMAGE_HEIGHT 600)
(define IMAGE_WIDTH 800)
(define IMAGE_PLANE_HEIGHT 1.72)
(define IMAGE_PLANE_WIDTH 2.56)
(define IMAGE_PLANE_DEPTH 0.5)
(define CAMERA_DEPTH -2)

(define REFLECTION_DEPTH 3)

;; Coordinates of corners of the image plane
(define x1 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT 2) (- IMAGE_PLANE_DEPTH)))
(define x2 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT 2) (- IMAGE_PLANE_DEPTH)))
(define x3 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT -2) (- IMAGE_PLANE_DEPTH)))
(define x4 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT -2) (- IMAGE_PLANE_DEPTH)))

;;Camera coordinates
(define cam (vec3 0 0 (- CAMERA_DEPTH)))

;;List of shapes to be rendered
(define shapes (list
                (sphere (vec3 -1.1 0.6 -1) 0.2
                        (mat (scolor 0.1 0.1 0.1) (scolor 0.5 0.5 0.9) (scolor 0.7 0.7 0.7) (scolor 0.1 0.1 0.2) 20))
                (sphere (vec3 0.2 -0.1 -1) 0.5
                        (mat (scolor 0.1 0.1 0.1) (scolor 0.9 0.5 0.5) (scolor 0.7 0.7 0.7) (scolor 0.2 0.1 0.1) 20))
                (sphere (vec3 1.2 -0.5 -1.75) 0.4
                        (mat (scolor 0.1 0.1 0.1) (scolor 0.1 0.5 0.1) (scolor 0.7 0.7 0.7) (scolor 0.8 0.9 0.8) 20))))

(define ambient-light (scolor 0.5 0.5 0.5))
(define lights (list (light (vec3 -3 -0.5 1) (scolor 0.8 0.3 0.3) (scolor 0.8 0.8 0.8))
                     (light (vec3 3 2 1) (scolor 0.4 0.4 0.9) (scolor 0.8 0.8 0.8))))

#|
vec3 -> ray
Gets a ray through a point on the image plane.

Args:
    v: a vec3 representing a point on the image plane
    c: a vec3 representing the location of the camera
Returns:
    The ray from the camera through v, starting at v
|#
(define (get-cam-ray v c)
  (ray v (vec3-subtract v c)))

#|
dc Integer Integer Color -> void
Colors a pixel at a specified position in the given drawing context.

Args:
    dc: the drawing context
    x: the x position of the pixel
    y: the y position of the pixel
    col: the color of the pixel
|#
(define (color-pixel dc x y col)
  (begin (send dc set-pen (unscale-color col) 1 'solid)
         (send dc draw-point x y)))

#|
Integer Integer vec3 -> ray
Gets the camera ray pointing toward a specified pixel on the image.

Args:
    x: the x coordinate of the pixel
    y: the y coordinate of the pixel
    c: the coordinates of the camera
Returns:
    a ray originating from the image plane pointing at the pixel
|#
(define (get-cam-ray-to-image x y c)
  (get-cam-ray
   (vec3-bilerp x1 x2 x3 x4 (/ x IMAGE_WIDTH) (/ y IMAGE_HEIGHT))
   c))

#|
sphere vec3 (list light) -> scolor
Calculates the color of a sphere at a specified point factoring in lighting.

Args:
    s: the sphere being considered
    p: the point on the sphere being considered
    lights: a list of all lights in the scene
Returns:
    the color of the lit sphere
|#
(define (get-lit-color s p lights r depth)
  (local [
          (define normal (sphere-surface-normal s p))
          (define mat (sphere-mat s))
          (define view (vec3-normalize (vec3-scale -1 (ray-dir r))))
          
          (define (get-ambient-light)
            (scolor-mult (mat-amb mat) ambient-light))
          
          (define (get-diffuse-light lights)
            (if (empty? lights)
                (scolor 0 0 0)
                (local [
                        (define l (first lights))
                        (define l-vec (vec3-get-dir p (light-pos l)))
                        (define l-n-dot (vec3-dot normal l-vec))
                        ]
                  (if (or (< l-n-dot 0) (obstacle-exists? p (light-pos l) shapes))
                      (get-diffuse-light (rest lights))
                      (scolor-add (scolor-scale l-n-dot
                                                (scolor-mult (mat-dif mat) (light-intensity-dif l)))
                                  (get-diffuse-light (rest lights)))))))
          
          (define (get-specular-light lights)
            (if (empty? lights)
                (scolor 0 0 0)
                (local [
                        (define l (first lights))
                        (define l-vec (vec3-get-dir p (light-pos l)))
                        (define l-n-dot (vec3-dot normal l-vec))
                        (define refl (vec3-subtract (vec3-scale (* 2 (vec3-dot normal l-vec)) normal) l-vec))
                        (define v-r-dot (vec3-dot view refl))
                        ]
                  (if (or (< l-n-dot 0) (< v-r-dot 0) (obstacle-exists? p (light-pos l) shapes))
                      (get-specular-light (rest lights))
                      (scolor-add (scolor-scale (expt v-r-dot (mat-shine mat))
                                                (scolor-mult (mat-spec mat) (light-intensity-spec l)))
                                  (get-specular-light (rest lights)))))))
          
          (define (get-refl-light)
            (local [
                    (define refl (vec3-subtract (vec3-scale (* 2 (vec3-dot normal view)) normal) view))
                    ]
              (scolor-mult (mat-refl mat) (color-pixel-by-ray (ray p refl) (- depth 1)))))
          ]
    (scolor-clamp (scolor-add
                   (get-ambient-light)
                   (get-diffuse-light lights)
                   (get-specular-light lights)
                   (get-refl-light)
                   ))))


#|
dc Integer Integer -> scolor
Finds the appropriate color for a pixel by casting a ray out.

Args:
    r: the ray being cast
|#
(define (color-pixel-by-ray r depth)
  (if (<= depth 0)
      (scolor 0 0 0)
      (local [
              (define intersecting-sphere (find-intersecting-shape r shapes))
              (define col (if (false? intersecting-sphere)
                              (scolor 0 0 0)
                              (get-lit-color intersecting-sphere
                                             (sphere-intersection-point r intersecting-sphere)
                                             lights
                                             r
                                             depth)))
              ]
        col)))

#|
dc -> void
Colors the entire drawing context according to the rendered image.

Args:
    dc: the drawing context
|#
(define (color-canvas dc)
  (for ([x IMAGE_WIDTH])
    (for ([y IMAGE_HEIGHT])
      (color-pixel dc x y (color-pixel-by-ray (get-cam-ray-to-image x y cam) REFLECTION_DEPTH)))))

(define frame (new frame%
                   [label "Hello world!"]
                   [width IMAGE_WIDTH]
                   [height IMAGE_HEIGHT]))

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (color-canvas dc))])

(define (render) (send frame show #t))

(provide ray
         get-cam-ray)

