#lang racket/gui
(require "vectors.rkt"
         "colors.rkt"
         "sphere.rkt")

(define IMAGE_HEIGHT 600)
(define IMAGE_WIDTH 800)
(define IMAGE_PLANE_HEIGHT 1.5)
(define IMAGE_PLANE_WIDTH 2)
(define CAMERA_DEPTH 1)

;; Coordinates of corners of the image plane
(define x1 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT 2) 0))
(define x2 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT 2) 0))
(define x3 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT -2) 0))
(define x4 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT -2) 0))

;;Camera coordinates
(define cam (vec3 0 0 (- CAMERA_DEPTH)))

;;List of shapes to be rendered
(define shapes (list (sphere (vec3 0 0 4) 0.5 (scolor 1 0 0)) (sphere (vec3 -3 -3 6) 1 (scolor 0 1 0)) (sphere (vec3 -2 2 10) 2 (scolor 0 0 1))))

#|
(ray vec3 vec3)
Structure for representing a ray with 3 components.

Fields:
    o: the origin of the ray
    dir: the direction of the ray
|#
(struct ray (o dir) #:transparent)

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
            (filter (lambda (p) (>= (cdr p) 0))
                    (map (lambda (shape) (cons shape (sphere-intersect-ray r shape))) shapes)))]
    (if (empty? valid-intersections)
        false
        (car (argmin cdr valid-intersections)))))

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
  (begin (send dc set-pen col 1 'solid)
         (send dc draw-point x y)))

#|
Integer -> Number
Gets the percentage width of a specified pixel in the image for linear interpolation

Args:
    x: the x coordinate of the pixel
Returns:
    x divided by the image width
|#

(define (get-percent-width x) (/ x IMAGE_WIDTH))

#|
Integer -> Number
Gets the percentage height of a specified pixel in the image for linear interpolation

Args:
    y: the y coordinate of the pixel
Returns:
    y divided by the image height
|#

(define (get-percent-height y) (/ y IMAGE_HEIGHT))

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
   (vec3-bilerp x1 x2 x3 x4 (get-percent-width x) (get-percent-height y))
   c))


(define (color-pixel-by-ray dc x y)
  (local [(define cam-ray (get-cam-ray-to-image x y cam))
          (define intersecting-sphere (find-intersecting-shape cam-ray shapes))
          (define col (if (false? intersecting-sphere) (scolor 0 0 0) (sphere-color intersecting-sphere)))]
    (color-pixel dc x y (unscale-color col))))

(define (color-canvas dc)
  (for ([x IMAGE_WIDTH])
    (for ([y IMAGE_HEIGHT])
      (color-pixel-by-ray dc x y))))

(define frame (new frame%
                   [label "Hello world!"]
                   [width IMAGE_WIDTH]
                   [height IMAGE_HEIGHT]))

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (color-canvas dc))])

(send frame show #t)

(provide ray
         get-cam-ray
         sphere-intersect-ray
         find-intersecting-shape)

