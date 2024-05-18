#lang racket/gui
(require "vectors.rkt")

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






(define frame (new frame%
                   [label "Hello world!"]
                   [width IMAGE_WIDTH]
                   [height IMAGE_HEIGHT]))



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
  (local [(define cam-ray-dir (ray-dir (get-cam-ray-to-image x y cam)))]
    (color-pixel dc x y (make-color (exact-floor (* 255 (+ 1/2 (/ (vec3-x cam-ray-dir) IMAGE_PLANE_WIDTH))))
                                    (exact-floor (* 255 (+ 1/2 (/ (vec3-y cam-ray-dir) IMAGE_PLANE_HEIGHT))))
                                    127))))

(define (color-canvas dc)
  (for ([x IMAGE_WIDTH])
    (for ([y IMAGE_HEIGHT])
      (color-pixel-by-ray dc x y))))

 
(send frame show #t)

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (color-canvas dc))])

(provide ray
         get-cam-ray)

