#lang racket/gui
#|
(scolor r g b)
Structure for representing an RGB color on a 0-1 scale.

Fields:
    r: the red component (0-1)
    g: the green component (0-1)
    b: the blue component (0-1)
|#

(struct scolor (r g b) #:transparent)

#|
Color -> scolor
Scales a conventional color to a 0-1 scale.

Args
    c: the color to be scaled
Returns:
    the scaled color (0-1 scale)
|#

(define (scale-color c) (scolor (/ (send c red) 255) (/ (send c green) 255) (/ (send c blue) 255)))

#|
Color -> scolor
Unscales a scaled color to a 0-255 scale.

Args
    c: the color to be unscaled
Returns:
    the unscaled color (0-255 scale)
|#
(define (unscale-color sc) (make-color (exact-round (* (scolor-r sc) 255)) (exact-round (* (scolor-g sc) 255)) (exact-round (* (scolor-b sc) 255))))

(provide scolor
         scolor-r
         scolor-g
         scolor-b
         scale-color
         unscale-color)