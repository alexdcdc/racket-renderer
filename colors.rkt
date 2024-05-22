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

Args:
    c: the color to be scaled
Returns:
    the scaled color (0-1 scale)
|#

(define (scale-color c)
  (scolor (/ (send c red) 255)
          (/ (send c green) 255)
          (/ (send c blue) 255)))

#|
Color -> scolor
Unscales a scaled color to a 0-255 scale, clamping as needed.

Args:
    sc: the color to be unscaled
Returns:
    the unscaled color (0-255 scale)
|#
(define (unscale-color sc)
  (local [(define c (scolor-clamp sc))]
    (make-color (exact-round (* (scolor-r c) 255))
                (exact-round (* (scolor-g c) 255))
                (exact-round (* (scolor-b c) 255)))))

#|
scolor -> scolor
Clamps a scaled color between 0 and 1 in each component.

Args:
    col: the color to be clamped
Returns:
    the clamped color
|#
(define (scolor-clamp col)
  (local [
          (define (clamp n)
            (cond [(> n 1) 1]
                  [(< n 0) 0]
                  [else n]))
          ]
    (scolor (clamp (scolor-r col)) (clamp (scolor-g col)) (clamp (scolor-b col)))))

#|
(list scolor) -> scolor
Adds scaled colors together (without clamping between 0 and 1).

Args:
    cols: the scaled colors to be added
Returns:
    the sum of all colors in cols, not necessarily bounded between 0 and 1 in each component
|#
(define scolor-add
  (lambda cols 
    (if (empty? cols)
        (scolor 0 0 0)
        (local
          [
           (define f (first cols))
           (define r (apply scolor-add (rest cols)))
           ]
          (scolor (+ (scolor-r f) (scolor-r r)) (+ (scolor-g f) (scolor-g r)) (+ (scolor-b f) (scolor-b r)))))))

#|
(list scolor) -> scolor
Multiplies scaled colors together (without clamping between 0 and 1).
Color multiplication produces a new color with it's r, g, and b values as the product of the other colors' r, g, and b values.

Args:
    cols: the scaled colors to be multiplied
Returns:
    the sum of all colors in cols, not necessarily bounded between 0 and 1 in each component
|#
(define scolor-mult
  (lambda cols 
    (if (empty? cols)
        (scolor 1 1 1)
        (local
          [
           (define f (first cols))
           (define r (apply scolor-mult (rest cols)))
           ]
          (scolor (* (scolor-r f) (scolor-r r)) (* (scolor-g f) (scolor-g r)) (* (scolor-b f) (scolor-b r)))))))

#|
Number scolor -> scolor
Scales a scolor by a specified factor.

Args:
    n: the scalar factor
    col: the scolor being scaled
Returns:
    the scaled color
|#
(define (scolor-scale n col)
  (scolor (* n (scolor-r col)) (* n (scolor-g col)) (* n (scolor-b col))))

#|
(list scolor) -> scolor
Averages a non-empty list of scolors

Args:
    scs: the list of scolors
Returns:
    the average of all scolors in scs
|#

(define (scolor-avg scs n)
  (scolor-scale (/ 1 n) (apply scolor-add scs)))
  

(provide scolor
         scolor-r
         scolor-g
         scolor-b
         scale-color
         unscale-color
         scolor-clamp
         scolor-add
         scolor-mult
         scolor-scale
         scolor-avg)