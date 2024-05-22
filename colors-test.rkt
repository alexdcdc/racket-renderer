#lang racket/gui
(require "colors.rkt"
         rackunit)

;;COLOR SCALING TESTS
(check-equal? (scale-color (make-color 255 255 255)) (scolor 1 1 1) "Color scaling white")
(check-equal? (scale-color (make-color 0 0 0)) (scolor 0 0 0) "Color unscaling black")
(check-equal? (scale-color (make-color 5 5 5)) (scolor 5/255 5/255 5/255) "Color unscaling other color")

;;COLOR UNSCALING TESTS
(check-equal? (unscale-color (scolor 1 1 1)) (make-color 255 255 255) "Color unscaling white")
(check-equal? (unscale-color (scolor 0 0 0)) (make-color 0 0 0) "Color unscaling black")
(check-equal? (unscale-color (scolor 1/2 1/2 1/2)) (make-color 128 128 128) "Color unscaling other color")

;;COLOR CLAMPING TESTS
(check-equal? (scolor-clamp (scolor 0.5 0.2 0.3)) (scolor 0.5 0.2 0.3) "No clamping needed")
(check-equal? (scolor-clamp (scolor -0.2 0.3 -0.4)) (scolor 0 0.3 0) "Clamping some components up to 0")
(check-equal? (scolor-clamp (scolor 1.3 1.4 0.4)) (scolor 1 1 0.4))

;;COLOR ADDITION TESTS
(check-equal? (scolor-add (scolor 0 0 0) (scolor 0.5 0.5 0.5)) (scolor 0.5 0.5 0.5) "Adding a color with black")
(check-equal? (scolor-add (scolor 0.1 0.1 0.1) (scolor 0.2 0.3 0.4) (scolor 0.3 0.2 0.1)) (scolor 0.6 0.6 0.6) "Adding 3 colors together (no clamping)") ;;this is not a valid color, but still the intended functionality

;;COLOR MULTIPLICATION TESTS
(check-equal? (scolor-mult (scolor 1/2 1/3 1/4) (scolor 1/4 1/3 1/4) (scolor 1 1/3 1/5)) (scolor 1/8 1/27 1/80) "Multiplying 3 colors together (no clamping)")
(check-equal? (scolor-mult (scolor 0 0 0) (scolor 1/4 1/3 1/4)) (scolor 0 0 0) "Multiplying a color with 0")

;;COLOR SCALING TESTS
(check-equal? (scolor-scale 3 (scolor 0 0 0)) (scolor 0 0 0) "Scaling 0 color")
(check-equal? (scolor-scale 0 (scolor 1 2 3)) (scolor 0 0 0) "Scaling color by 0")
(check-equal? (scolor-scale 4 (scolor 1/3 1/4 1/5)) (scolor 4/3 1 4/5) "Scaling non-zero color by non-zero factor")




