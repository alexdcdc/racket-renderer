#lang racket/gui
(require "colors.rkt"
         rackunit)

;;COLOR SCALING TESTS
(check-equal? (scale-color (make-color 255 255 255)) (scolor 1 1 1) "Color scaling white")
(check-equal? (scale-color (make-color 0 0 0 )) (scolor 0 0 0) "Color unscaling black")


