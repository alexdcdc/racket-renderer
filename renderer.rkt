#lang racket
(require "vectors.rkt")

(define IMAGE_HEIGHT 600)
(define IMAGE_WIDTH 800)
(define IMAGE_PLANE_HEIGHT 1.5)
(define IMAGE_PLANE_WIDTH 2)
(define CAMERA_DEPTH 1)

;; Coordinates of corners of the image plane
(define x1 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT 2) 0))
(define x2 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT 2) 0))
(define x3 (vec3 (/ IMAGE_PLANE_WIDTH 2) (/ IMAGE_PLANE_HEIGHT -2) 0))
(define x4 (vec3 (/ IMAGE_PLANE_WIDTH -2) (/ IMAGE_PLANE_HEIGHT -2) 0))

;;Camera coordinates
(define cam (vec3 (- CAMERA_DEPTH) 0 0))





