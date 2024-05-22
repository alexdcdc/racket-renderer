#lang racket/gui
#|
(mat Number Number Number Number)
Structure representing a light reflecting material.
Fields:
    amb: the percentage of ambient light the material reflects.
    dif: the percentage of diffuse light the material reflects.
    spec: the percentage of specular light the material reflects.
    shine: the shininess factor of the material.
|#

(struct mat (amb dif spec refl shine) #:transparent)

(provide mat
         mat-amb
         mat-dif
         mat-spec
         mat-refl
         mat-shine)