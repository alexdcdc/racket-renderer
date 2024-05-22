#lang racket/gui
(require "colors.rkt"
         "vectors.rkt")
#|
(light vec3 scolor scolor)
Structure representing a point light source.
Fields:
    pos: the position of the light.
    intensity-dif: the intensity of diffuse light the source emits.
    intensity-spec: the intensity of specular light the source emits.
|#

(struct light (pos intensity-dif intensity-spec))

(provide light
         light-pos
         light-intensity-dif
         light-intensity-spec)