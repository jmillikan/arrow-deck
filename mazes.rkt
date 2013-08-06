#lang racket

(require 2htdp/image)
(require rackunit)

;; Artscow rectangular deck: 700 x 1050

(define CARD-WIDTH 750)
(define CARD-HEIGHT 1050)
(define BLANK (rectangle (- CARD-WIDTH 80) (- CARD-HEIGHT 80) 'solid 'white))

;; Suit is a list of (color mode)
;; Where color is one of 'black 'red' blue
;; And mode is one of 'solid 'outline
(define suits '((black solid) (black outline) (red solid) (red outline) (blue solid) (blue outline)))

;; Suit Image -> Image
(define (with-suit suit card)
  (overlay/align     "center"
                     "center"
                     (if (equal? (second suit) 'solid)
                         (circle (/ CARD-WIDTH 6) 'solid (first suit))
                         (circle (/ CARD-WIDTH 6) 'outline (make-pen (first suit) (round (/ CARD-WIDTH 20)) 'solid 'round 'round)))
                     card))

(check-equal? (with-suit '(black solid) BLANK)
              (overlay/align "center" "center" 
                             (circle (/ CARD-WIDTH 6) 'solid 'black) BLANK))

(define (suit-tri suit)
  (overlay/align 'center 'center
                 (triangle (/ CARD-WIDTH 6) 'solid (first suit))
                 (triangle (/ CARD-WIDTH 6) 'outline (make-pen (first suit) (round (/ CARD-WIDTH 50)) 'solid 'round 'round)))) 

(define (with-top-arrow suit card)
  (overlay/align 'center 'top (scale/xy 1 1.2 (suit-tri suit)) card))

(define (with-bottom-arrow suit card)
  (overlay/align 'center 'bottom (rotate 180 (scale/xy 1 1.2 (suit-tri suit))) card))

(define (with-left-arrow suit card)
  (overlay/align 'left 'center (rotate 90 (suit-tri suit)) card))

(define (with-right-arrow suit card)
  (overlay/align 'right 'center (rotate 270 (suit-tri suit)) card))

(define eight-black-dot
  (with-suit '(black solid) 
             (with-top-arrow '(black solid)
                             BLANK)))

(define (design->card d)
  (overlay/align "center" "center" d (rectangle CARD-WIDTH CARD-HEIGHT 'solid 'white)))

(define (display-card c)
  (overlay/align "center" "center" c (rectangle 2000 2000 'solid 'grey)))

(define BC '(blue outline))
(define fifteen-blue-circle
  (with-suit BC
             (with-top-arrow BC
                             (with-bottom-arrow BC
                                                (with-right-arrow BC
                                                                  (with-left-arrow BC BLANK))))))
                                                                                   