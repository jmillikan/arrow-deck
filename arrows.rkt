#lang racket

(require 2htdp/image)
(require rackunit)

;; Artscow rectangular deck: 750 x 1050

(define TRANS (make-color 255 255 255 0))

(define CARD-WIDTH 750)
(define CARD-HEIGHT 1050)

;; TODO: Identify the real pixel bleed area...
(define BLANK (rectangle (- CARD-WIDTH 59) (- CARD-HEIGHT 50) 'solid TRANS))

;; Suit is a list of (color mode)
;; Where color is one of 'black 'red' blue
;; And mode is one of 'solid 'outline
(define SUITS '((black solid) (red solid) (red outline) (black outline) (blue solid) (blue outline)))

(define (h-space w)
  (rectangle w 1 'solid TRANS))

(define (v-space h)
  (rectangle 1 h 'solid TRANS))

;; Divided bar to indicate suit. 1 section for black, 2 for red, 3 for blue.
(define (suit-bar color)
  (let* ((bar-width (/ CARD-WIDTH 4))
         (break-width (/ CARD-WIDTH 30))
         (bar-height (/ CARD-HEIGHT 10)))
    
    (cond [(equal? color 'blue)
           (let ((section-width (/ (- bar-width (* 2 break-width)) 3)))
             (beside 
              (rectangle section-width bar-height 'solid color)
              (rectangle break-width bar-height 'solid TRANS)
              (rectangle section-width bar-height 'solid color)
              (rectangle break-width bar-height 'solid TRANS)
              (rectangle section-width bar-height 'solid color)))]
          [(equal? color 'red)
           (let ((section-width (/ (- bar-width break-width) 2)))
             (beside 
              (rectangle section-width bar-height 'solid color)
              (rectangle break-width bar-height 'solid TRANS)
              (rectangle section-width bar-height 'solid color)))]
          [else 
           (rectangle bar-width bar-height 'solid color)
           ])))

;; Suit Image -> Image
(define (with-suit suit card)
  (overlay/align     "center"
                     "center"
                     (suit-symbol suit)
                     card))

(define (suit-symbol suit)
  (above/align "center"
               (if (equal? (second suit) 'solid)
                   (circle (/ CARD-WIDTH 8) 'solid (first suit))
                   (circle (/ CARD-WIDTH 8) 'outline (make-pen (first suit) (round (/ CARD-WIDTH 20)) 'solid 'round 'round)))
               (rectangle (/ CARD-WIDTH 4) (/ CARD-HEIGHT 25) 'solid TRANS) ; spacer
               (suit-bar (first suit))
               (rectangle (/ CARD-WIDTH 4) (/ CARD-HEIGHT 30) 'solid TRANS))) ; More space for weight...
                                  
(define (suit-tri suit)
  (above
   (rectangle (/ CARD-WIDTH 6) (/ CARD-WIDTH 30) 'solid TRANS)
   (overlay/align 'center 'center
                  (triangle (/ CARD-WIDTH 6) 'solid (first suit))
                  (triangle (/ CARD-WIDTH 6) 'outline (make-pen (first suit) (round (/ CARD-WIDTH 50)) 'solid 'round 'round)))))


(define (with-top-arrow suit card)
  (overlay/align 'center 'top (scale/xy 1 1.25 (suit-tri suit)) card))

(define (with-bottom-arrow suit card)
  (overlay/align 'center 'bottom (rotate 180 (scale/xy 1 1.25 (suit-tri suit))) card))

(define (with-left-arrow suit card)
  (overlay/align 'left 'center (rotate 90 (scale/xy 1.1 1 (suit-tri suit))) card))

(define (with-right-arrow suit card)
  (overlay/align 'right 'center (rotate 270 (scale/xy 1.1 1 (suit-tri suit))) card))

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

(define (no-arrow suit card) card)

(define (add-arrows suit rank card)
  (cond [(>= rank 8) (with-top-arrow suit (add-arrows suit (- rank 8) card))]
        [(>= rank 4) (with-right-arrow suit (add-arrows suit (- rank 4) card))]
        [(>= rank 2) (with-bottom-arrow suit (add-arrows suit (- rank 2) card))]
        [(>= rank 1) (with-left-arrow suit (add-arrows suit (- rank 1) card))]
        [else card]))

; TODO: Mini-suit in summary
(define (with-corners rank suit card)
  (let ((summary
         (above/align 'center
                      (text/font (number->string rank) 
                           (round (/ CARD-WIDTH 6))
                           (first suit)
                           "Stint Ultra Condensed"
                           'system
                           'normal
                           'bold
                           #f)
                      (scale 0.3 (suit-symbol suit)))))
    (overlay/align 'left 'top
                   summary
                   (overlay/align 'right 'bottom
                                  (rotate 180 summary)
                                  card))))

(define (build-deck low-rank high-rank suits)
   (for/list [(suit (in-list suits))]
     (for/list [(rank (in-range low-rank (add1 high-rank)))]
       (design->card 
        (with-outline 
         (with-corners rank suit
                       (with-suit suit (add-arrows suit rank BLANK))))))))

(define (with-outline design)
  (overlay/align 'center 'center design (rectangle (* CARD-WIDTH 0.65) (* CARD-HEIGHT 0.7) 'outline (make-pen 'gray (round (/ CARD-WIDTH 80)) 'solid 'round 'round))))

(define DECK-SUITS (build-deck 0 15 SUITS))

(define (diagonal-juxtapose i1 i2)
  (overlay/align 'center 'center
                    (beside 
                     (above i1 (v-space (* (image-height i2) 0.8)))
                     (h-space (/ CARD-WIDTH 10))
                     (above (v-space (* (image-height i1) 0.8)) i2))
                    BLANK))

(define (bottom-ace ace-suit second-suit)
    (with-outline  
     (diagonal-juxtapose (scale 0.4 (suit-symbol second-suit)) (suit-symbol ace-suit))))

(define (top-ace ace-suit second-suit)
    (with-outline  
     (diagonal-juxtapose (suit-symbol ace-suit) (scale 0.4 (suit-symbol second-suit)))))

(define DECK-ACES (map design->card (append (map top-ace SUITS (append (cdr SUITS) (cons (car SUITS) '())))
                                              (map bottom-ace (append (cdr SUITS) (cons (car SUITS) '())) SUITS))))


(define MEDIUM 0.3)

(define ARROWS
  (beside/align 'center
   (rotate 90 (suit-tri '(red solid)))
   (h-space (/ CARD-WIDTH 20))
   (above  (scale/xy 1 1.2 (suit-tri '(black solid))) (v-space (/ CARD-HEIGHT 10)))
   (h-space (/ CARD-WIDTH 20))
   (rotate 270 (suit-tri '(blue solid)))))

(define DECK-BACK 
  (design->card
   (overlay/align "left" "middle"
                  (beside 
                   (h-space (/ CARD-WIDTH 5.5))
                  (scale MEDIUM
                         (above 
                         (rotate 0 (suit-symbol '(blue solid)))
                         (v-space (/ CARD-WIDTH 2))
                         (rotate 180 (suit-symbol '(blue outline))))))
                 
                 (overlay/align "right" "middle"
                                (beside 
                                 (scale MEDIUM
                                        (above 
                                         (rotate 0 (suit-symbol '(red solid)))
                                         (v-space (/ CARD-WIDTH 2))
                                         (rotate 180 (suit-symbol '(red outline)))))
                                 (rectangle (/ CARD-WIDTH 5.5) 1 'solid TRANS))
                                
                                (overlay/align "center" "top"
                                               (above
                                                (v-space (/ CARD-WIDTH 3.5))
                                                (scale MEDIUM (suit-symbol '(black solid))))
                                               
                                               (overlay/align "center" "bottom"
                                                              (above
                                                               (rotate 180 (scale MEDIUM (suit-symbol '(black outline))))
                                                               (v-space (/ CARD-WIDTH 1.67)))
                                                              
                                               
                                                              (overlay/align "center" "bottom"
                                                                             (above
                                                                              (rotate 180 (scale MEDIUM ARROWS))
                                                                              (v-space (/ CARD-WIDTH 3.5)))
                                                       
                                                                             (with-outline BLANK))))))))
  
(define (write-deck-files prefix deck)
  (for [(i (in-range 0 (length deck)))]
    (save-image (list-ref deck i) (string-append prefix (number->string (add1 i)) ".png"))))

(define DECK (cons DECK-BACK (flatten DECK-SUITS)))
;(scale 0.2 (list-ref DECK 75))

(define (with-preview-space card)
  (above/align 'center
   (h-space (* CARD-WIDTH 1.1))
   card
   (v-space (* CARD-WIDTH 0.1))))
   
; Preview image of deck and card back
(define DECK-PREVIEW
  (overlay/align 'center 'center
                 (above
                  (scale 0.1 (apply above (map (lambda (suit) 
                                                 (if (> (length suit) 1)
                                                     (apply beside (map with-preview-space suit))
                                                     (first (map with-preview-space suit)))) DECK-SUITS)))
                  (scale 0.13 (apply beside (map with-preview-space DECK-ACES)))
                  (scale 0.2 DECK-BACK))
                 (rectangle 1400 1200 'solid 'gray)))

DECK-PREVIEW


