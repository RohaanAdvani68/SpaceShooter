;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Final Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Call (game) to start the game
;; In this game a stationary player has to shoot a stationary target but there will be a horizontal wind that the player must account for, which changes randomly every time the target is hit (based on Paper Toss, the iPhone game).
;; Use left and right arrow keys to rotate player and press <space> to shoot missiles.


(require 2htdp/image)
(require 2htdp/universe)
(require "struct-inheritance.rkt")


;; Called when the left arrow key is pressed
(define (on-left-press)
  (set-game-object-rotational-velocity! the-player (- 2)))

;; Called when the left arrow key is released
(define (on-left-release)
  (set-game-object-rotational-velocity! the-player 0))

;; Called when the right arrow key is pressed
(define (on-right-press)
  (set-game-object-rotational-velocity! the-player 2))

;; Called when the right arrow key is released
(define (on-right-release)
  (set-game-object-rotational-velocity! the-player 0)) 

;; Called when the space bar is pressed.
;; There's no action to take when the space bar is released.
(define (on-space-press)
  (fire-missile!))


;;;
;;; Turnable constants
;;;

(define window-width 800)
(define window-height 600)
(define frame-rate 30)
(define inter-frame-interval (/ 1.0 frame-rate))
(define score 0)
(define wind-velocity (make-posn 0 0))

;;;
;;; Tracking game objects
;;;

(define all-game-objects '())

  (define (destroy! object)
  (set! all-game-objects
        (remove object all-game-objects)))

(define the-player '())

;;;
;;; Type definitions
;;;

(define-struct game-object
  ([position #:mutable]
   [velocity #:mutable]
   [orientation #:mutable]
   [rotational-velocity #:mutable]
   radius)
  )

(define-struct (player game-object) ()
  #:methods
  (define (render object)
    (isosceles-triangle 60 80 "solid" "aquamarine")
    )
  (define (update! object)
    (void)))

(define-struct (missile game-object)
  ([lifetime #:mutable])
  #:methods
  (define (update! m)
    (begin
      (if (= 0 (missile-lifetime m)) 
          (destroy! m)
          (set-missile-lifetime! m (- (missile-lifetime m) 1)))
      (set-game-object-velocity! m (posn-+ wind-velocity (game-object-velocity m)))))
  (define (render object)
    (circle 2 "solid" "black"))
  )

(define-struct (string game-object)
  ([words #:mutable] size color)
  #:methods
  (define (render object)
    (text (string-words object)
          (string-size object)
          (string-color object))))

(define-struct (disp-wind string) ()
  #:methods
  (define (update! object)
    (set-string-words! object (string-append "WIND: " (number->string (round (* 10 (posn-x wind-velocity))))))))

(define-struct (disp-score string) ()
  #:methods
  (define (update! object)
    (set-string-words! object (string-append "SCORE: " (number->string score)))))
 
(define-struct (target game-object)()
  #:methods
  (define (render object)
    (circle 25 "solid" (random-color)))
  (define (update! object)
    (void)))
  


;;;
;;; Object creation
;;;

(define (new-target)
  (make-target (make-posn (/ window-width 2) 30)
               (make-posn 0 0)
               0
               0
               25))

(define (make-score)
  (set! all-game-objects (cons
                          (make-disp-score
                           (make-posn 70 30)
                           (make-posn 0 0)
                           4.7
                           0
                           30
                           (string-append "SCORE: " (number->string score))
                           30
                           "purple")
                          all-game-objects)))

(define (make-wind)
  (set! all-game-objects (cons
                          (make-disp-wind
                           (make-posn 70 70)
                           (make-posn 0 0)
                           4.7
                           0
                           30
                           (string-append "WIND: " (number->string (round (* 10 (posn-x wind-velocity)))))
                           20
                           "purple")
                          all-game-objects)))

(define (fire-missile!)
  (local [(define forward (forward-direction the-player))]
    (set! all-game-objects
          (cons (make-missile (posn-+ (game-object-position the-player)
                                      (posn-* (+ (game-object-radius the-player) 5)
                                              forward))
                                      (posn-* 180 forward)
                              0
                              0
                              2
                              100)
                all-game-objects))))

;;;
;;; Driver loop
;;;

(define (game)
  (begin (set! the-player
               (make-player (make-posn (/ window-width 2)
                                       (- window-height 45))
                            (make-posn 0 0)
                            4.7
                            0
                            40))
         (set! all-game-objects
               (cons the-player
                     (cons (new-target)
                           all-game-objects)))
         (make-score)
         (make-wind)
         (big-bang all-game-objects
                   (on-key (λ (ignore key)
                             (begin (on-key-press key)
                                    all-game-objects)))
                   (on-release (λ (ignore key)
                                 (begin (on-key-release key)
                                        all-game-objects)))
                   (on-tick (lambda (game-objects)
                              (begin (for-each update! game-objects)
                                     (update-physics!)
                                     all-game-objects))
                            inter-frame-interval)
                   (to-draw (lambda (game-objects)
                              (foldl (lambda (object scene)
                                       (place-image (rotate (radians->rotation (game-object-orientation object))
                                                            (render object))
                                                    (posn-x (game-object-position object))
                                                    (posn-y (game-object-position object))
                                                    scene))
                                     (rectangle window-width window-height "solid" "white")
                                     game-objects))
                            800
                            600))))

  ;;;
;;; Event dispatch
;;;

(define (on-key-press key)
  (cond [(equal? key "left")
         (on-left-press)] 
        [(equal? key "right")
         (on-right-press)]
        [(equal? key " ")
         (on-space-press)]
        [else null]))

(define (on-key-release key)
  (cond [(equal? key "left")
         (on-left-release)]
        [(equal? key "right")
         (on-right-release)]
        [else null]))

;;;
;;; Rendering (drawing on the screen)
;;;

(define radians->rotation-coefficient
  (/ -360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     -90))

;;;
;;; State update
;;;




(define (update-physics!)
  (begin (for-each (λ (object)
                     (begin (set-game-object-orientation! object
                                                          (+ (game-object-orientation object)
                                                             (* inter-frame-interval
                                                                (game-object-rotational-velocity object))))
                            (set-game-object-position! object
                                                       (local [(define new-position
                                                                 (posn-+ (posn-* inter-frame-interval
                                                                                 (game-object-velocity object))
                                                                         (game-object-position object)))]
                                                         (make-posn (wrap (posn-x new-position) window-width)
                                                                    (wrap (posn-y new-position) window-height))))))
                   all-game-objects)
         (handle-collisions all-game-objects)))

;;;
;;; Collision handling
;;;

(define (handle-collisions objects)
  (unless (empty? objects)
    (local [(define head (first objects))
            (define tail (rest objects))]
      (begin (for-each (λ (object)
                         (when (collided? head object)
                           (handle-collision head object)))
                       tail)
             (handle-collisions tail)))))

(define (collided? a b)
  (< (distance-squared (game-object-position a)
                       (game-object-position b))
     (squared (+ (game-object-radius a)
                 (game-object-radius b)))))

(define (handle-collision a b)
  (cond [(and (target? a)
           (missile? b))
         (begin (destroy! b)
                (set! score (+ score 1))
                (set! wind-velocity (random-wind)))]
        [(and (target? b)
               (missile? a))
          (begin (destroy! a)
                 (set! score (+ score 1))
                 (set! wind-velocity (random-wind)))]
        [(and (missile? a)
              (missile? b))
         (begin
           (destroy! a)
           (destroy! b))]
        [(and (player? a)
           (missile? b))
         (destroy! b)]
        [(and (missile? a)
           (player? b))
         (destroy! a)]
        [else (void)])
  )
           

;;;
;;; Vector arithmetic
;;;

(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))


(define (forward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

;;;
;;; Randomization
;;;

(define random-color
  (local [(define colors
            (list (color 255 0 0)
                  (color 0 255 0)
                  (color 0 0 255)
                  (color 128 128 0)
                  (color 128 0 129)
                  (color 0 128 128)))]
    (λ () (random-element colors))))

(define (random-element list)
  (list-ref list
            (random (length list))))

(define (random-float min max)
  (+ min
     (* (random)
        (- max min))))

(define (random-velocity)
  (make-posn (random-float -10 10)
             (random-float -10 10)))

(define (random-wind)
  (make-posn (- (* (random) 3) 1.5) 0))
  

;;;
;;; Other arithmetic utilities
;;;

(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))

