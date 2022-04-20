;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT (color 0 0 0 0)))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; Data Definitions:

;; Game
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom       (game-missiles s))
       (fn-for-tank      (game-tank     s))))


;; Tank
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;; Invader
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 150 100 -12))          ;not landed, moving left

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvaders
(define-struct invaders (x y dx))
;; Invaders is one of:
;;  - empty
;;  - (cons (make-invader Natural Natural Natural) Invaders)
;; interp. a list of invaders, where each 
;;           x   is the invader position x in pixels 
;;           y   is the invader position y in pixels
;;           dx  is the invader along x by dx pixels per clock tick

(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I1 I4))
(define LOI4 (list I1 I2 I3))
(define LOI5 (list (make-invader 100 200 10) (make-invader 140 250 -10) (make-invader 200 290 -10) (make-invader 180 210 10)))

#;
(define (fn-for-invaders invaders)
  (cond [(empty? invaders) (...)]
        [else
         (... (invader-x       (first invaders)) ;Natural
              (invader-y       (first invaders)) ;Natural
              (invader-dx      (first invaders)) ;Natural
              (fn-for-invaders (rest invaders)))]))

;; Missile
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1
(define M4 (make-missile 150 250))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles
(define-struct lom (x y))
;; Missiles is one of:
;;  - empty
;;  - (cons (make-missile Natural Natural) Missiles)
;; interp. a list of missiles, where each 
;;           x is the missile position x in pixels 
;;           y is the missile position y in pixels

(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M2 M3))
(define LOM4 (list M1 M2 M3 M4))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (missile-x      (first lom))   ;Natural
              (missile-y      (first lom))   ;Natural
              (fn-for-lom     (rest  lom)))]))

;; Functions:

;; Game -> Game
;; Called to start the space invaders game; start with (main (make-game LOI LOM 0))
;; no tests for main function
(define (main game)
  (big-bang game
    (on-tick advance-game) ; Game          -> Game
    (to-draw render-game)  ; Game          -> Image
    (on-key  handle-key))) ; Game KeyEvent -> Game

;; Game -> Game
;; produce next game state

;(define (advance-game s) 0) ; stub

(define (advance-game s)
  (if (game-over? (game-invaders s))
      s
      (make-game
       (advance-invaders (create-invader (remove-hit-invaders (game-invaders s) (game-missiles s))))
       (advance-missiles (remove-hit-missiles (game-missiles s) (game-invaders s)))
       (advance-tank     (game-tank s)))))

;; Game -> Image
;; produce game image to render

; (define (render-game g) BACKGROUND) ; stub

(define (render-game s)
  (overlay
   (render-invaders (game-invaders s))
   (render-missiles (game-missiles s))
   (render-tank     (game-tank     s))))

;; ListOfInvader -> ListOfInvader
;; Returns a new list of invaders with an additional invader added at INVADE-RATE intervals

;(define (create-invaders loi) loi)

(define (create-invader loi)
  (cond[(> (random 108) INVADE-RATE)
        (cons (make-invader (random WIDTH) 0 1) loi)]
       [else loi]))

;; ListOfInvaders -> Image
;; produces an image of invaders
(check-expect (render-invaders (list
                                (make-invader 150 200 10)
                                (make-invader 200 250 15)
                                (make-invader 300  80 10)))
              (place-image INVADER 150 200
                           (place-image INVADER 200 250
                                        (place-image INVADER 300 80 BACKGROUND))))

; (define (render-invaders loi) 0); stub

(define (render-invaders invaders)
  (cond [(empty? invaders) BACKGROUND]
        [else
         (place-image INVADER (invader-x (first invaders)) (invader-y (first invaders))
                      (render-invaders (rest invaders)))]))

;; ListOfMissiles -> Image
;; produces an image of missiles
(check-expect (render-missiles (list
                                (make-missile 150 200)
                                (make-missile 200 250)))
              (place-image MISSILE 150 200 (place-image MISSILE 200 250 BACKGROUND)))

; (define (render-missiles lom) 0) ; stub

(define (render-missiles missiles)
  (cond [(empty? missiles) BACKGROUND]
        [else
         (place-image MISSILE (missile-x (first missiles)) (missile-y (first missiles))
                      (render-missiles (rest missiles)))]))

;; Tank -> Image
;; renders tank image
(check-expect (render-tank (make-tank 10 1)) (place-image TANK 10 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

; (define (render-tank t) 0) ;stub

(define (render-tank t) (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game Key -> Game
;; handle user arrow and space keys, where:
;; arrow right changes tank direction to the right
;; arrow left changes tank direction to the left
;; space add new invader at the end of game ListOfInvaders
;; based on position of the tank x and y
(check-expect (handle-key (make-game empty empty (make-tank 50 1))  "left") (make-game empty empty (make-tank 48 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "right") (make-game empty empty (make-tank 52  1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1))     " ") (make-game empty (list (make-missile 50 HEIGHT)) (make-tank 50  1)))


; (define (handle-key g key) 0) ; stub

(define (handle-key s a-key)
  (cond
    [(key=? a-key "left")  (make-game (game-invaders s) (game-missiles s) (make-tank (- (tank-x (game-tank s)) TANK-SPEED) -1))]
    [(key=? a-key "right") (make-game (game-invaders s) (game-missiles s) (make-tank (+ (tank-x (game-tank s)) TANK-SPEED)  1))]
    [(key=? a-key " ")     (make-game (game-invaders s) (insert-missile (game-missiles s) (tank-x (game-tank s))) (game-tank s))]
    [else s]))

;; ListOfMissiles Number -> ListOfMissiles
;; insert missile into a ListOfMissiles, in posn x Number
(check-expect (insert-missile empty 10) (list (make-missile 10 HEIGHT)))
(check-expect (insert-missile (list (make-missile 20 30) (make-missile 40 50)) 20)
              (list (make-missile 20 30) (make-missile 40 50) (make-missile 20 HEIGHT)))

; (define (insert-misssile loi x) empty) ;stub

(define (insert-missile lom x)
  (cond [(empty? lom) (cons (make-missile x HEIGHT) empty)]
        [else
         (cons (make-missile (missile-x (first lom)) (missile-y (first lom)))
               (insert-missile (rest lom) x))]))

;; ListOfInvaders -> Boolean
;; produce true if any invader has landed (height is greater or equal HEIGHT);
(check-expect (game-over? empty) false)
(check-expect (game-over? LOI2)  false)
(check-expect (game-over? LOI4)   true)

; (define (game-over? loi) false) ; stub

;; took template from Invaders

(define (game-over? invaders)
  (cond [(empty? invaders) false]
        [(>= (invader-y (first invaders)) HEIGHT) true]
        [else (game-over? (rest invaders))]))

;; ListOfInvaders -> ListOfInvaders
;; produce next ListOfInvaders state, advancing invader-x and invader-y.
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 150 100 10) (make-invader 250 200 10)))
              (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 10) (make-invader (+ 250 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 10)))
(check-expect (advance-invaders (list (make-invader 150 100 -10) (make-invader 250 200 10)))
              (list (make-invader (- 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -10) (make-invader (+ 250 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 10)))
(check-expect (advance-invaders (list (make-invader 150 WIDTH 10) (make-invader 250 200 10)))
              (list (make-invader (+ 150 INVADER-X-SPEED) 301.5 10) (make-invader (+ 250 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 10)))
(check-expect (advance-invaders (list (make-invader 150 0 -10) (make-invader 250 200 10)))
              (list (make-invader (- 150 INVADER-X-SPEED) 1.5 -10) (make-invader (+ 250 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 10)))
(check-expect (advance-invaders (list (make-invader 150 0 10) (make-invader 250 200 10)))
              (list (make-invader (+ 150 INVADER-X-SPEED) 1.5 10) (make-invader (+ 250 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 10)))

; (define (advance-invaders loi) 0) ; stub

(define (advance-invaders invaders)
  (cond [(empty? invaders) empty]
        [else
         (cons (advance-invader (first invaders)) (advance-invaders (rest invaders)))]))

;; Invader -> Invader
;; produce next invader position x, y and dx
(check-expect (advance-invader (make-invader 10 20  10)) (make-invader (+ 10 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED)  10))
(check-expect (advance-invader (make-invader 15 20 -10)) (make-invader (- 15 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) -10))

;(define (advance-invader invader) 0) ;stub

(define (advance-invader invader)
  (make-invader (next-invader-x (invader-x invader) (next-invader-dx (invader-dx invader) (invader-x invader)))
                (+ (invader-y invader) INVADER-Y-SPEED)
                (next-invader-dx (invader-dx invader) (invader-x invader))))

;; Number Number -> Number
;; the position that should change is position x not position y. Fix this before adding key missiles and random invader
;; produces next-invader-x position, where:
;; first Number is invader pos x
;; second Number is invader pos dx
(check-expect (next-invader-x  10  10)   11.5)
(check-expect (next-invader-x  10 -10)    8.5)
(check-expect (next-invader-x 300  10)    299)
(check-expect (next-invader-x 299  10)  300.5)
(check-expect (next-invader-x   1 -10)   -0.5)
(check-expect (next-invader-x   0 -10)      1)
(check-expect (next-invader-x  40 -10)   38.5)

; (define (next-invader-x x dx) 0) ;stub

(define (next-invader-x x dx)
  (cond [(<= x 0) 1]
        [(>= x WIDTH) 299]
        [else (if (negative? dx) (- x INVADER-X-SPEED) (+ x INVADER-X-SPEED))]))

;; Number Number -> Number
;; produces next-invader-dx position, where:
;; first Number is invader pos dx
;; second Number is invader pos y
(check-expect (next-invader-dx  10  50)   10)
(check-expect (next-invader-dx -10  60)  -10)
(check-expect (next-invader-dx -10  -2)   10)
(check-expect (next-invader-dx  10 302)  -10)
(check-expect (next-invader-dx -10  20)  -10)
(check-expect (next-invader-dx  10 299)   10)
(check-expect (next-invader-dx  10 300)  -10)
(check-expect (next-invader-dx -10 450)  -10)

; (define (next-invader-dx dx x) 0) ;stub

(define (next-invader-dx dx x)
  (cond
    [(<= x 0)   (abs dx)]
    [(and (>= x WIDTH) (negative? dx)) dx]
    [(>= x WIDTH) (- dx)]
    [else dx]))

;; ListOfMissiles -> ListOfMissiles
;; produce list of missiles, advancing each missile y pos by one.
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 250 100) (make-missile 350 200)))
              (list (make-missile 250 (- 100 MISSILE-SPEED)) (make-missile 350 (- 200 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 250 100) (make-missile 350 200) (make-missile 450 350)))
              (list (make-missile 250 (- 100 MISSILE-SPEED)) (make-missile 350 (- 200 MISSILE-SPEED)) (make-missile 450 (- 350 MISSILE-SPEED))))

; (define (advance-missiles lom) empty) ; stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (advance-missiles (rest lom)))]))

;; Tank -> Tank
;; Produce a Tank with incremented or decremented position x based on dir Interval[-1,1]
;; Assume: Tank dir remains the same.
(check-expect (advance-tank (make-tank 0  0))  (make-tank 1  1))
(check-expect (advance-tank (make-tank 1  1))  (make-tank (+ 1 TANK-SPEED)  1))
(check-expect (advance-tank (make-tank 2 -1))  (make-tank (- 2 TANK-SPEED) -1))

; (define (advance-tank t) (make-tank 0 0)) ; stub

;; took template from Tank

(define (advance-tank t)
  (cond [(<= (tank-x t) 0)     (make-tank                        1               1)]
        [(>= (tank-x t) WIDTH) (make-tank               (- WIDTH 1)             -1)]
        [(= (tank-dir t)  1)   (make-tank (+ (tank-x t) TANK-SPEED)   (tank-dir t))]
        [(= (tank-dir t) -1)   (make-tank (- (tank-x t) TANK-SPEED)   (tank-dir t))]
        [else (make-tank 0 0)]))

;; Helper Functions:

;; Invader ListOfMissiles -> ListOfMissile
;; produce a list of missiles that do not hit the given invader.
(check-expect (non-hit-missiles I1 empty) empty)
(check-expect (non-hit-missiles I1 LOM3) (list M1 M3))
(check-expect (non-hit-missiles (make-invader 155 105 10) (list M1 (make-missile 150 190) M3 M4))
              (list M1 (make-missile 150 190) M4))

; (define (non-hit-missiles invader lom) lom) ;stub

(define (non-hit-missiles invader lom)
  (cond [(empty? lom) empty]
        [(missile-hits-invader? (first lom) invader) (rest lom)]
        [else (cons (first lom) (non-hit-missiles invader (rest lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produce true if any missile in a given lom hits given invader
(check-expect (lom-hits-invader? I1 empty) false)
(check-expect (lom-hits-invader? I1 (list (make-missile 450 500) (make-missile 600 800))) false)
(check-expect (lom-hits-invader? I1 LOM4) true)
(check-expect (lom-hits-invader? (first
                                  (list (make-invader 150 500 -10)
                                        (make-invader 150 510 10)))
                                 (list (make-missile 150 300)
                                       (make-missile 150 105)
                                       (make-missile 150 250))) false)

;(define (lom-hits-invader? i lom) false) ;stub

(define (lom-hits-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (if (missile-hits-invader? (first lom) i)
             true
             (lom-hits-invader? i (rest lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produce true if any invader in a given loi hits given missile
(check-expect (loi-hits-missile? M1 LOI4) false)
(check-expect (loi-hits-missile? M2 LOI4) true)

; (define (loi-hits-missile? m loi) false) ;stub

(define (loi-hits-missile? m loi)
  (cond [(empty? loi) false]
        [else
         (if (missile-hits-invader? m (first loi))
             true
             (loi-hits-missile? m (rest loi)))]))

;; Missile ListOfInvaders -> ListOfInvaders
;; produce a list of invaders that are not hit by the given missile.
(check-expect (non-hit-invaders M1 empty) empty)
(check-expect (non-hit-invaders (make-missile 145 505) LOI4) (list I1 I3))
(check-expect (non-hit-invaders (make-missile 155 105)
                                (list (make-invader 250 100 10)
                                      (make-invader 150 110 10)
                                      (make-invader 350 100 10)))
              (list (make-invader 250 100 10) (make-invader 350 100 10)))


; (define (non-hit-invaders missile loi) loi) ;stub

(define (non-hit-invaders missile loi)
  (cond [(empty? loi) empty]
        [(missile-hits-invader? missile (first loi)) (rest loi)]
        [else (cons (first loi) (non-hit-invaders missile (rest loi)))]))

;; Missile Invader -> Boolean
;; produce true if given missile hits given invader, otherwise produce false
(check-expect (missile-hits-invader? (make-missile 100 120) (make-invader 110 125   5))   true) ; in the hit range
(check-expect (missile-hits-invader? (make-missile 150 220) (make-invader 150 220  10))   true) ; same range
(check-expect (missile-hits-invader? (make-missile 149 220) (make-invader 150 220  10))   true) ; below hit range
(check-expect (missile-hits-invader? (make-missile 150 180) (make-invader 185 125   5))  false) ; above hit range
(check-expect (missile-hits-invader? (make-missile 150 300) (make-invader 150 500 -10))  false)

(define (missile-hits-invader? missile invader)
  (and
   (<= (abs (- (missile-x missile) (invader-x invader))) HIT-RANGE)
   (<= (abs (- (missile-y missile) (invader-y invader))) HIT-RANGE)))

;; ListOfInvaders ListOfMissiles -> Missile
;; produces first missile that hits any invader in ListOfInvaders
;; assume at least one invader is hit by on missile of ListOfMissiles
(check-expect (first-hit-missile (cons (make-invader 110 125 5) empty)
                                 (cons (make-missile 100 120) empty))
              (make-missile 100 120))
(check-expect (first-hit-missile LOI4 LOM4) (make-missile 150 110))
(check-expect (first-hit-missile LOI4 empty) empty)
(check-expect (first-hit-missile empty LOM4) empty)

(define (first-hit-missile loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi) empty]
        [else
         (if
          (and
           (<= (missile-x (first lom)) (+ (invader-x (first loi)) HIT-RANGE))
           (<= (missile-y (first lom)) (+ (invader-y (first loi)) HIT-RANGE)))
          (first lom)
          (first-hit-missile (rest loi) (rest lom)))]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; produce list of non hit invaders given invaders and missiles.
;(check-expect (remove-hit-invaders LOI1 LOM1) LOI1)
(check-expect (remove-hit-invaders LOI4 LOM4) (list (make-invader 150 500 -10) (make-invader 150 510 10)))
(check-expect (remove-hit-invaders LOI4 empty) LOI4)

; (define (remove-hit-invaders loi lom) empty) ;stub

(define (remove-hit-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [(lom-hits-invader? (first loi) lom) (remove-hit-invaders (rest loi) (non-hit-missiles (first loi) lom))]
        [else (cons (first loi) (remove-hit-invaders (rest loi) lom))]))

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; produce list of non hit missiles given missiles and invaders.
(check-expect (remove-hit-missiles LOM4 empty) LOM4)
(check-expect (remove-hit-missiles LOM4 LOI4)
              (list (make-missile 150 300) (make-missile 150 105) (make-missile 150 250)))

; (define (remove-hit-missiles lom loi) empty) ;stub

(define (remove-hit-missiles lom loi)
  (cond [(empty? loi) lom]
        [(empty? lom) empty]
        [(loi-hits-missile? (first lom) loi) (remove-hit-missiles (rest lom) (non-hit-invaders (first lom) loi))]
        [else (cons (first lom) (remove-hit-missiles (rest lom) loi))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game LOI5 (list M1 M2) T1))

(main G0)

