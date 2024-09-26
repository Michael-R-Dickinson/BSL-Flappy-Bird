;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flappy-bird) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(define (scale-and-crop image desired-width desired-height)
  (local [(define original-width (image-width image))
          (define original-height (image-height image))
          (define scale-factor (/ desired-height original-height))
          (define scaled-image (scale scale-factor image))
          (define scaled-width (image-width scaled-image))
          (define x-offset (floor (/ (- scaled-width desired-width) 2)))
          (define cropped-image (crop x-offset 0 desired-width desired-height scaled-image))]
    cropped-image))

(define SPEED 5)
(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define TEXT-DISPLAY-Y (- CTR-Y 90))


(define BG
  (bitmap/url "https://wallpapers.com/images/hd/flappy-bird-background-gecj5m4a9yhhjp87.jpg"))
;(define MTS (rectangle WIDTH HEIGHT "solid" "white"))
(define MTS (scale-and-crop BG WIDTH HEIGHT))

(define PIPE-WIDTH 50)

(define PIPE-START (+ WIDTH PIPE-WIDTH))
(define PIPE-END (- PIPE-WIDTH))
(define MIN-PIPE-Y 145)
(define MAX-PIPE-Y (- HEIGHT 145))
(define PIPE-HOLE 140)
(define PIPE-BETWEEN-GAP 300) ; !!!

(check-expect (> (- HEIGHT MAX-PIPE-Y) PIPE-HOLE) true)
(check-expect (> MIN-PIPE-Y PIPE-HOLE) true)


(define GRAVITY 2.6)

(define BIRD-RADIUS 20)
(define BIRD-X CTR-X)
;(define BIRD (circle BIRD-RADIUS "solid" "yellow"))
(define BIRD-IMG
  (bitmap/url "https://i.pinimg.com/originals/dd/b0/a4/ddb0a4f27b9be7ffedf03a872545a3e2.png"))
(define BIRD (scale-and-crop BIRD-IMG 40 30))
(define JUMP-HEIGHT 17)

;(define BG
;  (bitmap/url
;   "https://i.pinimg.com/736x/72/5e/5d/725e5dc00ba49c240cd489e7b87e0496.jpg"))

;; Helper function
(define (rand-in bot top)
  (+ bot (random (- top bot))))

(@htdd Bird)
(define-struct bird (y dy))
;; Bird is (make-bird Number Number)
;; interp. a bird with y coordinate and y velocity

(define B1 (make-bird 30 40))

(@dd-template-rules compound) ; 2 fields
(define (fn-for-bird b)
  (... (bird-y b)   ; Number
       (bird-dy b))); Number

(@htdd Pipe)
(define-struct pipe (x y))
;; Pipe is (make-pipe Number Number)
;; interp. the y position and velocity of a bird

(@dd-template-rules compound) ; 2 fields

(define P1 (make-pipe 10 20))

(define (fn-for-pipe p)
  (... (pipe-x p) (pipe-y p)))


(@htdd GameStage)
;; GameStage is one of:
;; - "start"
;; - "in progress"
;; - "ended"
;; interp. whether the game has started, is in progress, or ended
(@dd-template-rules one-of
                    atomic-distinct  ; "start"
                    atomic-distinct  ; "in progress"
                    atomic-distinct) ; "ended"

(define (fn-for-game-stage gst)
  (cond [(string=? gst "start") (...)]
        [(string=? gst "in progress") (...)]
        [else (...)]))

(@htdd GS)
(define-struct gs (bird pipe1 pipe2 pipe3 stage score))
;; GS is (make-gs Bird Pipe Pipe Pipe GameStage Number)
;; interp. state of the current game
;;   bird is the state of the character's flappy bird
;;   pipe1, 2 and 3 are the pipes actively on the screen
;;   stage denotes whether the game has started, is in progress, or ended
(define GS1 (make-gs (make-bird CTR-Y 0)
                     (make-pipe 450 (rand-in MIN-PIPE-Y MAX-PIPE-Y))
                     (make-pipe 750 (rand-in MIN-PIPE-Y MAX-PIPE-Y))
                     (make-pipe 1050 (rand-in MIN-PIPE-Y MAX-PIPE-Y))
                     "start"
                     0))

(@dd-template-rules compound) ; 4 fields
(define (fn-for-gs gs)
  (... (gs-bird gs)
       (gs-pipe1 gs)
       (gs-pipe2 gs)
       (gs-pipe3 gs)))

;; -------------------------------------------------------
;; -------------------------------------------------------
;; FUNCTIONS

(@htdf main)
(@signature GS -> GS)
;; starts the game, call with (main GS1)
;; <no tests for main functions>

(@template-origin GS)
(@template
 (define (main gs)
   (... (gs-bird gs)
        (gs-pipe1 gs)
        (gs-pipe2 gs)
        (gs-pipe3 gs))))

(define (main gs)
  (big-bang gs
    (on-tick next-state)
    (on-draw draw-game)
    (on-key handle-key)))

(@htdf next-state)
(@signature GS -> GS)
;; produces the next bird and pipe state
; (check-expect (next-state ))

(@template-origin GS)
(@template
 (define (main gs)
   (... (gs-bird gs)
        (gs-pipe1 gs)
        (gs-pipe2 gs)
        (gs-pipe3 gs))))

(define (next-state gs)
  (cond
    [(game-over? gs) (end-game gs)]
    [(string=? (gs-stage gs) "in progress")
     (make-gs (next-bird (gs-bird gs))
              (next-pipe (gs-pipe1 gs) (pipe-respawn-start
                                        (gs-pipe1 gs)
                                        (gs-pipe2 gs)
                                        (gs-pipe3 gs)))
              (next-pipe (gs-pipe2 gs) (pipe-respawn-start
                                        (gs-pipe1 gs)
                                        (gs-pipe2 gs)
                                        (gs-pipe3 gs)))
              (next-pipe (gs-pipe3 gs) (pipe-respawn-start
                                        (gs-pipe1 gs)
                                        (gs-pipe2 gs)
                                        (gs-pipe3 gs)))
              "in progress"
              4)]
    [else gs]))

(define (pipe-distance p1 p2)
  (abs (- (pipe-x p1) (pipe-x p2))))
;; Takes 2 pipes and determines the x coordinate to respawn the 3rd at
(define (pipe-respawn-start p1 p2 p3)
  (cond [(and (> (pipe-x p1) (pipe-x p2))
              (> (pipe-x p1) (pipe-x p3)))
         (+ (pipe-x p1) PIPE-BETWEEN-GAP)]
        [(> (pipe-x p2) (pipe-x p3)) (+ (pipe-x p2) PIPE-BETWEEN-GAP)]
        [else (+ (pipe-x p3) PIPE-BETWEEN-GAP)]))



;; Subtracts SPEED from the x coord or reset it to the start of screen
(@htdf next-pipe)
(@signature Pipe -> Pipe)
;; check expect

(@template-origin Pipe)
(@template
 (define (next-pipe p)
   (... (pipe-x p) (pipe-y p))))

(define (next-pipe p respawn-x)
  (if (< (- (pipe-x p) SPEED) PIPE-END)
      (make-pipe respawn-x (rand-in MIN-PIPE-Y MAX-PIPE-Y))
      (make-pipe (- (pipe-x p) SPEED) (pipe-y p))))

(@htdf next-bird)
(@signature Bird -> Bird)
;; Adds bird dy to y and subtracts gravity from dy
;; check-expect
(@template-origin Bird)
(@template
 (define (next-bird b)
   (... (bird-y b)   ; Number
        (bird-dy b))))

(define (next-bird b)
  (make-bird (+ (bird-y b) (bird-dy b)) (+ (bird-dy b) GRAVITY)))


(define (draw-bird b scrn)
  (place-image BIRD BIRD-X (bird-y b) scrn))

(define (draw-pipe p scrn)
  (place-image
   (rectangle PIPE-WIDTH (* (pipe-y p) 2) "solid" "green") (pipe-x p) 0
   (place-image
    (rectangle
     PIPE-WIDTH
     (* (- (- HEIGHT (pipe-y p)) PIPE-HOLE) 2)
     "solid"
     "green")
    (pipe-x p)
    HEIGHT
    scrn)))

(define (draw-game-foreground s)
  (draw-pipe
   (gs-pipe3 s)
   (draw-pipe (gs-pipe2 s)
              (draw-pipe (gs-pipe1 s)
                         (draw-bird (gs-bird s) MTS)))))
(define (draw-game s)
  (cond [(string=? (gs-stage s) "start")
         (place-image
          (text "Press 'space' to start" 30 "blue")
          CTR-X TEXT-DISPLAY-Y
          (draw-game-foreground s))]
        [(string=? (gs-stage s) "ended")
         (place-image
          (text "L bozo" 30 "red")
          CTR-X TEXT-DISPLAY-Y
          (draw-game-foreground s))]
        [else (draw-game-foreground s)]))

(define (handle-key gs k)
  (cond [(key=? k " ")
         (make-gs
          (make-bird (bird-y (gs-bird gs)) (- JUMP-HEIGHT))
          (gs-pipe1 gs)
          (gs-pipe2 gs)
          (gs-pipe3 gs)
          (if (or (string=? (gs-stage gs) "in progress")
                  (string=? (gs-stage gs) "start"))
              "in progress"
              "ended")
          (gs-score gs))]
        [(key=? k "r" ) GS1]
        [else gs]))

;; -------------------------------------------------------
;; -------------------------------------------------------
;; Collision Helpers
(define (game-over? gs)
  (or (pipe-bird-collision? (gs-pipe1 gs) (gs-bird gs))
      (pipe-bird-collision? (gs-pipe2 gs) (gs-bird gs))
      (pipe-bird-collision? (gs-pipe3 gs) (gs-bird gs))))

(define (pipe-bird-collision? p b)
  (or (floor-collision? b)
      (and (pipe-bird-y-intersection? p b)
           (pipe-bird-x-intersection? p b))))

(define (pipe-bird-x-intersection? p b)
  (and (>= (+ BIRD-X BIRD-RADIUS) (- (pipe-x p) (/ PIPE-WIDTH 2)))
       (<= (- BIRD-X BIRD-RADIUS) (+ (pipe-x p) (/ PIPE-WIDTH 2)))))

(define (pipe-bird-y-intersection? p b)
  (or (>= (+ (bird-y b) BIRD-RADIUS) (+ (pipe-y p) PIPE-HOLE))
      (<= (- (bird-y b) BIRD-RADIUS) (pipe-y p))))

(define (floor-collision? b)
  (>= (+ (bird-y b) BIRD-RADIUS) HEIGHT))

(define (notify-gamestate gs)
  (make-gs (gs-bird gs)
           (next-pipe (make-pipe
                       (pipe-x (gs-pipe1 gs))
                       (+ 1 (pipe-y (gs-pipe1 gs))))
                      (pipe-respawn-start
                       (gs-pipe1 gs)
                       (gs-pipe2 gs)
                       (gs-pipe3 gs)))
           (next-pipe (make-pipe
                       (pipe-x (gs-pipe2 gs))
                       (+ 1 (pipe-y (gs-pipe2 gs))))
                      (pipe-respawn-start
                       (gs-pipe1 gs)
                       (gs-pipe2 gs)
                       (gs-pipe3 gs)))
           (next-pipe (make-pipe
                       (pipe-x (gs-pipe3 gs))
                       (+ 1 (pipe-y (gs-pipe3 gs))))
                      (pipe-respawn-start
                       (gs-pipe1 gs)
                       (gs-pipe2 gs)
                       (gs-pipe3 gs)))
           "in progress"
           (gs-score gs)))

(define (end-game gs)
  (make-gs (gs-bird gs)
           (gs-pipe1 gs)
           (gs-pipe2 gs)
           (gs-pipe3 gs)
           "ended"
           (gs-score gs)))
