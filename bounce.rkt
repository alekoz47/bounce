;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;;a ball that bounces off the walls of the window like a billiards ball

;todo: 
;- gravity on missed mousedown
;- create random angle function to generate unit vector for ball-start-mouse
;- create rotating color function
;- fix origin to bottom left

;;================
;;Constants:

(define WIDTH 800)
(define HEIGHT 800)
(define MTS (empty-scene WIDTH HEIGHT "black"))

;;================
;;Data definitions:

(define-struct pos (x y))
;;Position is (make-pos Number[0, WIDTH-MOD] Number[0, HEIGHT-MOD])

#;
(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

(define-struct vel (x y))
;;Velocity is (make-vel Number[-1, 1] Number[-1, 1])
#;
(define (fn-for-vel v)
  (... (vel-x v)
       (vel-y v)))

(define-struct ball (pos vel speed radius))
;;Ball is (make-ball Position Velocity Number Number)
#;
(define (fn-for-ball b)
  (... (fn-for-pos (ball-pos b))
       (fn-for-vel (ball-vel b))
       (ball-speed b)
       (ball-radius b)))

;;================
;;Functions:

;;Ball -> Ball
;;start world with (main ...)
(define (main b)
  (big-bang b
            (on-tick ball-status 0.01)
            (to-draw render)
            (on-key handle-key)
            (on-mouse handle-mouse)))

;;Ball -> Ball
;;decides bounce-ball if touching wall, otherwise move-ball
(define (ball-status b)
  (cond [(or (or (>= (round-five (pos-x (ball-pos b))) (- WIDTH (ball-radius b)))
                 (<= (round-five (pos-x (ball-pos b))) (+ 0 (ball-radius b))))
             (or (>= (round-five (pos-y (ball-pos b))) (- HEIGHT (ball-radius b)))
                 (<= (round-five (pos-y (ball-pos b))) (+ 0 (ball-radius b)))))
         (bounce-ball b)]
        [else (move-ball b)]))

;;Ball -> Ball
;;advances ball-pos-x and ball-pos-y in direction of ball-vel-x and ball-vel-y
;;    at scale of ball-speed
(define (move-ball b)
  (make-ball (make-pos (+ (round-five (pos-x (ball-pos b)))
                          (* (ball-speed b)
                             (round-five (vel-x (ball-vel b)))))
                       (+ (round-five (pos-y (ball-pos b)))
                          (* (ball-speed b)
                             (round-five (vel-y (ball-vel b))))))
             (make-vel (vel-x (ball-vel b))
                       (vel-y (ball-vel b)))
             (ball-speed b)
             (ball-radius b)))

;;Ball -> Ball
;;decides if ball is on side or top/bottom wall
(define (bounce-ball b)
  (cond [(or (>= (round-five (pos-x (ball-pos b))) (- WIDTH (ball-radius b)))
             (<= (round-five (pos-x (ball-pos b))) (+ 0 (ball-radius b))))
         (ball-up-down b)]
        [(or (>= (round-five (pos-y (ball-pos b))) (- HEIGHT (ball-radius b)))
             (<= (round-five (pos-y (ball-pos b))) (+ 0 (ball-radius b))))
         (ball-left-right b)]))

;;Ball -> Ball
;;bounces ball off of top or bottom of screen
(define (ball-up-down b)
  (make-ball (make-pos (- (round-five (pos-x (ball-pos b))) 
                          (* (ball-speed b) (round-five (vel-x (ball-vel b)))))
                       (+ (round-five (pos-y (ball-pos b)))
                          (* (ball-speed b) (round-five (vel-y (ball-vel b))))))
             (make-vel (- 0 (round-five (vel-x (ball-vel b))))
                       (round-five (vel-y (ball-vel b))))
             (ball-speed b)
             (ball-radius b)))

;;Ball -> Ball
;;bounces ball off of left or right of screen
(define (ball-left-right b)
  (make-ball (make-pos (+ (round-five (pos-x (ball-pos b)))
                          (* (ball-speed b) (round-five (vel-x (ball-vel b)))))
                       (- (round-five (pos-y (ball-pos b)))
                          (* (ball-speed b) (round-five (vel-y (ball-vel b))))))
             (make-vel (round-five (vel-x (ball-vel b)))
                       (- 0 (round-five (vel-y (ball-vel b)))))
             (ball-speed b)
             (ball-radius b)))

;;Number -> Number
;;truncates inexact number to 5 decimal places
(define (round-five n)
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;Ball -> Image
;;render ball on screen at position ball-pos-x, ball-pos-y
;!!!
(define (render b)
  (place-image (circle (ball-radius b) "solid" "red")
               (pos-x (ball-pos b))
               (pos-y (ball-pos b))
               MTS))

;;Ball KeyEvent -> Ball
;;d: ball-speed + 1, a: ball-speed - 1, w: ball-radius + 1, s: ball-radius - 1
(define (handle-key b ke)
  (cond [(key=? ke "d")
         (make-ball (make-pos (round-five (pos-x (ball-pos b)))
                              (round-five (pos-y (ball-pos b))))
                    (make-vel (round-five (vel-x (ball-vel b)))
                              (round-five (vel-y (ball-vel b))))
                    (+ (ball-speed b) 1)
                    (ball-radius b))]
        [(key=? ke "a")
         (make-ball (make-pos (round-five (pos-x (ball-pos b)))
                              (round-five (pos-y (ball-pos b))))
                    (make-vel (round-five (vel-x (ball-vel b)))
                              (round-five (vel-y (ball-vel b))))
                    (- (ball-speed b) 1)
                    (ball-radius b))]
        [(key=? ke "w") 
         (make-ball (make-pos (round-five (pos-x (ball-pos b)))
                              (round-five (pos-y (ball-pos b))))
                    (make-vel (round-five (vel-x (ball-vel b)))
                              (round-five (vel-y (ball-vel b))))
                    (ball-speed b)
                    (+ (ball-radius b) 1))]
        [(key=? ke "s") 
         (make-ball (make-pos (round-five (pos-x (ball-pos b)))
                              (round-five (pos-y (ball-pos b))))
                    (make-vel (round-five (vel-x (ball-vel b)))
                              (round-five (vel-y (ball-vel b))))
                    (ball-speed b)
                    (decrease-ball-radius b))]
        [else 
         (make-ball (make-pos (round-five (pos-x (ball-pos b)))
                              (round-five (pos-y (ball-pos b))))
                    (make-vel (round-five (vel-x (ball-vel b)))
                              (round-five (vel-y (ball-vel b))))
                    (ball-speed b)
                    (ball-radius b))]))

;;Ball -> Ball
;;decreases ball radius by one if not zero
(define (decrease-ball-radius b)
  (if (not (= (ball-radius b) 0))
      (- (ball-radius b) 1)
      (ball-radius b)))

;;Ball MouseEvent -> Ball
;;determine if mouseclick is within boundary of ball-radius
;;!!!
(define (handle-mouse b x y me)
  (cond [(<= (sqrt (+ (sqr (- (pos-x (ball-pos b)) x))
                      (sqr (- (pos-y (ball-pos b)) y))))
             (ball-radius b))
         (ball-moving? b x y me)]
        [else (make-ball (make-pos (pos-x (ball-pos b))
                                   (pos-y (ball-pos b)))
                         (make-vel (vel-x (ball-vel b))
                                   (vel-y (ball-vel b)))
                         (ball-speed b)
                         (ball-radius b))]))

;;Ball MouseEvent -> Ball
;;determines if ball is stopped or moving, then relays stop or start functions
;;!!!
(define (ball-moving? b x y me)
  (cond [(and (= 0 (vel-x (ball-vel b))) (= 0 (vel-y (ball-vel b))))
         (ball-start-mouse b x y me)]
        [else (ball-stop-mouse b x y me)]))

;;Ball MouseEvent -> Ball
;;on button-down, stop ball
;!!!
(define (ball-stop-mouse b x y me)
  (cond [(mouse=? me "button-down")
         (make-ball (make-pos (pos-x (ball-pos b))
                              (pos-y (ball-pos b)))
                    (make-vel 0 0)
                    (ball-speed b)
                    (ball-radius b))]
        [else (make-ball (make-pos (pos-x (ball-pos b))
                                   (pos-y (ball-pos b)))
                         (make-vel (vel-x (ball-vel b))
                                   (vel-y (ball-vel b)))
                         (ball-speed b)
                         (ball-radius b))]))

;;Ball MouseEvent -> Ball
;;on button-down, restart ball
;;!!!
(define (ball-start-mouse b x y me)
  (cond [(mouse=? me "button-down")
         (make-ball (make-pos (pos-x (ball-pos b))
                              (pos-y (ball-pos b)))
                    (make-vel (/ (sqrt 3) 2) (/ 1 2))
                    (ball-speed b)
                    (ball-radius b))]
        [else (make-ball (make-pos (pos-x (ball-pos b))
                                   (pos-y (ball-pos b)))
                         (make-vel (vel-x (ball-vel b))
                                   (vel-y (ball-vel b)))
                         (ball-speed b)
                         (ball-radius b))]))

;;================
;;Run:

(main (make-ball (make-pos (/ WIDTH 2)
                           (/ HEIGHT 2))
                 (make-vel (/ (sqrt 3) 2)
                           (/ 1 2))
                 10
                 50))