;;; Orc Battle Game
;;
;; In the Orc battle game you are a knight surrounded by 12 monsters,
;;engaged in a fight to the death. With your superior wits and your
;;repetoire of sword fighting manouvres, you must carefully strategize
;;in your battle with orcs, hydras, and other nasy enemies.

;; global variables for player
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;; global variables for monsters
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;; main game functions
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list (lambda (m) (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

;; player management functions
(defun init-player ()
  (setf *player-health* 30
        *player-agility* 30
        *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monster-dead)
                   (monster-hit (random-monster) 1))))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))



