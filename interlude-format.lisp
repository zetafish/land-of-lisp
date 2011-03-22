;;; Some examples with format
(format t "Add onion rings for only ~$ dollars more!" 1.5)

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))

(defun random-animal ()
  (let ((s (list "dog" "tick" "tiger" "walrus" "kangaroo")))
    (nth (random (length s)) s)))

;; fixed column width
(loop repeat 10
   do (format t "~5t~a ~15t~a ~25t~a~%"
              (random-animal)
              (random-animal)
              (random-animal)))

;; evenly spaced
(loop repeat 10
   do (format t "~30<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

;; aligned to center
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

;; center aligned 3 animals per row
(loop repeat 10 do
     (format t "~30:@<~a~;~a~;~a~>~%"
             (random-animal)
             (random-animal)
             (random-animal)))

;; center aligned and columns nice
(loop repeat 10 do
     (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
             (random-animal)
             (random-animal)
             (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animals*)
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

;; yeah some pretty stuff
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

(defun robots ()
  (loop named main
     with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                         (d .   1) (z .  63) (x .  64) (c . 65))
     for pos = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                 (force-output)
                 (let* ((c (read))
                        (d (assoc c directions)))
                   (cond (d (+ pos (cdr d)))
                         ((eq 't c) (random 1024))
                         ((eq 'l c) (return-from main 'bye))
                         (t pos))))
     for monsters = (loop repeat 10
                       collect (random 1024))
     then (loop for mpos in monsters
             collect (if (> (ount mpos monsters) 1)
                         mpos
                         (cdar (sort (loop for (k . d) in directions
                                        for new-mpos = (+ mpos d)
                                        collect (cons (+ (abs (- (mod new-mpos 64)
                                                                 (mod pos 64)))
                                                         (abs (- (ash new-mpos -6)
                                                                 (ash pos -6))))
                                                      new-mpos))
                                     '<
                                     :key #'car))))
     when (loop for mpos in monsters
             always (> (count mpos monsters) 1))
     return 'player-wins
       do (format t)))

