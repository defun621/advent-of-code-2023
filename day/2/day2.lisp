(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)


(defstruct game
  id
  rgbs)

(defstruct rgb
  r
  g
  b)

(defun parse-game-id (game-id-string)
  (let ((tokens (cl-ppcre:split " " game-id-string)))
    (parse-integer (car (cdr tokens)))))

(defun parse-subsets (subsets-line)
  (let ((subsets (cl-ppcre:split ";" subsets-line)))
    (reduce (lambda (acc x)
              (let ((subset (parse-subset (string-trim " " x))))
                (cons subset acc)))
            subsets
            :initial-value nil)))

(defun parse-subset (subset-line)
  (let ((draws (cl-ppcre:split "," subset-line)))
    (reduce (lambda (acc x)
              (let* ((draw (parse-draw (string-trim " " x)))
                     (amount (car draw))
                     (color (cdr draw)))
                (cond
                  ((string= "red" color) (setf (rgb-r acc) amount))
                  ((string= "blue" color) (setf (rgb-b acc) amount))
                  (t (setf (rgb-g acc) amount)))
                acc))
            draws
            :initial-value (make-rgb :r 0 :g 0 :b 0))))

(defun parse-draw (draw-line)
  ;returns a draw 
  (let ((tokens (cl-ppcre:split " " draw-line)))
    (cons (parse-integer (car tokens))
          (car (cdr tokens)))))

(defun parse-line (line)
  (let* ((parts (cl-ppcre:split ":" line))
         (id (parse-game-id (string-trim " " (car parts))))
         (subsets (parse-subsets (string-trim " " (car (cdr parts))))))
    (make-game :id id :rgbs (reverse subsets))))


(defun possible-subset-p (config rgb)
  (and
   (<= (rgb-r rgb) (rgb-r config))
   (<= (rgb-b rgb) (rgb-b config))
   (<= (rgb-g rgb) (rgb-g config))))

(defun valid-game-p (config game)
  (reduce (lambda (acc x)
            (and acc (possible-subset-p config x)))
          (game-rgbs game)
          :initial-value t))

(defun power-of-game (game)
  (let ((res (reduce (lambda (acc x)
                   (make-rgb :r (max (rgb-r acc) (rgb-r x))
                             :g (max (rgb-g acc) (rgb-g x))
                             :b (max (rgb-b acc) (rgb-b x))))
                 (game-rgbs game))))
    (* (rgb-r res)
       (rgb-b res)
       (rgb-g res))))


(defun powers-of-games (input-file)
  (with-open-file (in input-file :direction :input)
    (do ((line (read-line in nil) (read-line in nil))
         (acc nil (let ((game (parse-line line)))
                    (cons (power-of-game game) acc))))
        ((null line) (return acc)))))

(defun possible-ids (input-file)
  (let ((config (make-rgb :r 12 :g 13 :b 14)))
    (with-open-file (in input-file :direction :input)
      (do ((line (read-line in nil) (read-line in nil))
           (acc nil (let ((game (parse-line line)))
                      (if (valid-game-p config game)
                          (cons (game-id game) acc)
                          acc))))
          ((null line) (return acc))
        (print line)))))
