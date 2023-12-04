(defun calibration-values (file)
  (with-open-file (stream file :direction :input)
    (do ((line (read-line stream nil) (read-line stream nil))
         (acc
          nil
          (cons
           (parse-integer
            (calibration-value
             (filter-digits line)))
           acc)))
        ((null line) (return acc))
      (print (parse-integer
              (calibration-value
               (filter-digits line)))))))

(defun filter-digits (input-string)
  (coerce (remove-if-not #'digit-char-p input-string) 'string))


(defun calibration-value (input-digits)
  (coerce (cons
           (char input-digits 0)
           (cons
            (char input-digits (1- (length input-digits)))
            nil))
          'string))

;(print (reduce #'+ (calibration-values "input")))

(setq words '(("one" . "1")
              ("two" . "2")
              ("three" . "3")
              ("four" . "4")
              ("five" . "5")
              ("six" . "6")
              ("seven" . "7")
              ("eight" . "8")
              ("nine" . "9")
              ("1" . "1")
              ("2" . "2")
              ("3" . "3")
              ("4" . "4")
              ("5" . "5")
              ("6" . "6")
              ("7" . "7")
              ("8" . "8")
              ("9" . "9")))

(setq reverse-words (map 'list
                         (lambda (pair)
                           (let ((word (car pair))
                                 (str (cdr pair)))
                             (cons (reverse word) str)))
                         words))

(defun calibration-values-1 (input)
  (with-open-file (in input :direction :input)
    (do ((line (read-line in nil)
               (read-line in nil))
         (acc nil
              (cons (calibration-value-1 line)
                    acc)))
        ((null line) (return acc)))))

(defun calibration-value-1 (line)
  (parse-integer (extract-read line)))

(defun extract-read (line)
  (let ((ten (find-target-digit line words #'min-by))
        (one (find-target-digit (reverse line) reverse-words #'min-by)))
    (concatenate 'string (cdr ten) (cdr one))))

(defun max-by (x y)
  (cond
    ((null (car x)) y)
    ((null (car y)) x)
    ((> (car x) (car y)) x)
    (t y)))

(defun min-by (x y)
  (cond
    ((null (car x)) y)
    ((null (car y)) x)
    ((< (car x) (car y)) x)
    (t y)))


(defun find-target-digit (input-string dictionary f)
  (let ((results (map 'list
                      (lambda (pair)
                        (let ((target (car pair))
                              (digit (cdr pair)))
                          (cons (search target input-string)
                                digit)))
                      dictionary)))
    (reduce f results)))


