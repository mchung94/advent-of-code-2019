(defun doublep (int-string)
  "Given a six digit number as a string, return true if two adjacent digits are
the same."
  (loop for i from 0 to 4
        for ch = (char int-string i)
        thereis (char= ch (char int-string (1+ i)))))

(defun double-without-triple-p (int-string)
  "Given a six digit number as a string, return true if two adjacent digits are
the same without being part of a larger group of matching digits."
  (loop with padded = (format nil " ~A " int-string) ; eliminates edge cases
        for i from 1 to 5
        for ch = (char padded i)
        thereis (and (char= ch (char padded (1+ i)))
                     (char/= ch (char padded (1- i)))
                     (char/= ch (char padded (+ i 2))))))

(defun passwordp (integer &optional (double-function #'doublep))
  "Return true if the integer is a valid password, without the range rule.
This is the password function for part 1."
  (let* ((string (format nil "~D" integer))
         (digits (map 'list #'digit-char-p string)))
    (and (= (length string) 6)
         (apply #'<= digits)
         (funcall double-function string))))

(defun test-part1 ()
  "Test the examples in the problem description for part 1."
  (assert (passwordp 111111))
  (assert (not (passwordp 223450)))
  (assert (not (passwordp 123789))))

(defun part1 ()
  "Return the answer for part 1."
  (loop for password from 158126 to 624572
        count (passwordp password)))

(defun test-part2 ()
  "Test the examples in the problem description for part 2."
  (assert (passwordp 112233 #'double-without-triple-p))
  (assert (not (passwordp 123444 #'double-without-triple-p)))
  (assert (passwordp 111122 #'double-without-triple-p)))

(defun part2 ()
  "Return the answer for part 2."
  (loop for password from 158126 to 624572
        count (passwordp password #'double-without-triple-p)))
