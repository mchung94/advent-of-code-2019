(defun fuel-to-launch (module-mass)
  "Return the fuel required to launch a module based on its mass."
  (- (floor module-mass 3) 2))

(defun read-masses (&optional (filename "input/day01.txt"))
  "Return a list containing the mass of each module."
  (with-open-file (stream filename :direction :input)
    (loop for num = (read stream nil nil)
          while num
          collect num)))

(defun part1 ()
  "Return the sum of the fuel requirements for all modules on the spacecraft."
  (reduce #'+ (mapcar #'fuel-to-launch (read-masses))))

(defun test-fuel-to-launch ()
  "Test the examples given in the problem description for the first half."
  (assert (= 2 (fuel-to-launch 12)))
  (assert (= 2 (fuel-to-launch 14)))
  (assert (= 654 (fuel-to-launch 1969)))
  (assert (= 33583 (fuel-to-launch 100756))))

(defun total-fuel (fuel)
  "Return the fuel plus the amount of fuel to launch with the given fuel."
  (+ fuel
     (loop for extra = (fuel-to-launch fuel) then (fuel-to-launch extra)
           while (> extra 0)
           sum extra)))

(defun total-fuel-to-launch (module-mass)
  "Return the fuel to launch the module plus the fuel to launch the fuel."
  (total-fuel (fuel-to-launch module-mass)))

(defun part2 ()
  "Return the sum of fuel requirements for all modules plus added fuel."
  (reduce #'+ (mapcar #'total-fuel-to-launch (read-masses))))

(defun test-total-fuel ()
  "Test the examples given in the problem decription for the second half."
  (assert (= 2 (total-fuel 2)))
  (assert (= (+ 654 216 70 21 5) (total-fuel 654)))
  (assert (= (+ 33583 11192 3728 1240 411 135 43 12 2) (total-fuel 33583))))
