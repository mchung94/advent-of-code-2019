(defclass moon ()
  ((position :accessor pos :initarg :position)
   (velocity :accessor vel :initarg :velocity))
  (:documentation "A moon represented by its position and velocity.
The position is a list of X, Y, and Z coordinate values.
The velocity is a list of X, Y, and Z direction values."))

(defun make-moon (description)
  "Return a moon instance from the string description of its position.
The descriptions look like <x=-1, y=0, z=2>."
  (flet ((read-integer (start-string end-string)
           (let* ((start-pos (+ (search start-string description)
                                (length start-string)))
                  (end-pos (search end-string description :start2 start-pos)))
             (parse-integer (subseq description start-pos end-pos)))))
    (make-instance 'moon
                   :position (list (read-integer "x=" ",")
                                   (read-integer "y=" ",")
                                   (read-integer "z=" ">"))
                   :velocity (list 0 0 0))))

(defmethod print-object ((object moon) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "pos=~A vel=~A" (pos object) (vel object))))

(defun load-moons (&optional (filename "input/day12.txt"))
  "Return a list of the moons in the file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (make-moon line))))
  
(defun apply-gravity (moons)
  "Apply gravity to update the velocity on the list of moons."
  (labels ((gravity-change (coord1 coord2)
             (cond ((> coord1 coord2) -1)
                   ((< coord1 coord2) 1)
                   (t 0)))
           (gravity-changes (pos1 pos2)
             (list (gravity-change (first pos1) (first pos2))
                   (gravity-change (second pos1) (second pos2))
                   (gravity-change (third pos1) (third pos2)))))
    (loop for sublist on moons
          for moon1 = (first sublist)
          for pos1 = (pos moon1)
          for vel1 = (vel moon1)
          do (loop for moon2 in (rest sublist)
                   for pos2 = (pos moon2)
                   for vel2 = (vel moon2)
                   for (dx dy dz) = (gravity-changes pos1 pos2)
                   do (progn
                        (incf (first vel1) dx)
                        (incf (second vel1) dy)
                        (incf (third vel1) dz)
                        (decf (first vel2) dx)
                        (decf (second vel2) dy)
                        (decf (third vel2) dz))))))

(defun apply-velocity (moons)
  "Apply velocity to update each moon's position."
  (loop for moon in moons
        for pos = (pos moon)
        for vel = (vel moon)
        do (progn
             (incf (first pos) (first vel))
             (incf (second pos) (second vel))
             (incf (third pos) (third vel)))))

(defun time-step (moons)
  "Update the moons after 1 time step."
  (apply-gravity moons)
  (apply-velocity moons))

(defun time-steps (moons num-steps)
  "Update the moons after num-steps time steps."
  (dotimes (i num-steps moons)
    (time-step moons)))

(defun potential-energy (moon)
  "Return the potential energy of the moon - the sum of the absolute values
of its x, y, and z position coordinates."
  (reduce #'+ (mapcar #'abs (pos moon))))

(defun kinetic-energy (moon)
  "Return the kinetic energy of the moon - the sum of the absolute values of
its velocity coordinates."
  (reduce #'+ (mapcar #'abs (vel moon))))

(defun total-energy (moon)
  "Return the total energy of the moon."
  (* (potential-energy moon) (kinetic-energy moon)))

(defun system-energy (moons)
  "Return the sum of the total energy of all the moons."
  (reduce #'+ (mapcar #'total-energy moons)))

(defun part1 ()
  "Return the solution for part 1."
  (system-energy (time-steps (load-moons) 1000)))