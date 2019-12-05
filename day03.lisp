(defun split (string)
  "Return a list of the substrings in string, separated by commas."
  (do* ((substrings ())
        (start 0 (1+ end))
        (end (position #\, string) (position #\, string :start start)))
       ((not end) (nreverse (cons (subseq string start) substrings)))
    (push (subseq string start end) substrings)))

(defun direction (direction-character)
  "Return a (delta-x . delta-y) cons representing the direction the wire goes.
The direction-character must be one of the following: U, D, L, R."
  (cons (case direction-character
          (#\L -1)
          (#\R 1)
          (otherwise 0))
        (case direction-character
          (#\D -1)
          (#\U 1)
          (otherwise 0))))

(deftype coordinates ()
  "Coordinates are conses containing x and y integer values."
  '(cons integer integer))

(defun wire-path (path-string)
  "Return a list of (x . y) coordinates tracing the path of the wire.
The wire starts at (0 . 0) but that's not included in the return value."
  (loop with x = 0
        with y = 0
        with coords = ()
        for string in (split path-string)
        for (x-dir . y-dir) = (direction (char string 0))
        for length = (parse-integer string :start 1)
        do (dotimes (i length)
             (incf x x-dir)
             (incf y y-dir)
             (push (cons x y) coords))
        finally (return (nreverse coords))))

(defun intersections (wire-path-1 wire-path-2)
  "Return all coordinates of intersections between the two wire paths.
The central port at coordinates (0 . 0) are excluded from the results.
The standard function intersection would work but it's too slow."
  (let ((wire-1-coordinates (make-hash-table :test #'equal)))
    (dolist (coordinates wire-path-1)
      (setf (gethash coordinates wire-1-coordinates) t))
     (loop for coordinates in wire-path-2
           when (gethash coordinates wire-1-coordinates)
           collect coordinates)))

(defun distance-to-closest-intersection (path-string-1 path-string-2)
  "Return the Manhattan distance from the central port to the closest
intersection between the two wires."
  (let ((path1 (wire-path path-string-1))
        (path2 (wire-path path-string-2)))
    (flet ((distance (coordinates)
             (+ (abs (car coordinates)) (abs (cdr coordinates)))))
      (reduce #'min (mapcar #'distance (intersections path1 path2))))))

(defun test-part1 ()
  "Test the examples in the problem description in part 1."
  (assert (= 6 (distance-to-closest-intersection
                "R8,U5,L5,D3"
                "U7,R6,D4,L4")))
  (assert (= 159 (distance-to-closest-intersection
                  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                  "U62,R66,U55,R34,D71,R55,D58,R83")))
  (assert (= 135 (distance-to-closest-intersection
                  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))

(defun part1 (&optional (filename "input/day03.txt"))
  "Return the answer for part 1."
  (with-open-file (stream filename :direction :input)
    (distance-to-closest-intersection (read-line stream) (read-line stream))))

(defun minimum-path-distance (path-string-1 path-string-2)
  "Return the distance from the central port to the closest intersection of the
two wires, where the distance is the sum of the number of steps along each wire
from the central port to the intersection. The closest intersection is the one
with the minimum distance."
  (let ((path1 (wire-path path-string-1))
        (path2 (wire-path path-string-2)))
    (flet ((distance (coordinates)
             (+ (1+ (position coordinates path1 :test #'equal))
                (1+ (position coordinates path2 :test #'equal)))))
      (reduce #'min (mapcar #'distance (intersections path1 path2))))))

(defun test-part2 ()
  "Test the examples in the problem description in part 2."
  (assert (= 30 (minimum-path-distance
                 "R8,U5,L5,D3"
                 "U7,R6,D4,L4")))
  (assert (= 610 (minimum-path-distance
                  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                  "U62,R66,U55,R34,D71,R55,D58,R83")))
  (assert (= 410 (minimum-path-distance
                  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))

(defun part2 (&optional (filename "input/day03.txt"))
  (with-open-file (stream filename :direction :input)
    (minimum-path-distance (read-line stream) (read-line stream))))

