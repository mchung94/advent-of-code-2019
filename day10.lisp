(defun read-lines (&optional (filename "input/day10.txt"))
  "Return a list containing every line from the file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(deftype coordinates ()
  "Coordinates are represented as conses containing (x . y) values.
Points, asteroids, and the location of the monitoring station are represented
by their coordinates."
  '(cons integer integer))

(defun make-asteroids (&optional (lines (read-lines)))
  "Return a list of asteroids in the list of strings."
  (loop for line in lines
        for y from 0
        nconc (loop for char across line
                    for x from 0
                    when (char= char #\#)
                    collect (cons x y))))

(defun asteroid-hash-table (asteroids)
  "Convert a list of asteroids into a hash table where the asteroids are keys."
  (loop with mapping = (make-hash-table :test #'equal)
        for asteroid in asteroids
        do (setf (gethash asteroid mapping) t)
        finally (return mapping)))

(deftype direction ()
  "Directions are represented by conses containing (dx . dy) values."
  ;; This happens to have the same definition as coordinates but they aren't
  ;; related types.
  '(cons integer integer))

(defun direction (coordinates1 coordinates2)
  "Return a (dx . dy) cons containing the simplified difference in x and y
values between the two coordinates. Vertical and horizontal directions will
be represented with conses like (-1 . 0), (1 . 0), (0 . -1), or (0 . 1).
Repeatedly adding the dx and dy values to coordinates1's x and y will
eventually lead to coordinates2. This is different than the slope, since it
takes into account increasing or decreasing coordinates."
  (let ((dx (- (car coordinates2) (car coordinates1)))
        (dy (- (cdr coordinates2) (cdr coordinates1))))
    (cond ((zerop dx) (cons 0 (signum dy)))
          ((zerop dy) (cons (signum dx) 0))
          (t (let ((gcd (gcd dx dy)))
               (cons (/ dx gcd) (/ dy gcd)))))))

(defun line-of-sight (asteroid1 asteroid2)
  "Return a list of the coordinates in between the two asteroids, not including
the asteroids themselves."
  (loop with (x . y) = asteroid1
        with (dx . dy) = (direction asteroid1 asteroid2)
        for asteroid = (cons (incf x dx) (incf y dy))
        until (equal asteroid asteroid2)
        collect asteroid))

(defun detectablep (asteroid1 asteroid2 asteroid-hash-table)
  "Return true if asteroid1 has direct line of sight to asteroid2."
  (loop for coordinates in (line-of-sight asteroid1 asteroid2)
        never (gethash coordinates asteroid-hash-table)))

(defun detectable-asteroids (asteroid asteroid-hash-table)
  "Return a list of the asteroids detectable from the given asteroid."
  (loop for a being the hash-keys of asteroid-hash-table
        when (and (not (equal a asteroid))
                  (detectablep asteroid a asteroid-hash-table))
        collect a))

(defun maximize (list key-function)
  "Return the element of the list that has the largest value of key-function
called on each element. Return NIL if the list is empty."
  (if (null list)
      nil
    (loop with best = (first list)
          with best-value = (funcall key-function best)
          for element in (rest list)
          for value = (funcall key-function element)
          when (> value best-value)
          do (setf best element
                   best-value value)
          finally (return best))))

(defun best-asteroid (asteroids)
  "Return a (asteroid . detectable-asteroids) cons of the asteroid that can
detect the most other asteroids, and a list of the asteroids it can detect.
There must be at least one asteroid in the list."
  (maximize (loop with ht = (asteroid-hash-table asteroids)
                  for asteroid in asteroids
                  collect (cons asteroid (detectable-asteroids asteroid ht)))
            (lambda (cons) (length (cdr cons)))))


(defvar *example1* (make-asteroids (list ".#..#"
                                         "....."
                                         "#####"
                                         "....#"
                                         "...##")))

(defvar *example2* (make-asteroids (list "......#.#."
                                         "#..#.#...."
                                         "..#######."
                                         ".#.#.###.."
                                         ".#..#....."
                                         "..#....#.#"
                                         "#..#....#."
                                         ".##.#..###"
                                         "##...#..#."
                                         ".#....####")))

(defvar *example3* (make-asteroids (list "#.#...#.#."
                                         ".###....#."
                                         ".#....#..."
                                         "##.#.#.#.#"
                                         "....#.#.#."
                                         ".##..###.#"
                                         "..#...##.."
                                         "..##....##"
                                         "......#..."
                                         ".####.###.")))

(defvar *example4* (make-asteroids (list ".#..#..###"
                                         "####.###.#"
                                         "....###.#."
                                         "..###.##.#"
                                         "##.##.#.#."
                                         "....###..#"
                                         "..#.#..#.#"
                                         "#..#.#.###"
                                         ".##...##.#"
                                         ".....#.#..")))

(defvar *example5* (make-asteroids (list ".#..##.###...#######"
                                         "##.############..##."
                                         ".#.######.########.#"
                                         ".###.#######.####.#."
                                         "#####.##.#.##.###.##"
                                         "..#####..#.#########"
                                         "####################"
                                         "#.####....###.#.#.##"
                                         "##.#################"
                                         "#####.##.###..####.."
                                         "..######..##.#######"
                                         "####.##.####...##..#"
                                         ".#####..#.######.###"
                                         "##...#.##########..."
                                         "#.##########.#######"
                                         ".####.#.###.###.#.##"
                                         "....##.##.###..#####"
                                         ".#.#.###########.###"
                                         "#.#.#.#####.####.###"
                                         "###.##.####.##.#..##")))


(defun test-part1-answer (best-asteroid expected-asteroid expected-detectable-size)
  (assert (equal expected-asteroid (car best-asteroid)))
  (assert (= expected-detectable-size (length (cdr best-asteroid)))))

(defun test-part1 ()
  "Test the examples in the problem description for part 1."
  (test-part1-answer (best-asteroid *example1*) (cons 3 4) 8)
  (test-part1-answer (best-asteroid *example2*) (cons 5 8) 33)
  (test-part1-answer (best-asteroid *example3*) (cons 1 2) 35)
  (test-part1-answer (best-asteroid *example4*) (cons 6 3) 41)
  (test-part1-answer (best-asteroid *example5*) (cons 11 13) 210))

(defun part1 ()
  "Return the solution to part 1."
  (length (cdr (best-asteroid (make-asteroids)))))

;;;; Part 2

(defun angle (direction)
  "Return a direction using a double-float number representing degrees,
where straight up is 0, right is 90, down is 180, left is 270.
This will break down when the direction values are very large, due to floating
point calculations."
  (let* ((x (float (car direction) 0d0))
         (y (- (float (cdr direction) 0d0)))
         (radians (atan y x))
         (degrees (* radians (/ 180d0 pi))))
    (cond ((<= 0d0 degrees 90d0) (abs (- degrees 90d0)))
          ((and (< 90d0 degrees) (<= degrees 180d0)) (- 450 degrees))
          (t (+ (abs degrees) 90d0)))))

(defun angle-to-asteroids-table (asteroids laser-coordinates)
  "Return a hash table where the key is a direction and the value is a list of
asteroids in the same direction from the laser."
  (loop with table = (make-hash-table :test #'equal)
        for asteroid in asteroids
        for angle = (angle (direction laser-coordinates asteroid))
        do (push asteroid (gethash angle table))
        finally (return table)))
                                   
(defun asteroid-vaporized-list (asteroids laser-coordinates)
  "Return a list of the asteroids in the order they are destroyed by the laser."
  (let ((table (angle-to-asteroids-table asteroids laser-coordinates)))
    (labels ((distance (asteroid)
               "Return the Manhattan distance from the laser to the asteroid."
               (+ (abs (- (car asteroid) (car laser-coordinates)))
                  (abs (- (cdr asteroid) (cdr laser-coordinates))))))
      (let* ((angle-plus-asteroids (loop for angle being the hash-keys of table
                                         using (hash-value asteroids)
                                         collect (cons angle (sort asteroids #'< :key #'distance))))
             (sorted-by-angle (sort angle-plus-asteroids #'< :key #'car))
             (vector-of-asteroid-lists (map 'vector #'rest sorted-by-angle))
             (len (length vector-of-asteroid-lists)))
        (loop until (every #'null vector-of-asteroid-lists)
              nconc (loop for i from 0 below len
                          for asteroid = (pop (svref vector-of-asteroid-lists i))
                          when asteroid
                          collect it))))))

(defun test-part2 ()
  "Test the example from the problem description for part 2."
  (let ((vec (coerce (asteroid-vaporized-list *example5* '(11 . 13)) 'vector)))
    (assert (equal (svref vec (1- 1)) '(11 . 12)))
    (assert (equal (svref vec (1- 2)) '(12 . 1)))
    (assert (equal (svref vec (1- 3)) '(12 . 2)))
    (assert (equal (svref vec (1- 10)) '(12 . 8)))
    (assert (equal (svref vec (1- 20)) '(16 . 0)))
    (assert (equal (svref vec (1- 50)) '(16 . 9)))
    (assert (equal (svref vec (1- 100)) '(10 . 16)))
    (assert (equal (svref vec (1- 199)) '(9 . 6)))
    (assert (equal (svref vec (1- 200)) '(8 . 2)))
    (assert (equal (svref vec (1- 201)) '(10 . 9)))
    (assert (equal (svref vec (1- 300)) '(11 . 1)))))

(defun part2 ()
  "Return the solution for part 2."
  (destructuring-bind (x . y)
      (elt (asteroid-vaporized-list (make-asteroids) '(22 . 25)) (1- 200))
    (+ (* x 100) y)))
         

    