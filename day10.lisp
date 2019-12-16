(defun read-lines (&optional (filename "input/day10.txt"))
  "Return a list containing every line from the file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(deftype asteroid ()
  "Asteroids are represented as conses containing (x . y) coordinates."
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

(defun direction (asteroid1 asteroid2)
  "Return a (dx . dy) cons containing the simplified difference in x and y
coordinates between the two asteroids. Vertical and horizontal directions will
be represented with conses like (-1 . 0), (1 . 0), (0 . -1), or (0 . 1).
Repeatedly adding the dx and dy values to asteroid1's x and y will eventually
lead to asteroid2. This is different than the slope, since it takes into
account increasing or decreasing coordinates."
  (let ((dx (- (car asteroid2) (car asteroid1)))
        (dy (- (cdr asteroid2) (cdr asteroid1))))
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

(defun test-part1-answer (best-asteroid expected-asteroid expected-detectable-size)
  (assert (equal expected-asteroid (car best-asteroid)))
  (assert (= expected-detectable-size (length (cdr best-asteroid)))))

(defun test-part1 ()
  "Test the examples in the problem description for part 1."
  (test-part1-answer
   (best-asteroid (make-asteroids (list ".#..#"
                                        "....."
                                        "#####"
                                        "....#"
                                        "...##")))
   (cons 3 4) 8)
  (test-part1-answer
   (best-asteroid (make-asteroids (list "......#.#."
                                        "#..#.#...."
                                        "..#######."
                                        ".#.#.###.."
                                        ".#..#....."
                                        "..#....#.#"
                                        "#..#....#."
                                        ".##.#..###"
                                        "##...#..#."
                                        ".#....####")))
   (cons 5 8) 33)
  (test-part1-answer
   (best-asteroid (make-asteroids (list "#.#...#.#."
                                        ".###....#."
                                        ".#....#..."
                                        "##.#.#.#.#"
                                        "....#.#.#."
                                        ".##..###.#"
                                        "..#...##.."
                                        "..##....##"
                                        "......#..."
                                        ".####.###.")))
   (cons 1 2) 35)
  (test-part1-answer
   (best-asteroid (make-asteroids (list ".#..#..###"
                                        "####.###.#"
                                        "....###.#."
                                        "..###.##.#"
                                        "##.##.#.#."
                                        "....###..#"
                                        "..#.#..#.#"
                                        "#..#.#.###"
                                        ".##...##.#"
                                        ".....#.#..")))
   (cons 6 3) 41)
  (test-part1-answer
   (best-asteroid (make-asteroids (list ".#..##.###...#######"
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
   (cons 11 13) 210))

(defun part1 ()
  "Return the solution to part 1."
  (length (cdr (best-asteroid (make-asteroids)))))