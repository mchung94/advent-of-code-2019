(defun load-orbit-data (&optional (filename "input/day06.txt"))
  "Read the input file and return a hash table mapping each object (the key)
to the object it is orbiting (the value)."
  (with-open-file (stream filename :direction :input)
    (loop with mapping = (make-hash-table :test #'equal)
          for line = (read-line stream nil nil)
          while line
          do (setf (gethash (subseq line 4) mapping) (subseq line 0 3))
          finally (return mapping))))

(defun orbits (object mapping)
  "Return a list of the objects that the given object is directly and
indirectly orbiting."
  (loop for obj = (gethash object mapping) then (gethash obj mapping)
        while obj
        collect obj))

(defvar *part1-example-orbits*
  (let ((mapping (make-hash-table :test #'equal)))
    (setf (gethash "B" mapping) "COM"
          (gethash "C" mapping) "B"
          (gethash "D" mapping) "C"
          (gethash "E" mapping) "D"
          (gethash "F" mapping) "E"
          (gethash "G" mapping) "B"
          (gethash "H" mapping) "G"
          (gethash "I" mapping) "D"
          (gethash "J" mapping) "E"
          (gethash "K" mapping) "J"
          (gethash "L" mapping) "K")
    mapping)
  "The orbit example given in the problem description for part 1.")

(defun count-orbits (mapping)
  "Return the number of direct and indirect orbits in the mapping."
  (loop for object being the hash-keys of mapping
        sum (length (orbits object mapping))))
        
(defun test-part1 ()
  "Test the example given in the problem description for part 1."
  (assert (= 42 (count-orbits *part1-example-orbits*))))

(defun part1 ()
  "Return the solution for part 1."
  (count-orbits (load-orbit-data)))

(defvar *part2-example-orbits*
  (let ((mapping (make-hash-table :test #'equal)))
    (setf (gethash "B" mapping) "COM"
          (gethash "C" mapping) "B"
          (gethash "D" mapping) "C"
          (gethash "E" mapping) "D"
          (gethash "F" mapping) "E"
          (gethash "G" mapping) "B"
          (gethash "H" mapping) "G"
          (gethash "I" mapping) "D"
          (gethash "J" mapping) "E"
          (gethash "K" mapping) "J"
          (gethash "L" mapping) "K"
          (gethash "YOU" mapping) "K"
          (gethash "SAN" mapping) "I")
    mapping)
  "The orbit example given in the problem description for part 2.")

(defun common-orbiting-object (object1-orbits object2-orbits)
  "Return the first object that both object1 and object2 are orbiting.
The input parameters are the results of calling orbit on each object."
  (let ((object1-mapping (make-hash-table :test #'equal)))
    (dolist (obj object1-orbits)
      (setf (gethash obj object1-mapping) t))
    (dolist (obj object2-orbits)
      (when (gethash obj object1-mapping)
        (return-from common-orbiting-object obj)))))

(defun min-orbital-transfers (object1 object2 mapping)
  "Return the minimum number of orbital transfers required to move from the
object object1 is orbiting to the object object2 is orbiting."
  (let* ((object1-orbits (orbits object1 mapping))
         (object2-orbits (orbits object2 mapping))
         (obj (common-orbiting-object object1-orbits object2-orbits)))
    (+ (position obj object1-orbits :test #'equal)
       (position obj object2-orbits :test #'equal))))

(defun test-part2 ()
  "Test the example given in the problem description for part 2."
  (assert (= 4 (min-orbital-transfers "YOU" "SAN" *part2-example-orbits*))))

(defun part2 ()
  "Return the solution for part 2."
  (min-orbital-transfers "YOU" "SAN" (load-orbit-data)))