(defun load-program (&optional (filename "input/day02.txt"))
  "Load the Intcode from the first line of the file as a vector of integers."
  (with-open-file (stream filename :direction :input)
    (with-input-from-string (str (nsubstitute #\space #\, (read-line stream)))
      (coerce (loop for num = (read str nil nil)
                    while num
                    collect num)
              'vector))))

(defun run (memory)
  "Run then return the Intcode stored in memory as a vector of integers."
  (let ((ip 0)) ; instruction pointer
    (flet ((run-operation (function)
             (let* ((val1 (svref memory (svref memory (+ ip 1))))
                    (val2 (svref memory (svref memory (+ ip 2))))
                    (pos3 (svref memory (+ ip 3))))
               (setf (svref memory pos3) (funcall function val1 val2)))))
      (loop
       (ecase (svref memory ip)
         (1 (run-operation #'+))
         (2 (run-operation #'*))
         (99 (return-from run memory)))
       (incf ip 4)))))

(defun test-run ()
  "Test the examples given in the problem description for part 1."
  (assert (equalp #(3500 9 10 70 2 3 11 0 99 30 40 50)
                  (run (vector 1 9 10 3 2 3 11 0 99 30 40 50))))
  (assert (equalp #(2 0 0 0 99)
                  (run (vector 1 0 0 0 99))))
  (assert (equalp #(2 3 0 6 99)
                  (run (vector 2 3 0 3 99))))
  (assert (equalp #(2 4 4 5 99 9801)
                  (run (vector 2 4 4 5 99 0))))
  (assert (equalp #(30 1 1 4 2 5 6 0 99)
                  (run (vector 1 1 1 4 99 5 6 0 99)))))

(defun result (memory noun verb)
  "Return the output of the Intcode program with the given noun and verb.
The output is the value at memory address 0. This modifies memory."
  (setf (svref memory 1) noun
        (svref memory 2) verb)
  (svref (run memory) 0))

(defun part1 ()
  "Return the solution for part 1."
  ;; restore the program to the "1202 program alarm" state before running
  (result (load-program) 12 2))

(defun part2 ()
  "Return the solution for part 2."
  (let* ((memory (load-program))
         (size (length memory)))
    (dotimes (noun size)
      (dotimes (verb size)
        (when (= 19690720 (result (copy-seq memory) noun verb))
          (return-from part2 (+ (* 100 noun) verb)))))))
