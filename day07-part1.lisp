;;;; This code works for Part 1, but I would like to think of a different
;;;; approach to handle Part 2.

(defun load-memory (&optional (filename "input/day07.txt"))
  "Load the Intcode from the first line of the file as a vector of integers.
The vector represents the memory space of the program."
  (with-open-file (stream filename :direction :input)
    (with-input-from-string (str (nsubstitute #\space #\, (read-line stream)))
      (coerce (loop for num = (read str nil nil)
                    while num
                    collect num)
              'vector))))

(define-condition halt ()
  ()
  (:documentation "Indicates the Intcode program is finished and must halt."))

(defun read-params (memory ip num-params &rest write-params)
  "Read the parameters from memory and return them in a list.
The opcode must be at the memory location indicated by ip, the instruction
pointer. The parameters will be the following num-params memory locations.
The write-params are the parameter numbers (starting from 1) that the
instruction will write to."
  (loop for i from 1 to num-params
        for modes = (floor (svref memory ip) 100) then (floor modes 10)
        for immediate-mode-p = (= 1 (rem modes 10))
        for value = (svref memory (+ ip i))
        collect (if (or immediate-mode-p
                        (member i write-params))
                    value
                  (svref memory value))))

(defun opcode-1 (memory ip)
  "Add the first two parameters and save the result at the third."
  (let ((p (read-params memory ip 3 3)))
    (setf (svref memory (third p))
          (+ (first p) (second p))))
  (+ ip 4))

(defun opcode-2 (memory ip)
  "Multiply the first two parameters and save the result at the third."
  (let ((p (read-params memory ip 3 3)))
    (setf (svref memory (third p))
          (* (first p) (second p))))
  (+ ip 4))

(defun opcode-3 (memory ip)
  "Read input and store it at the first parameter."
  (let ((p (read-params memory ip 1 1)))
    (setf (svref memory (first p)) (read)))
  (+ ip 2))

(defun opcode-4 (memory ip)
  "Output the value of the first parameter."
  (let ((p (read-params memory ip 1)))
    (format t "~D" (first p)))
  (+ ip 2))

(defun opcode-5 (memory ip)
  "Jump if true: if the first parameter is non-zero it sets the instruction
pointer to the value from the second parameter."
  (let ((p (read-params memory ip 2)))
    (if (zerop (first p))
        (+ ip 3)
      (second p))))

(defun opcode-6 (memory ip)
  "Jump if false: if the first parameter is zero, it sets the instruction
pointer to the value from the second parameter."
  (let ((p (read-params memory ip 2)))
    (if (zerop (first p))
        (second p)
      (+ ip 3))))

(defun opcode-7 (memory ip)
  "Less than: If the first parameter is less than the second parameter, it
stores 1 in the position given by the third parameter, otherwise 0."
  (let ((p (read-params memory ip 3 3)))
    (setf (svref memory (third p))
          (if (< (first p) (second p))
              1
            0)))
  (+ ip 4))

(defun opcode-8 (memory ip)
  "Equals: if the first parameter is equal to the second parameter, it stores
1 in the position given by the third parameter, otherwise 0."
  (let ((p (read-params memory ip 3 3)))
    (setf (svref memory (third p))
          (if (= (first p) (second p))
              1
            0)))
  (+ ip 4))

(defun opcode-99 (memory ip)
  (declare (ignore memory ip))
  (error 'halt)) ; it's an error to not handle this condition

(defun run (memory)
  "Run then return the Intcode stored in memory as a vector of integers."
  (let ((ip 0)) ; instruction pointer
    (handler-case
        (loop
         (setf ip (ecase (rem (svref memory ip) 100)
                    (1 (opcode-1 memory ip))
                    (2 (opcode-2 memory ip))
                    (3 (opcode-3 memory ip))
                    (4 (opcode-4 memory ip))
                    (5 (opcode-5 memory ip))
                    (6 (opcode-6 memory ip))
                    (7 (opcode-7 memory ip))
                    (8 (opcode-8 memory ip))
                    (99 (opcode-99 memory ip)))))
      (halt () memory))))

(defun run-returning-output (memory &key input)
  "Run the program using the given string input and return the string output."
  (with-input-from-string (*standard-input* input)
    (with-output-to-string (*standard-output*)
      (run (copy-seq memory)))))

(defun test-io (memory &key input expected-output)
  "Run the program using input as input and assert that the output matches
expected-output."
  (let ((output (run-returning-output memory :input input)))
    (assert (string= expected-output output))))

(defun run-amplifiers (memory phase-settings)
  "Run the amplifier controller software on the five amplifiers connected in
series and return the resulting output signal. The memory must be the amplifier
controller software. The phase-settings must be a list of five integers, 0 to
4, representing each amplifier's phase setting."
  (let ((value "0")) ; the input and output values during the run
    (dolist (phase phase-settings)
      (let ((input (format nil "~A ~A" phase value)))
        (setf value (run-returning-output (copy-seq memory) :input input))))
    value))

(defun test-part1 ()
  "Test the examples in the problem description for part 1."
  (assert (string= "43210"
                   (run-amplifiers #(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99
                                       0 0)
                                   (list 4 3 2 1 0))))
  (assert (string= "54321"
                   (run-amplifiers #(3 23 3 24 1002 24 10 24 1002 23 -1 23 101
                                       5 23 23 1 24 23 23 4 23 99 0 0)
                                   (list 0 1 2 3 4))))
  (assert (string= "65210"
                   (run-amplifiers #(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007
                                       31 0 33 1002 33 7 33 1 33 31 31 1 32 31
                                       31 4 31 99 0 0 0)
                                   (list 1 0 4 3 2)))))

(defun all-permutations (list)
  "Return a list of all permutations of the given list."
  (cond ((null list) ())
        ((null (rest list)) (list list))
        (t (loop for element in list
                 nconc (mapcar (lambda (l) (cons element l))
                               (all-permutations (remove element list)))))))

(defun part1 ()
  "Return the solution for part 1."
  (loop with memory = (load-memory)
        for phase-settings in (all-permutations (list 0 1 2 3 4))
        maximize (parse-integer (run-amplifiers memory phase-settings))))
