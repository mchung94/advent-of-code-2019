(defun load-memory (&optional (filename "input/day05.txt"))
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
    (format t "~D~%" (first p)))
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

(defun test-part1 ()
  "Test the example given in the problem description for part 1."
  (assert (equalp #(1002 4 3 4 99)
                  (run (vector 1002 4 3 4 33)))))

(defun run-returning-output (memory &key input)
  "Run the program using the given string input and return the string output."
  (let ((*standard-input* (make-string-input-stream input))
        (*standard-output* (make-string-output-stream)))
    (run (copy-seq memory))
    (get-output-stream-string *standard-output*)))

(defun part1 ()
  "Return the solution for part 1."
  (run-returning-output (load-memory) :input "1"))

(defun test-io (memory &key input expected-output)
  "Run the program using input as input and assert that the output matches
expected-output."
  (let ((output (run-returning-output memory :input input)))
    (assert (string= expected-output (string-trim #(#\Newline) output)))))

(defun test-part2 ()
  "Test the examples given in the problem description for part 2."
  ;; output 1 if equal to 8, otherwise 0
  (let ((memory #(3 9 8 9 10 9 4 9 99 -1 8)))
    (test-io memory :input "8" :expected-output "1")
    (test-io memory :input "7" :expected-output "0")
    (test-io memory :input "9" :expected-output "0"))
  ;; output 1 if less than 8, otherwise 0
  (let ((memory #(3 9 7 9 10 9 4 9 99 -1 8)))
    (test-io memory :input "8" :expected-output "0")
    (test-io memory :input "7" :expected-output "1")
    (test-io memory :input "9" :expected-output "0"))
  ;; output 1 if equal to 8, otherwise 0
  (let ((memory #(3 3 1108 -1 8 3 4 3 99)))
    (test-io memory :input "8" :expected-output "1")
    (test-io memory :input "7" :expected-output "0")
    (test-io memory :input "9" :expected-output "0"))
  ;; output 1 if less than 8, otherwise 0
  (let ((memory #(3 3 1107 -1 8 3 4 3 99)))
    (test-io memory :input "8" :expected-output "0")
    (test-io memory :input "7" :expected-output "1")
    (test-io memory :input "9" :expected-output "0"))
  ;; output 0 if the input is 0, or 1 if non-zero
  (let ((memory #(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)))
    (test-io memory :input "0" :expected-output "0")
    (test-io memory :input "1" :expected-output "1"))
  (let ((memory #(3 3 1105 -1 9 1101 0 0 12 4 12 99 1)))
    (test-io memory :input "0" :expected-output "0")
    (test-io memory :input "1" :expected-output "1"))
  ;; output 999 if the input is < 8, 1000 if = 8, or 1001 if > 8
  (let ((memory #(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                    1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                    999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)))
    (test-io memory :input "7" :expected-output "999")
    (test-io memory :input "8" :expected-output "1000")
    (test-io memory :input "9" :expected-output "1001")))
    
(defun part2 ()
  "Return the solution for part 2."
  (run-returning-output (load-memory) :input "5"))
