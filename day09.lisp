(defclass computer ()
  ((memory :accessor memory :initarg :memory)
   (ip :accessor ip :initform 0)
   (base :accessor base :initform 0))
  (:documentation "An Intcode computer."))

(defun load-intcode-string (&optional (filename "input/day09.txt"))
  "Return the intcode string in the first line of the file."
  (with-open-file (stream filename :direction :input)
    (read-line stream)))

(defun make-memory (intcode-string)
  "Return memory initialized with the Intcode in the given intcode-string.
The memory is returned as a hash table where the keys are memory addresses and
the values are the integers in the string."
  (with-input-from-string (str (substitute #\space #\, intcode-string))
    (loop with memory = (make-hash-table)
          for address from 0
          for num = (read str nil nil)
          while num
          do (setf (gethash address memory) num)
          finally (return memory))))

(define-condition halt ()
  ()
  (:documentation "Indicates the Intcode program is finished and must halt."))

(defvar *parameter-modes*
  (list :position-mode ; the value is a memory address for the real value
        :immediate-mode ; the value is an immediate value
        :relative-mode) ; the value is a memory address relative to some base
  "A list of intcode parameter modes, the index into the list is the integer
value representing the mode.")

(defun read-params (computer num-params &rest write-params)
  "Return a list of the parameters from the computer's memory after the
opcode at its current ip (the instruction pointer). The parameters will be the
following num-params memory locations after the current ip. The write-params are
the parameter numbers (starting from 1) that the instructions will write to."
  (flet ((read-mem (address memory)
           (setf (gethash address memory) (or (gethash address memory) 0))))
    (loop with memory = (memory computer)
          with ip = (ip computer)
          with base = (base computer)
          for i from 1 to num-params
          for modes = (floor (gethash ip memory) 100) then (floor modes 10)
          for writep = (member i write-params)
          for mode = (elt *parameter-modes* (rem modes 10))
          for value = (read-mem (+ ip i) memory)
          collect (if writep
                      (ecase mode
                        (:position-mode value)
                        ;; it's an error to write to an immediate mode parameter
                        (:relative-mode (+ base value)))
                    (ecase mode
                      (:position-mode (read-mem value memory))
                      (:immediate-mode value)
                      (:relative-mode (read-mem (+ base value) memory)))))))

(defun opcode-1 (computer)
  "Add the first two parameters and save the result at the third."
  (let ((p (read-params computer 3 3)))
    (setf (gethash (third p) (memory computer)) (+ (first p) (second p))))
  (incf (ip computer) 4))

(defun opcode-2 (computer)
  "Multiply the first two parameters and save the result at the third."
  (let ((p (read-params computer 3 3)))
    (setf (gethash (third p) (memory computer)) (* (first p) (second p))))
  (incf (ip computer) 4))

(defun opcode-3 (computer)
  "Read input and store it at the first parameter."
  (let ((p (read-params computer 1 1)))
    (setf (gethash (first p) (memory computer)) (read)))
  (incf (ip computer) 2))

(defun opcode-4 (computer)
  "Output the value of the first parameter."
  (let ((p (read-params computer 1)))
    (format t "~D," (first p)))
  (incf (ip computer) 2))

(defun opcode-5 (computer)
  "Jump if true: if the first parameter is non-zero it sets the instruction
pointer to the value from the second parameter."
  (let ((p (read-params computer 2)))
    (setf (ip computer)
          (if (zerop (first p))
              (+ (ip computer) 3)
            (second p)))))

(defun opcode-6 (computer)
  "Jump if false: if the first parameter is zero, it sets the instruction
pointer to the value from the second parameter."
  (let ((p (read-params computer 2)))
    (setf (ip computer)
          (if (zerop (first p))
              (second p)
            (+ (ip computer) 3)))))

(defun opcode-7 (computer)
  "Less than: If the first parameter is less than the second parameter, it
stores 1 in the position given by the third parameter, otherwise 0."
  (let ((p (read-params computer 3 3)))
    (setf (gethash (third p) (memory computer))
          (if (< (first p) (second p))
              1
            0)))
  (incf (ip computer) 4))

(defun opcode-8 (computer)
  "Equals: if the first parameter is equal to the second parameter, it stores
1 in the position given by the third parameter, otherwise 0."
  (let ((p (read-params computer 3 3)))
    (setf (gethash (third p) (memory computer))
          (if (= (first p) (second p))
              1
            0)))
  (incf (ip computer) 4))

(defun opcode-9 (computer)
  "Adjust the relative base by the value of it's only parameter."
  (let ((p (read-params computer 1)))
    (incf (base computer) (first p)))
  (incf (ip computer) 2))

(defun opcode-99 (computer)
  "Halt the program, it's finished runnning."
  (declare (ignore computer))
  (error 'halt)) ; it's an error to not handle this condition

(defun run (memory)
  "Run then return the Intcode stored in memory as a vector of integers.
The mailboxes are LispWorks mailboxes which are FIFO queues that allow
communication between processes. Input is read from in-mailbox and output is
sent to out-mailbox."
  (let ((computer (make-instance 'computer :memory memory)))
    (handler-case
        (loop
         (let ((opcode (rem (gethash (ip computer) (memory computer)) 100)))
           (ecase opcode
             (1 (opcode-1 computer))
             (2 (opcode-2 computer))
             (3 (opcode-3 computer))
             (4 (opcode-4 computer))
             (5 (opcode-5 computer))
             (6 (opcode-6 computer))
             (7 (opcode-7 computer))
             (8 (opcode-8 computer))
             (9 (opcode-9 computer))
             (99 (opcode-99 computer)))))
      (halt () memory))))

(defun copy-memory (memory)
  "Return a shallow copy of the memory."
  (loop with copy = (make-hash-table)
        for address being the hash-key of memory using (hash-value value)
        do (setf (gethash address copy) value)
        finally (return copy)))

(defun run-returning-output (memory &key input)
  "Run the program using the given string input and return the string output."
  (with-input-from-string (*standard-input* input)
    (with-output-to-string (*standard-output*)
      (run (copy-memory memory)))))

(defun test-part1 ()
  "Test the examples in the problem description for part 1."
  (let* ((program "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
         (actual (run-returning-output (make-memory program) :input "")))
    (assert (string= program (subseq actual 0 (1- (length actual))))))
  (let* ((program "1102,34915192,34915192,7,4,7,99,0")
         (actual (run-returning-output (make-memory program) :input "")))
    (assert (and (= 16 (1- (length actual))))))
  (let* ((program "104,1125899906842624,99")
         (actual (run-returning-output (make-memory program) :input "")))
    (assert (string= "1125899906842624" (subseq actual 0 (1- (length actual)))))))

(defun part1 ()
  "Return the solution for part 1."
  (run-returning-output (make-memory (load-intcode-string)) :input "1"))

(defun part2 ()
  "Return the solution for part 2."
  (run-returning-output (make-memory (load-intcode-string)) :input "2"))