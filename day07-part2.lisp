;;;; In this code, we use LispWorks-specific threading functions and mailboxes
;;;; which are FIFO queues that threads can block on while reading.

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

(defun opcode-3 (memory ip in-mailbox)
  "Read input and store it at the first parameter."
  (let ((p (read-params memory ip 1 1)))
    (setf (svref memory (first p)) (mp:mailbox-read in-mailbox)))
  (+ ip 2))

(defun opcode-4 (memory ip out-mailbox)
  "Output the value of the first parameter."
  (let ((p (read-params memory ip 1)))
    (mp:mailbox-send out-mailbox (first p)))
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

(defun run (memory in-mailbox out-mailbox)
  "Run then return the Intcode stored in memory as a vector of integers.
The mailboxes are LispWorks mailboxes which are FIFO queues that allow
communication between processes. Input is read from in-mailbox and output is
sent to out-mailbox."
  (let ((ip 0)) ; instruction pointer
    (handler-case
        (loop
         (setf ip (ecase (rem (svref memory ip) 100)
                    (1 (opcode-1 memory ip))
                    (2 (opcode-2 memory ip))
                    (3 (opcode-3 memory ip in-mailbox))
                    (4 (opcode-4 memory ip out-mailbox))
                    (5 (opcode-5 memory ip))
                    (6 (opcode-6 memory ip))
                    (7 (opcode-7 memory ip))
                    (8 (opcode-8 memory ip))
                    (99 (opcode-99 memory ip)))))
      (halt () memory))))

(defun all-permutations (list)
  "Return a list of all permutations of the given list."
  (cond ((null list) ())
        ((null (rest list)) (list list))
        (t (loop for element in list
                 nconc (mapcar (lambda (l) (cons element l))
                               (all-permutations (remove element list)))))))


(defun run-amplifiers (memory phase-settings)
  "Run the five amplifiers in a feedback loop using the given phase settings.
Return the last output signal from the last amplifier after all amplifiers have
finished running. The memory must be the amplifier controller software, and the
phase settings must be a list containing the numbers 5 through 9 in any order."
  (let* ((mailboxes (loop for name in '("AB" "BC" "CD" "DE" "EA")
                          collect (mp:make-mailbox :name name)))
         (inboxes (append (last mailboxes) (butlast mailboxes)))
         (processes (loop for name in '("A" "B" "C" "D" "E")
                          for inbox in inboxes
                          for outbox in mailboxes
                          collect (mp:process-run-function
                                   name () #'run (copy-seq memory)
                                   inbox outbox))))
    (loop for setting in phase-settings
          for inbox in inboxes
          do (mp:mailbox-send inbox setting))
    (mp:mailbox-send (first inboxes) 0)
    (loop for process in processes
          do (mp:process-join process))
    (mp:mailbox-read (first inboxes))))

(defun max-thruster-signal (memory)
  "Find the maximum thrust signal using every ordering of phase settings."
  (loop for phase-settings in (all-permutations (list 5 6 7 8 9))
        maximize (run-amplifiers memory phase-settings)))

(defun test-part2 ()
  "Test the examples in the problem description for part 2."
  (assert (= 139629729
             (max-thruster-signal #(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27
                                      26 27 4 27 1001 28 -1 28 1005 28 6 99 0
                                      0 5))))
  (assert (= 18216
             (max-thruster-signal #(3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54
                                      5 55 1005 55 26 1001 54 -5 54 1105 1 12 1
                                      53 54 53 1008 54 0 55 1001 55 1 55 2 53
                                      55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0
                                      0 0 10)))))

(defun part2 ()
  "Return the solution for part 2."
  (max-thruster-signal (load-memory)))
