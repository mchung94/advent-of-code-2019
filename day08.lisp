(defun load-image (&optional (filename "input/day08.txt"))
  "Read and return the first line of image file data as a string."
  (with-open-file (stream filename :direction :input)
    (read-line stream nil nil)))

(defun get-layers (image-string width height)
  "Return a list of each layer of the image-string. Each layer is a substring."
  (loop with layer-size = (* width height)
        with image-length = (length image-string)
        with num-layers = (floor image-length layer-size)
        repeat num-layers
        for i = 0 then (+ i layer-size)
        collect (subseq image-string i (+ i layer-size))))

(defun layer-with-fewest-zeros (layers)
  "Return the layer with the fewest 0 digits."
  (let* ((min-layer (first layers))
         (min-count (count #\0 min-layer)))
    (dolist (layer (rest layers) min-layer)
      (let ((num-zeros (count #\0 layer)))
        (when (< num-zeros min-count)
          (setf min-layer layer min-count num-zeros))))))

(defun part1 ()
  "Return the solution for part 1."
  (let ((layer (layer-with-fewest-zeros (get-layers (load-image) 25 6))))
    (* (count #\1 layer)
       (count #\2 layer))))

(defun top-visible-pixel (layers x y width)
  "Return the top visible pixel in the layers."
  (let ((offset (+ (* y width) x)))
    (dolist (layer layers)
      (let ((pixel (char layer offset)))
        (unless (char= pixel #\2)
          (return-from top-visible-pixel pixel))))))

(defun image-2d (image-string width height)
  "Return the decoded image as a 2D array."
  (let ((layers (get-layers image-string width height))
        (image (make-array (list height width))))
    (dotimes (y height image)
      (dotimes (x width)
        (setf (aref image y x) (top-visible-pixel layers x y width))))))

(defun print-image (image-2d)
  "Print a 2D image. Only print the white pixels."
  (fresh-line)
  (destructuring-bind (height width) (array-dimensions image-2d)
    (dotimes (y height)
      (dotimes (x width)
        (let ((pixel (aref image-2d y x)))
          (format t "~C" (if (char= pixel #\1) pixel #\space))))
      (fresh-line))))

(defun part2 ()
  "Print the solution for part 2."
  (print-image (image-2d (load-image) 25 6)))
