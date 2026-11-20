

(defconstant +boss+ #\!)
(defconstant +bonfire+ #\B)
(defconstant +fight+ #\F)
(defconstant +elite+ #\E)
(defconstant +shop+ #\$)
(defconstant +treasure+ #\T)
(defconstant +mystery+ #\?)
(defconstant +entrance+ #\@)

(defconstant +safe-places+ (list +entrance+ +bonfire+ +shop+ +treasure+))
(defconstant +unsafe-places+ (list +fight+ +elite+ +mystery+))

(defconstant +rooms-per-path+ 10)



(defun make-path (num-rooms path)
    (if (<= num-rooms 0)
        path
        (make-path (- num-rooms 1) (append path (list +fight+)))))

(defun fix-node (path floor-num)
    (progn
        (when (= floor-num (- +rooms-per-path+ 1))
            (setf (nth (- +rooms-per-path+ 1) path) +bonfire+))
        (when (and (> floor-num 1) (< floor-num (- +rooms-per-path+ 1)))
            (setf (nth floor-num path) (nth (random (length +unsafe-places+)) +unsafe-places+))) 
                path))


(defun fix-nodes (path index)
    (progn
        (fix-node path index)
        (if (< index +rooms-per-path+)
            (fix-nodes path (+ index 1))
            path)))

(defun print-path (path position)
    (cond
        ((= position 0)
            (format nil "  / ~{~a ~^--> ~}" path))
        ((= position 1)
            (format nil "  - ~{~a ~^--> ~}" path))
        ((= position 2)
            (format nil "  \\ ~{~a ~^--> ~}" path))
    )
)
(defun print-paths (paths)
    (progn
        (format t "   ~a \\~%" (print-path (nth 0 paths) 0))
        (format t "@  ~a -   !!!~%" (print-path (nth 1 paths) 1))
        (format t "   ~a /~%" (print-path (nth 2 paths) 2))
    )
)


(print-paths (list 
    (fix-nodes (make-path +rooms-per-path+ '()) 0)
    (fix-nodes (make-path +rooms-per-path+ '()) 0)
    (fix-nodes (make-path +rooms-per-path+ '()) 0)
))