(defvar *grid* '(
    (#\0 #\1 #\2)
    (#\3 #\4 #\5)
    (#\6 #\7 #\8)
))

(defvar *turn-symbol* #\#)

(defun show (col row)
    (if (< col 3)
        (progn
            (princ (nth col (nth row *grid*)))
            (if (< col 2)
                (princ "|")
            )
            (show (+ col 1) row)
        )
        (if (< row 2)
            (progn
                (if (< row 2)
                    (format t "~%-----~%")
                )
                (show 0 (+ row 1))
            )
        )
    )
    
)

(defun change-cell (cellnum val)
    (setf 
        (nth (mod cellnum 3) (nth (floor cellnum 3) *grid*))
        val
    )
)

(defun get-cell (cellnum)
    (nth (mod cellnum 3) (nth (floor cellnum 3) *grid*))
)

(defun take-turn ()
    (progn
        (format t "~%~%What cell do you want?~%~%")
        (finish-output)
        (let ((choice (read)))
            (if (or 
                    (char= (get-cell choice) #\#)
                    (char= (get-cell choice) #\!)
                )
                (progn
                    (format t "~%~%That cell is taken, goober! Pick an empty one.~%~%")
                    (take-turn)
                )
                (progn
                    (change-cell choice *turn-symbol*)
                    (show 0 0)
                    (next-turn)
                )
            )
        )
    )
)

(defun check-victory ()
    (or
        (some #'horizontal-victory'(1 4 7))
        (some #'vertical-victory'(3 4 5))
        (some #'diagonal-victory-08'(4))
        (some #'diagonal-victory-26'(4))
    )
)

(defun horizontal-victory (cellnum) 
    (char= 
        (get-cell cellnum) 
        (get-cell (+ cellnum 1)) 
        (get-cell (- cellnum 1))
    )
)

(defun vertical-victory (cellnum) 
    (char= 
        (get-cell cellnum) 
        (get-cell (+ cellnum 3)) 
        (get-cell (- cellnum 3))
    )
)

(defun diagonal-victory-08 (cellnum) 
    (char= 
        (get-cell cellnum) 
        (get-cell (+ cellnum 4)) 
        (get-cell (- cellnum 4))
    )
)

(defun diagonal-victory-26 (cellnum) 
    (char= 
        (get-cell cellnum) 
        (get-cell (+ cellnum 2)) 
        (get-cell (- cellnum 2))
    )
)

(defun next-turn ()
    (if (char= *turn-symbol* #\#)
        (setf *turn-symbol* #\!)
        (setf *turn-symbol* #\#)
    )
)

(defun game-loop ()
    (progn
        (take-turn)
        (if (check-victory)
            *turn-symbol*
            (game-loop)
        )
    )
)

(defun victory-handler ()
    (format t "~%~%Victory by ~A!~%~%" (game-loop))
)

(show 0 0)
(victory-handler)
