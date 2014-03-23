(defun fibo-recur (n)
  (if (<= n 1)
    1
    (+ (fibo-recur (- n 2))
       (fibo-recur (- n 1)))))
