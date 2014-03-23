(defmacro random-seq (seq-type length min max)
  `(,seq-type ,@(make-list length :initial-element `(+ ,min (random ,max)))))

(defmacro random-vector (length min max)
  `(random-seq vector ,length ,min ,max))

(defmacro random-list (length min max)
  `(random-seq list ,length ,min ,max))

