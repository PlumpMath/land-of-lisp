(defmacro list-symbols (package)
  "패키지에 포함된 모든 심볼의 정렬된 리스트를 반환한다."
  `(sort (let ((acc nil))
           (do-symbols (i ,package (nreverse acc))
             (push i acc)))
         #'string-lessp))

(defmacro rand (min max)
  "min ~ max 사이의 난수를 반환한다."
  `(+ ,min (random (- ,max ,min))))

(defmacro rand-seq (seq-type length min max)
  "length 개의 min ~ max 사이의 난수를 담은 seq-type 형의 시퀀스를 반환한다."
  `(,seq-type ,@(make-list length :initial-element `(rand ,min ,max))))

(defmacro rand-vector (length min max)
  "length 개의 min ~ max 사이의 난수를 담은 벡터를 반환한다."
  `(rand-seq vector ,length ,min ,max))

(defmacro rand-list (length min max)
  "length 개의 min ~ max 사이의 난수를 담은 리스트를 반환한다."
  `(rand-seq list ,length ,min ,max))

(defmacro memoize-equal-defun (name params &body body)
  "인자와 반환값을 메모이즈하는 함수를 정의한다. 메모이즈의 해시테이블은 #'equal을 테스트로 사용한다."
  (let ((g-memoization-table (gensym))
        (g-param-list (gensym))
        (g-memoized (gensym)))
    `(let ((,g-memoization-table (make-hash-table :test #'equal)))
       (defun ,name ,params
         (let* ((,g-param-list (list ,@params))
                (,g-memoized (gethash ,g-param-list ,g-memoization-table)))
           (if ,g-memoized
             (progn (format t "~A~&" ,g-memoization-table)
                    ,g-memoized)
             (setf (gethash ,g-param-list ,g-memoization-table)
                   ,@body)))))))
