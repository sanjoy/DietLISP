;; Checks if `number` is prime
(let ((number 7)
      (check (Y (lambda (self n current)
                  (if (>= current (/ n 2))
                      true
                      (if (zerop (% n current))
                          false
                          (self n (+ 1 current))))))))
  (check number 2))
