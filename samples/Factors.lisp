;; Computes the list of factors in number
(let ((number 126)
      (factorize (Y (lambda (self n current)
                      (if (> current n)
                          null
                          (let ((rest (self n (+ 1 current))))
                            (if (zerop (% n current))
                                (cons current rest)
                                rest)))))))
  (factorize number 1))
