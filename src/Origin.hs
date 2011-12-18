module Origin(origin) where

origin =
  "                                           \
  \  (id (lambda (x)                          \
  \        x))                                \
  \  (Y (lambda (f)                           \
  \       ((lambda (x)                        \
  \          (f (x x)))                       \
  \        (lambda (x)                        \
  \          (f (x x))))))                    \
  \  (not (lambda (x)                         \
  \         (== x false)))                    \
  \  (zerop (lambda (x)                       \
  \           (== x 0)))                      \
  \  (nullp (lambda (x)                       \
  \           (== x null)))                   \
  \  (map (Y (lambda (self f l)               \
  \            (if (== null l)                \
  \                null                       \
  \              (cons (f (head l))           \
  \                    (self f (tail l))))))) \
  \  (fold (Y (lambda (self f p l)            \
  \             (if (== null l)               \
  \                 p                         \
  \               (self f (f p (head l))      \
  \                     (tail l))))))         \
  \  (range (Y (lambda (self b e)             \
  \              (if (>= b e)                 \
  \                  null                     \
  \                (cons b (self (+ 1 b)      \
  \                              e))))))      \
  \"
