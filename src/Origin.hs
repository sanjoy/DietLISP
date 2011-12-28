module Origin(origin) where

origin = "\
\  (defun not (x)                       \
\    (== x false))                      \
\                                       \
\  (defun zerop (x)                     \
\    (== x 0))                          \
\                                       \
\  (defun nullp (x)                     \
\    (== x null))                       \
\                                       \
\  (defun map (f list)                  \
\    (if (nullp list)                   \
\        null                           \
\      (cons (f (head list))            \
\            (map f (tail list)))))     \
\                                       \
\  (defun fold (f a list)               \
\    (if (nullp list)                   \
\        a                              \
\      (fold f (f a (head list))        \
\            (tail list))))             \
\                                       \
\  (defun range (begin end)             \
\    (if (>= begin end)                 \
\        null                           \
\      (cons begin (range (+ 1 begin)   \
\                         end))))       \
\"
