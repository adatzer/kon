;;;; kon-fun.lisp

(defpackage :kon
  (:use :common-lisp))

(in-package :kon)



;; JSON
(defun encode-array (array)
  "JSON-Encodes a 2D array."
  (let ((m (array-dimension array 0)))
    (yason:with-array ()
      (dotimes (i m)
          (yason:encode-array-element (aops:sub array i))))))

;; FILE2STRING
(defun file2string (file &key (downcase t))
  (if downcase
      (nstring-downcase (uiop:read-file-string file))
      (uiop:read-file-string file)))

(defun str2list (str delims)
  "Splits a string according to delims. Does not keep delims.
   Returns a list."
  (let ((ls (length str))
        (ld (length delims)))
    (and (not (zerop ls))
         (if (zerop ld)
             str
             (flet ((delimp (c) (position c delims))
                    (upd (lst a b)
                      (if (or (= b a) (= (1- b) a))
                          lst
                          (push (subseq str (1+ a) b) lst))))
               (do ((i ls (position-if #'delimp str :end e :from-end t))
                    (e ls i)
                    (res nil (upd res i e)))
                   ((null i) (if (= e 0)
                                 res
                                 (push (subseq str 0 e) res)))))))))

(defun deletedelims (str delims)
  (flet ((delimp (c) (position c delims)))
    (delete-if #'delimp str)))

(defun explode-str (str size &key (keep-last-if-less nil)
                                  (overlap t))
  (and (> size 0)
       (let ((ls (length str)))
         (if (= ls 0)
             nil
             (do ((i 0 (if overlap
                           (1+ i)
                           (+ i size)))
                  (res nil (cons (subseq str i (+ i size))
                                 res)))
                 ((or (= i ls)
                      (> size (- ls i)))
                  (if (= i ls)
                      (nreverse res)
                      (if keep-last-if-less
                          (nreverse (cons (subseq str i)
                                          res))
                          (nreverse res)))))))))





(defun list2vec (li)
  (make-array (length li)
              :initial-contents li))

(defun vec2ngrams (vec n fun)
  "Inputs: a vector of tokens, the size of ngrams
   and a function that maps the token to a number.
   Output: a 2D array."
  (let ((l (length vec)))
    (and (> n 0)
         (>= l n)
         (let ((array (make-array (list (1+ (- l n))
                                        n)
                                  :element-type 'real)))
           (do ((i 0 (1+ i)))
               ((> i (- l n)) array)
             (do ((j 0 (1+ j)))
                 ((= j n))
               (setf (aref array i j)
                     (funcall fun
                              (svref vec (+ i j))))))))))

;; ENCODING of TOKENS
(defun make-char-encoder (alist)
  #'(lambda (c)
      (let ((found (assoc c alist)))
        (if found
            (cdr found)
            (error "character ~S not in the alist" c)))))

(defun word2vec (word format)
  (let ((myenc (make-char-encoder format)))
    (map 'vector
         #'(lambda (k)
             (funcall myenc k))
         word)))

(defun make-token-encoder (alist transformer reducer finalizer)
  #'(lambda (token)
      (let ((myenc (make-char-encoder alist)))
        (funcall finalizer
                 (reduce reducer
                         (funcall transformer
                                  (map 'vector
                                       #'(lambda (k) (funcall myenc k))
                                       token)))))))


;; TRANSFORMERS
(defun big-endian (vec base)
  "output a vec of same dims"
  (let* ((d (array-dimension vec 0))
         (res (make-array d)))
    (do ((i 0 (1+ i))
         (expon (1- d) (1- expon)))
        ((= i d) res)
      (setf (aref res i) (* (aref vec i)
                            (expt base expon))))))

(defun little-endian (vec base)
  "output a vec of same dims"
  (let* ((d (array-dimension vec 0))
         (res (make-array d)))
    (do ((i (1- d) (1- i))
         (expon (1- d) (1- expon)))
        ((< i 0) res)
      (setf (aref res i) (* (aref vec i)
                            (expt base expon))))))

(defun big-endianess (base)
  #'(lambda (vec)
      (funcall #'big-endian vec base)))

(defun little-endianess (base)
  #'(lambda (vec)
      (funcall #'little-endian vec base)))


;; FINALIZERS
(defun natlog (base)
  #'(lambda (num)
      (funcall #'log num base)))



;; META
(defun array-min-max (arr)
  "Returns min and max of an m x n array of numbers"
  (let ((m (array-dimension arr 0))
        (n (array-dimension arr 1))
        (mini (aref arr 0 0))
        (maxi (aref arr 0 0)))
    (do ((i 0 (1+ i)))
        ((= i m) (values mini maxi))
      (do ((j 0 (1+ j)))
          ((= j n))
        (let ((elem (aref arr i j)))
          (progn
            (when (< elem mini)
              (setf mini elem))
            (when (> elem maxi)
              (setf maxi elem))))))))

(defun minmax-of (&rest arrays)
  (let ((lmm (mapcan #'(lambda (arr)
                         (multiple-value-list (array-min-max arr)))
                     arrays)))
    (cons (reduce #'min lmm)
          (reduce #'max lmm))))

(defun mapper-minmax (mini maxi)
  "Input: a minimum and a maximum value.
   Returns a function that accepts a 2D array of numbers x,
   and returns a new array of same dimensions, whose elements
   are y = (x-mini)/(maxi-mini)."
  #'(lambda (arr)
      (and (not (>= mini maxi))
           (let* ((m (array-dimension arr 0))
                  (n (array-dimension arr 1))
                  (new-array (make-array (list m n))))
             (dotimes (i m new-array)
               (dotimes (j n)
                 (setf (aref new-array i j)
                       (divide-with-precision (- (aref arr i j) mini)
                                              (- maxi mini)
                                              :precision 5))))))))  ; *****

(defun rr (arr &rest arrays-to-compare)
  "Does not change the initial arrays"
  (let ((minmax (apply #'minmax-of
                       (cons arr arrays-to-compare))))
    (funcall (mapper-minmax (car minmax)
                            (cdr minmax))
             arr)))


;; UTILITIES

(defun divide-with-precision (n d &key (precision 5))
  "uses read-from-string.."
  (and (typep precision 'integer)
       (> precision 0)
       (< precision 8)
       (read-from-string (format nil
                                 "~,vF"
                                 precision
                                 (float (/ n d))))))

(defun digits-list (n)
  (if (< n 10)
      (list n)
      (nconc (digits-list (truncate n 10))
             (list (mod n 10)))))

(defun reduce-digits (num limit)
  (if (< num limit)
      num
      (let* ((dl (digits-list num))
             (temp (reduce #'+ dl)))
        (unless (and (= (length dl) 1) (> temp limit))
          (if (< temp limit)
              temp
              (reduce-digits temp limit))))))

(defun digit-reducer (limit)
  "returns a function that accepts a number and does reduce-digits
   To be used in a compose"
  #'(lambda (n)
      (reduce-digits n limit)))

;; from Graham page 110 - ANSI Common Lisp
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (nreverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))


;; (defun chop-str (str size)
;;   "Splits string to pieces of given size. The size of last piece depends.
;;    Returns the pieces in a list"
;;   (if (<= size 0)
;;       (list str)
;;       (let ((ls (length str)))
;;         (and (not (zerop ls))
;;              (if (>= size ls)
;;                  (list str)
;;                  (cons (subseq str 0 size)
;;                        (chop-str (subseq str size)
;;                                  size)))))))

;; (defun str2tokens (str delims size)
;;   "Splits strings to tokens of given size. Does not include delims"
;;   (and (> size 0)
;;        (chop-str (apply #'uiop:strcat
;;                         (str2list str delims))
;;                  size)))
