;;;; kon-params.lisp

(in-package :kon)



(defparameter *delims*
  '(#\Return #\Linefeed #\Space #\Tab #\Sub
    #\_ #\- #\' #\" #\[ #\] #\( #\) #\+ #\=
    #\. #\, #\? #\! #\: #\; #\< #\> #\` #\~
    #\$ #\% #\^ #\& #\* #\{ #\} #\| #\\ #\/
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\U+2019 #\U+2015 #\U+2014 #\U+2013 #\U+2026
    #\Greek_Tonos #\Greek_Ano_Teleia #\Middle_Dot
    #\Left-Pointing_Double_Angle_Quotation_Mark
    #\Right-Pointing_Double_Angle_Quotation_Mark))
(defparameter *delims-keep-space*
  '(#\Return #\Linefeed #\Tab #\Sub
    #\_ #\- #\' #\" #\[ #\] #\( #\) #\+ #\=
    #\. #\, #\? #\! #\: #\; #\< #\> #\` #\~
    #\$ #\% #\^ #\& #\* #\{ #\} #\| #\\ #\/
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))


;; ALPHABETS
(defparameter *english-low-alist*
  '((#\Space . 0)
    (#\a . 3)
    (#\Latin_Small_Letter_A_With_Grave . 3)
    (#\Latin_Small_Letter_A_With_Macron . 3)
    (#\Latin_Small_Letter_Ae . 3)
    (#\b . 20)
    (#\c . 12)
    (#\d . 11)
    (#\e . 1)
    (#\Latin_Small_Letter_E_With_Acute . 1)
    (#\Latin_Small_Letter_E_With_Grave . 1)
    (#\f . 15)
    (#\g . 17)
    (#\h . 9)
    (#\i . 5)
    (#\j . 24)
    (#\k . 22)
    (#\l . 10)
    (#\m . 14)
    (#\n . 6)
    (#\o . 4)
    (#\p . 16)
    (#\q . 25)
    (#\r . 8)
    (#\s . 7)
    (#\t . 2)
    (#\u . 13)
    (#\v . 21)
    (#\w . 18)
    (#\x . 23)
    (#\y . 19)
    (#\z . 26)
    (#\@ . 27)))
(defparameter *english-base* 28)

(defparameter *greek-low-alist*
  '((#\Space . 0)
    (#\α . 1)
    (#\ά . 1)
    (#\β . 21)
    (#\γ . 17)
    (#\δ . 16)
    (#\ε . 4)
    (#\έ . 4)
    (#\ζ . 23)
    (#\η . 8)
    (#\ή . 8)
    (#\θ . 19)
    (#\ι . 3)
    (#\ί . 3)
    (#\Greek_Small_Letter_Iota_With_Dialytika . 3)
    (#\Greek_Small_Letter_Iota_With_Dialytika_And_Tonos . 3)
    (#\κ . 12)
    (#\λ . 14)
    (#\μ . 13)
    (#\ν . 7)
    (#\ξ . 22)
    (#\ο . 2)
    (#\ό . 2)
    (#\π . 11)
    (#\ρ . 10)
    (#\σ . 6)
    (#\Greek_Small_Letter_Final_Sigma . 6)
    (#\τ . 5)
    (#\υ . 9)
    (#\ύ . 9)
    (#\φ . 20)
    (#\χ . 18)
    (#\ψ . 24)
    (#\ω . 15)
    (#\ώ . 15)
    (#\@ . 25)))
(defparameter *greek-base* 26)


;; FILES
(defparameter *rand-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/a.txt"))
(defparameter *austen-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/austen-sense.txt"))
(defparameter *hume-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/hume-enquiry.txt"))
(defparameter *milton-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/milton-paradise.txt"))
(defparameter *nietz-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/nietzsche.txt"))
(defparameter *whit-text*
  (file2string "/home/ada/common-lisp/ada/kon/data/whitman-leaves.txt"))
(defparameter *poems-eng*
  (file2string "/home/ada/common-lisp/ada/kon/data/poems-eng.txt"))
(defparameter *poems-gr*
  (file2string "/home/ada/common-lisp/ada/kon/data/poems-gr.txt"))


;; TXTs --no-space --no-words
(defparameter *rand-txt-no*
  (deletedelims *rand-text*
                *delims*))
(defparameter *austen-txt-no*
  (deletedelims *austen-text*
                *delims*))
(defparameter *hume-txt-no*
  (deletedelims *hume-text*
                *delims*))
(defparameter *milton-txt-no*
  (deletedelims *milton-text*
                *delims*))
(defparameter *nietz-txt-no*
  (deletedelims *nietz-text*
                *delims*))
(defparameter *whit-txt-no*
  (deletedelims *whit-text*
                *delims*))
(defparameter *poems-eng-no*
  (deletedelims *poems-eng*
                *delims*))
(defparameter *poems-gr-no*
  (deletedelims *poems-gr*
                *delims*))


;; ========================================================================
;; ======================= NO WORDS =======================================
;; ========================================================================
;; =============== A, B, C, D, E, F, G, H, I, J ===========================
;; ========================================================================

;; =====
;; WHITMAN VS RANDOM, 1char, no-space, big-endian(makes no diff), id, 1/A
;; =====
(defparameter *whit-toklist-a*
  (explode-str *whit-txt-no*
               1
               :keep-last-if-less nil
               :overlap t))
(defparameter *rand-toklist-a*
  (explode-str *rand-txt-no*
               1
               :keep-last-if-less nil
               :overlap t))

(defparameter *whit-tokvec-a*
  (list2vec *whit-toklist-a*))
(defparameter *rand-tokvec-a*
  (list2vec *rand-toklist-a*)) ;-------

(defparameter *whit-ngrams-a*
  (vec2ngrams *whit-tokvec-a*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-a*
  (vec2ngrams *rand-tokvec-a*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *whit-rand-a*
  (rr *whit-ngrams-a* *rand-ngrams-a*))
(defparameter *rand-whit-a*
  (rr *rand-ngrams-a* *whit-ngrams-a*))



;; =====
;; MILTON VS RANDOM, triplets of 3grams, no-space, big-endian, id, 2/B
;; =====
(defparameter *milton-toklist-b*
  (explode-str *milton-txt-no*
               3
               :keep-last-if-less nil
               :overlap t))
(defparameter *rand-toklist-b*
  (explode-str *rand-txt-no*
               3
               :keep-last-if-less nil
               :overlap t))

(defparameter *milton-tokvec-b*
  (list2vec *milton-toklist-b*))
(defparameter *rand-tokvec-b*
  (list2vec *rand-toklist-b*))  ;-------

(defparameter *milton-ngrams-b*
  (vec2ngrams *milton-tokvec-b*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-b*
  (vec2ngrams *rand-tokvec-b*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))  ; also for 15/O

(defparameter *milton-rand-b*
  (rr *milton-ngrams-b* *rand-ngrams-b*))
(defparameter *rand-milton-b*
  (rr *rand-ngrams-b* *milton-ngrams-b*))


;; ;; =====
;; ;; MILTON VS RANDOM, triplets of 3grams, no-space, little-endian, id, 3/C
;; ;; =====
;; ;; (defparameter *milton-toklist-b*
;; ;;   (explode-str *milton-txt-no*
;; ;;                3
;; ;;                :keep-last-if-less nil
;; ;;                :overlap t))
;; ;; (defparameter *rand-toklist-b*
;; ;;   (explode-str *rand-txt-no*
;; ;;                3
;; ;;                :keep-last-if-less nil
;; ;;                :overlap t))

;; ;; (defparameter *milton-tokvec-b*
;; ;;   (list2vec *milton-toklist-b*))
;; ;; (defparameter *rand-tokvec-b*
;; ;;   (list2vec *rand-toklist-b*)) ---------

(defparameter *milton-ngrams-c*
  (vec2ngrams *milton-tokvec-b*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-c*
  (vec2ngrams *rand-tokvec-b*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *milton-rand-c*
  (rr *milton-ngrams-c* *rand-ngrams-c*))
(defparameter *rand-milton-c*
  (rr *rand-ngrams-c* *milton-ngrams-c*))



;; ;; =====
;; ;; HUME VS RANDOM, triplets of 5grams, no-space, big-endian, id, 4/D
;; ;; =====
(defparameter *hume-toklist-d*
  (explode-str *hume-txt-no*
               5
               :keep-last-if-less nil
               :overlap t))
(defparameter *rand-toklist-d*
  (explode-str *rand-txt-no*
               5
               :keep-last-if-less nil
               :overlap t))

(defparameter *hume-tokvec-d*
  (list2vec *hume-toklist-d*))
(defparameter *rand-tokvec-d*
  (list2vec *rand-toklist-d*))  ;-------

(defparameter *hume-ngrams-d*
  (vec2ngrams *hume-tokvec-d*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-d*
  (vec2ngrams *rand-tokvec-d*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *hume-rand-d*
  (rr *hume-ngrams-d* *rand-ngrams-d*))
(defparameter *rand-hume-d*
  (rr *rand-ngrams-d* *hume-ngrams-d*))


;; ;; =====
;; ;; HUME VS RANDOM, triplets of 5grams, no-space, little-endian, id, 5/E
;; ;; =====
;; ;; (defparameter *hume-toklist-d*
;; ;;   (explode-str *hume-txt-no*
;; ;;                5
;; ;;                :keep-last-if-less nil
;; ;;                :overlap t))
;; ;; (defparameter *rand-toklist-d*
;; ;;   (explode-str *rand-txt-no*
;; ;;                5
;; ;;                :keep-last-if-less nil
;; ;;                :overlap t))

;; ;; (defparameter *hume-tokvec-d*
;; ;;   (list2vec *hume-toklist-d*))
;; ;; (defparameter *rand-tokvec-d*
;; ;;   (list2vec *rand-toklist-d*)) --------

(defparameter *hume-ngrams-e*
  (vec2ngrams *hume-tokvec-d*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-e*
  (vec2ngrams *rand-tokvec-d*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *hume-rand-e*
  (rr *hume-ngrams-e* *rand-ngrams-e*))
(defparameter *rand-hume-e*
  (rr *rand-ngrams-e* *hume-ngrams-e*))



;; ;; =====
;; ;; AUSTEN VS RANDOM, triplets of 2-Char, no-space, big-endian, id, 6/F
;; ;; =====
(defparameter *austen-toklist-f*
  (explode-str *austen-txt-no*
               2
               :keep-last-if-less nil
               :overlap nil))
(defparameter *rand-toklist-f*
  (explode-str *rand-txt-no*
               2
               :keep-last-if-less nil
               :overlap nil))

(defparameter *austen-tokvec-f*
  (list2vec *austen-toklist-f*))
(defparameter *rand-tokvec-f*
  (list2vec *rand-toklist-f*))  ;-------

(defparameter *austen-ngrams-f*
  (vec2ngrams *austen-tokvec-f*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-f*
  (vec2ngrams *rand-tokvec-f*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *austen-rand-f*
  (rr *austen-ngrams-f* *rand-ngrams-f*))
(defparameter *rand-austen-f*
  (rr *rand-ngrams-f* *austen-ngrams-f*))



;; ;; =====
;; ;; WHITMAN VS RANDOM, triplets of 3-Char, no-space, big-endian, id, 7/G
;; ;; =====
(defparameter *whit-toklist-g*
  (explode-str *whit-txt-no*
               3
               :keep-last-if-less nil
               :overlap nil))
(defparameter *rand-toklist-g*
  (explode-str *rand-txt-no*
               3
               :keep-last-if-less nil
               :overlap nil))

(defparameter *whit-tokvec-g*
  (list2vec *whit-toklist-g*))
(defparameter *rand-tokvec-g*
  (list2vec *rand-toklist-g*))  ;-------

(defparameter *whit-ngrams-g*
  (vec2ngrams *whit-tokvec-g*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-g*
  (vec2ngrams *rand-tokvec-g*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *whit-rand-g*
  (rr *whit-ngrams-g* *rand-ngrams-g*))
(defparameter *rand-whit-g*
  (rr *rand-ngrams-g* *whit-ngrams-g*))



;; ;; =====
;; ;; WHITMAN VS RANDOM, triplets of 3-Char, no-space, little-endian, id, 8/H
;; ;; =====
;; ;; (defparameter *whit-toklist-g*
;; ;;   (explode-str *whit-txt-no*
;; ;;                3
;; ;;                :keep-last-if-less nil
;; ;;                :overlap nil))
;; ;; (defparameter *rand-toklist-g*
;; ;;   (explode-str *rand-txt-no*
;; ;;                3
;; ;;                :keep-last-if-less nil
;; ;;                :overlap nil))

;; ;; (defparameter *whit-tokvec-g*
;; ;;   (list2vec *whit-toklist-g*))
;; ;; (defparameter *rand-tokvec-g*
;; ;;   (list2vec *rand-toklist-g*)) -------

(defparameter *whit-ngrams-h*
  (vec2ngrams *whit-tokvec-g*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-h*
  (vec2ngrams *rand-tokvec-g*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *whit-rand-h*
  (rr *whit-ngrams-h* *rand-ngrams-h*))
(defparameter *rand-whit-h*
  (rr *rand-ngrams-h* *whit-ngrams-h*))



;; ;; =====
;; ;; NIETZ VS RANDOM, triplets of 5Char, no-space, big-endian, id, 9/I
;; ;; =====
(defparameter *nietz-toklist-i*
  (explode-str *nietz-txt-no*
               5
               :keep-last-if-less nil
               :overlap nil))
(defparameter *rand-toklist-i*
  (explode-str *rand-txt-no*
               5
               :keep-last-if-less nil
               :overlap nil))

(defparameter *nietz-tokvec-i*
  (list2vec *nietz-toklist-i*))
(defparameter *rand-tokvec-i*
  (list2vec *rand-toklist-i*))  ;-------

(defparameter *nietz-ngrams-i*
  (vec2ngrams *nietz-tokvec-i*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-i*
  (vec2ngrams *rand-tokvec-i*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *nietz-rand-i*
  (rr *nietz-ngrams-i* *rand-ngrams-i*))
(defparameter *rand-nietz-i*
  (rr *rand-ngrams-i* *nietz-ngrams-i*))



;; ;; =====
;; ;; NIETZ VS RANDOM, triplets of 5Char, no-space, little-endian, id, 10/J
;; ;; =====
;; ;; (defparameter *nietz-toklist-i*
;; ;;   (explode-str *nietz-txt-no*
;; ;;                5
;; ;;                :keep-last-if-less nil
;; ;;                :overlap nil))
;; ;; (defparameter *rand-toklist-i*
;; ;;   (explode-str *rand-txt-no*
;; ;;                5
;; ;;                :keep-last-if-less nil
;; ;;                :overlap nil))

;; ;; (defparameter *nietz-tokvec-i*
;; ;;   (list2vec *nietz-toklist-i*))
;; ;; (defparameter *rand-tokvec-i*
;; ;;   (list2vec *rand-toklist-i*)) ---------

(defparameter *nietz-ngrams-j*
  (vec2ngrams *nietz-tokvec-i*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))
(defparameter *rand-ngrams-j*
  (vec2ngrams *rand-tokvec-i*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  #'identity)))

(defparameter *nietz-rand-j*
  (rr *nietz-ngrams-j* *rand-ngrams-j*))
(defparameter *rand-nietz-j*
  (rr *rand-ngrams-j* *nietz-ngrams-j*))




;; ;; ========================================================================
;; ;; ========================================================================

;; ;; ========================================================================
;; ;; ========================== WORDS =======================================
;; ;; ========================================================================
;; ;; ================== DEMO, K, L, M, N ====================================
;; ;; ========================================================================


;; ;; WORDLISTS
(defparameter *austen-wordlist*
  (str2list *austen-text*
            *delims*))
(defparameter *hume-wordlist*
  (str2list *hume-text*
            *delims*))
(defparameter *milton-wordlist*
  (str2list *milton-text*
            *delims*))
(defparameter *nietz-wordlist*
  (str2list *nietz-text*
            *delims*))
(defparameter *whit-wordlist*
  (str2list *whit-text*
            *delims*))
(defparameter *poems-eng-wordlist*
  (str2list *poems-eng*
            *delims*))
(defparameter *poems-gr-wordlist*
  (str2list *poems-gr*
            *delims*))

;; ;; WORDVECTORS
(defparameter *austen-wordvec*
  (list2vec *austen-wordlist*))
(defparameter *hume-wordvec*
  (list2vec *hume-wordlist*))
(defparameter *milton-wordvec*
  (list2vec *milton-wordlist*))
(defparameter *nietz-wordvec*
  (list2vec *nietz-wordlist*))
(defparameter *whit-wordvec*
  (list2vec *whit-wordlist*))
(defparameter *poems-eng-wordvec*
  (list2vec *poems-eng-wordlist*))
(defparameter *poems-gr-wordvec*
  (list2vec *poems-gr-wordlist*))


;; ;; =====
;; ;; HUME VS MILTON, words, naive, DEMO
;; ;; =====
(defparameter *hume-downcase-naive*
  (vec2ngrams *hume-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  #'identity
                                  #'+
                                  (funcall #'digit-reducer 128))))
(defparameter *milton-downcase-naive*
  (vec2ngrams *milton-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  #'identity
                                  #'+
                                  (funcall #'digit-reducer 128))))

(defparameter *naive-hume*
  (rr *hume-downcase-naive* *milton-downcase-naive*))
(defparameter *naive-milton*
  (rr *milton-downcase-naive* *hume-downcase-naive*))


;; ;; =================
;; ;; HUME VS NIETZSCHE, WORD - 3grams, big-endian, natlog, 11/K
;; ;; =================
(defparameter *hume-3w-k*
  (vec2ngrams *hume-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *nietz-3w-k*
  (vec2ngrams *nietz-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *hume-nietz-k*
  (rr *hume-3w-k* *nietz-3w-k*))
(defparameter *nietz-hume-k*
  (rr *nietz-3w-k* *hume-3w-k*))


;; ;; =================
;; ;; HUME VS NIETZSCHE, WORD - 3grams, little-endian, natlog, 12/L
;; ;; =================
(defparameter *hume-3w-l*
  (vec2ngrams *hume-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *nietz-3w-l*
  (vec2ngrams *nietz-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *hume-nietz-l*
  (rr *hume-3w-l* *nietz-3w-l*))
(defparameter *nietz-hume-l*
  (rr *nietz-3w-l* *hume-3w-l*))


;; ;; =================
;; ;; AUSTEN VS WHITMAN, WORD - 3grams, big-endian, natlog, 13/M
;; ;; =================
(defparameter *austen-3w-m*
  (vec2ngrams *austen-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *whit-3w-m*
  (vec2ngrams *whit-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *austen-whit-m*
  (rr *austen-3w-m* *whit-3w-m*))
(defparameter *whit-austen-m*
  (rr *whit-3w-m* *austen-3w-m*))


;; ;; =================
;; ;; AUSTEN VS WHITMAN, WORD - 3grams, little-endian, natlog, 14/N
;; ;; =================
(defparameter *austen-3w-n*
  (vec2ngrams *austen-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *whit-3w-n*
  (vec2ngrams *whit-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *austen-whit-n*
  (rr *austen-3w-n* *whit-3w-n*))
(defparameter *whit-austen-n*
  (rr *whit-3w-n* *austen-3w-n*))



;; ;; ============== II ======================================================

;; ;; ======
;; ;; RANDOM(5grams,big,log) VS RANDOM(5chars,big,log), 15/O
;; ;; ======
(defparameter *randa-ngrams-o*
  (vec2ngrams *rand-tokvec-d*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *randb-ngrams-o*
  (vec2ngrams *rand-tokvec-i*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *randa-o*
  (rr *randa-ngrams-o*))
(defparameter *randb-o*
  (rr *randb-ngrams-o*))


;; ;; ======
;; ;; AUSTEN(3grams,big,log) VS AUSTEN(3grams,little,log), 16/P
;; ;; ======
(defparameter *austen-toklist-p*
  (explode-str *austen-txt-no*
               3
               :keep-last-if-less nil
               :overlap t))

(defparameter *austen-tokvec-p*
  (list2vec *austen-toklist-p*))  ;-------

(defparameter *austena-ngrams-p*
  (vec2ngrams *austen-tokvec-p*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *austenb-ngrams-p*
  (vec2ngrams *austen-tokvec-p*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *austena-p*
  (rr *austena-ngrams-p*))
(defparameter *austenb-p*
  (rr *austenb-ngrams-p*))



;; ;; =================
;; ;; AUSTEN(Words,big,log) VS AUSTEN(Words,little, log), 17/Q
;; ;; =================
(defparameter *austena-3w-q*
  (vec2ngrams *austen-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (big-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *austenb-3w-q*
  (vec2ngrams *austen-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *austena-q*
  (rr *austena-3w-q*))
(defparameter *austenb-q*
  (rr *austenb-3w-q*))



;; ;; ============
;; ;; POEMS-NGRAMS(3grams,big,log), 18/Rgrams
;; ;; ============
;; ;; ENG
(defparameter *poems-eng-toklist-r*
  (explode-str *poems-eng-no*
               3
               :keep-last-if-less nil
               :overlap t))

(defparameter *poems-eng-tokvec-r*
  (list2vec *poems-eng-toklist-r*))

(defparameter *poems-eng-ngrams-r*
  (vec2ngrams *poems-eng-tokvec-r*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))

(defparameter *poems-eng-r*
  (rr *poems-eng-ngrams-r*))


;; ;; GR
(defparameter *poems-gr-toklist-r*
  (explode-str *poems-gr-no*
               3
               :keep-last-if-less nil
               :overlap t))

(defparameter *poems-gr-tokvec-r*
  (list2vec *poems-gr-toklist-r*))

(defparameter *poems-gr-ngrams-r*
  (vec2ngrams *poems-gr-tokvec-r*
              3
              (make-token-encoder *greek-low-alist*
                                  (little-endianess *greek-base*)
                                  #'+
                                  (natlog *greek-base*))))

(defparameter *poems-gr-r*
  (rr *poems-gr-ngrams-r*))



;; ;; ============
;; ;; POEMS-WORDS(Words,little,log), 19/Rwords
;; ;; ============

(defparameter *poems-eng-rf*
  (vec2ngrams *poems-eng-wordvec*
              3
              (make-token-encoder *english-low-alist*
                                  (little-endianess *english-base*)
                                  #'+
                                  (natlog *english-base*))))
(defparameter *poems-gr-rf*
  (vec2ngrams *poems-gr-wordvec*
              3
              (make-token-encoder *greek-low-alist*
                                  (little-endianess *greek-base*)
                                  #'+
                                  (natlog *greek-base*))))

(defparameter *poems-eng-rrf*
  (rr *poems-eng-rf*))
(defparameter *poems-gr-rrf*
  (rr *poems-gr-rf*))



;; ;; ;;
;; ;; ;; for GC

(setf *rand-text* nil)
(setf *austen-text* nil)
(setf *hume-text* nil)
(setf *milton-text* nil)
(setf *nietz-text* nil)
(setf *whit-text* nil)
(setf *poems-eng* nil)
(setf *poems-gr* nil)
