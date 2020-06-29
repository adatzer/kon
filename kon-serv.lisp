;;;; Ada serving
;;;; DEPENDS-ON "kon-params.lisp"



(in-package :kon)



;; ===========================
;; SERVER
;; ===========================

;; hunchentoot has already defined
;; 1. (defvar *default-content-type* "text/html")
;; 2. (defvar *hunchentoot-default-external-format* +utf-8+)
;; 3. (defconstant +utf-8+
;;      (flexi-streams:make-external-format :utf8 :eol-style :lf))


;; SERVER
;; (defvar *kon-server*
;;   (make-instance 'hunchentoot:easy-acceptor
;;                  :document-root "/home/ada/common-lisp/ada/kon/www/"
;;                  :port 8080))

;; (hunchentoot:start *kon-server*)

(defvar *kon-server* nil)

(defun set-server (wwwdir portnum)
  (if (null *kon-server*)
      (progn
        (setf *kon-server* (make-instance 'hunchentoot:easy-acceptor
                                          :document-root wwwdir
                                          :port portnum))
        (hunchentoot:start *kon-server*)
        (format t "server just started!~%serving: ~A~%on port: ~A" wwwdir portnum))
      (format t "server has already started..try stop-server first")))

(defun stop-server ()
  (if (null *kon-server*)
      (format t "there is no server running")
      (progn
        (hunchentoot:stop *kon-server*)
        (setf *kon-server* nil)
        (format t "server stopped"))))

;; Ensure server is stopped before any call of (ccl:quit)
(setf ccl:*lisp-cleanup-functions* (list #'kon::stop-server))


;; ;; DEMO -------------------------------------------------------------------
(hunchentoot:define-easy-handler (demohume :uri "/demohume")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *naive-hume*))))

(hunchentoot:define-easy-handler (demomilton :uri "/demomilton")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *naive-milton*))))

;; ;; A/1 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (awhit :uri "/awhit")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *whit-rand-a*))))

(hunchentoot:define-easy-handler (arand :uri "/arand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-whit-a*))))

;; B/2 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (bmilton :uri "/bmilton")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *milton-rand-b*))))

(hunchentoot:define-easy-handler (brand :uri "/brand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-milton-b*))))

;; C/3 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (cmilton :uri "/cmilton")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *milton-rand-c*))))

(hunchentoot:define-easy-handler (crand :uri "/crand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-milton-c*))))

;; D/4 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (dhume :uri "/dhume")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *hume-rand-d*))))

(hunchentoot:define-easy-handler (drand :uri "/drand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-hume-d*))))

;; E/5 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (ehume :uri "/ehume")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *hume-rand-e*))))

(hunchentoot:define-easy-handler (erand :uri "/erand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-hume-e*))))

;; F/6 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (fausten :uri "/fausten")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austen-rand-f*))))

(hunchentoot:define-easy-handler (frand :uri "/frand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-austen-f*))))

;; G/7 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (gwhit :uri "/gwhit")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *whit-rand-g*))))

(hunchentoot:define-easy-handler (grand :uri "/grand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-whit-g*))))

;; H/8 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (hwhit :uri "/hwhit")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *whit-rand-h*))))

(hunchentoot:define-easy-handler (hrand :uri "/hrand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-whit-h*))))

;; I/9 --------------------------------------------------------------------
(hunchentoot:define-easy-handler (inietz :uri "/inietz")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *nietz-rand-i*))))

(hunchentoot:define-easy-handler (irand :uri "/irand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-nietz-i*))))

;; J/10 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (jnietz :uri "/jnietz")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *nietz-rand-j*))))

(hunchentoot:define-easy-handler (jrand :uri "/jrand")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *rand-nietz-j*))))

;; K/11 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (khume :uri "/khume")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *hume-nietz-k*))))

(hunchentoot:define-easy-handler (knietz :uri "/knietz")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *nietz-hume-k*))))

;; L/12 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (lhume :uri "/lhume")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *hume-nietz-l*))))

(hunchentoot:define-easy-handler (lnietz :uri "/lnietz")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *nietz-hume-l*))))

;; M/13 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (mausten :uri "/mausten")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austen-whit-m*))))

(hunchentoot:define-easy-handler (mwhit :uri "/mwhit")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *whit-austen-m*))))

;; N/14 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (nausten :uri "/nausten")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austen-whit-n*))))

(hunchentoot:define-easy-handler (nwhit :uri "/nwhit")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *whit-austen-n*))))

;; O/15 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (oranda :uri "/oranda")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *randa-o*))))

(hunchentoot:define-easy-handler (orandb :uri "/orandb")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *randb-o*))))

;; P/16 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (paustena :uri "/paustena")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austena-p*))))

(hunchentoot:define-easy-handler (paustenb :uri "/paustenb")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austenb-p*))))

;; Q/17 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (qaustena :uri "/qaustena")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austena-q*))))

(hunchentoot:define-easy-handler (qaustenb :uri "/qaustenb")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *austenb-q*))))

;; Rgrams/18 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (rgramseng :uri "/rgramseng")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *poems-eng-r*))))

(hunchentoot:define-easy-handler (rgramsgr :uri "/rgramsgr")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *poems-gr-r*))))

;; Rwords/19 -------------------------------------------------------------------
(hunchentoot:define-easy-handler (rwordseng :uri "/rwordseng")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *poems-eng-rrf*))))

(hunchentoot:define-easy-handler (rwordsgr :uri "/rwordsgr")
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:with-output (*standard-output*)
      (encode-array *poems-gr-rrf*))))
