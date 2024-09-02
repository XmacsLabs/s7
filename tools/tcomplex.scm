;;; complex timings

(define size 1000)
(define tries 1000)

(define cv (make-complex-vector size 1.0+i))

(define (c1)
  (do ((i 0 (+ i 1)))
      ((= i tries))
    (copy cv)))

;(c1) ; 7 copy_source_no_dest -> s7_vector_copy_1


(define (c2a)
  (let ((val 1+i))
    (do ((i 0 (+ i 1)))
	((= i tries))
      (fill! cv val))))

;(c2a) ; 3 complex_vector_fill

(define (c2)
  (let* ((len (length cv))
	 (cv1 (make-complex-vector len))
	 (val 1+i))
    (do ((j 0 (+ j 1)))
	((= j tries))
      (do ((i 0 (+ i 1)))
	  ((= i len) cv1)
	(complex-vector-set! cv1 i val)))))

;(c2) ; 100 univect_set(70) opt_p_call_sss g_complex_vector_set opt_do_step_i
     ; 36 complex_vector_set_p_pip_direct(15) opt_p_pip_sss opt_do_step_i


(define (c3)
  (let* ((len (length cv))
	 (cv1 (make-complex-vector len)))
    (do ((j 0 (+ j 1)))
	((= j tries))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(complex-vector-set! cv1 i (complex-vector-ref cv i))))))

;(c3) ; 161 univect_set + complex_vector_ref_p_pp + opt_p_call_ssf->g_complex_vector_set(74)
     ; needs complex_vector_set_pip_direct? and complex_vector_ref_p_pi and complex_vector_ref signature car is (complex? complex-vector?) which 65615 doesn't like
     ; 113 complex_vector_ref_p_pp complex_vector_set_p_ppp opt_p_ppp_ssf -- why not the pi/pip cases (68367)? 65011--needs p_pi 65655
     ; removed pointless cv1 return val -> opt_do_very_simple 113
     ; 83 complex_vector_ref_p_pi opt_do_very_simple complex_vector_set_p_pip_direct


(define (c4)
  (let* ((len (length cv))
	 (cv1 (make-complex-vector len)))
    (do ((j 0 (+ j 1)))
	((= j tries))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(complex-vector-set! cv1 i (complex i (- i)))))))

;;; (c4) ; 129 complex_vector_set_p_ppp complex_p_ii etc


(define (c5)
  (let* ((len (length cv))
	 (cv1 (copy cv))
	 (magmore (lambda (a b) (> (magnitude a) (magnitude b)))))
    (do ((j 0 (+ j 1)))
	((= j tries))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(complex-vector-set! cv1 i (complex i (- i))))
      (sort! cv1 magmore)
      ;; (let-temporarily (((*s7* 'print-length) 10)) (display cv1) (newline))
      (do ((i 0 (+ i 1)))
	  ((= i (- len 1)))
	(if (<= (magnitude (complex-vector-ref cv1 i)) (magnitude (complex-vector-ref cv1 (+ i 1))))
	    (display 'oops))))))

;(c5) ; 1481: 508 magnitude_p_p, 125 qsort, 117 gc, 116 opt_b_7pp_ffo, 80 opt_bool_sort_0, 76 complex_vector_ref_p_pi, 66 gt_b_7pp etc
     ;   maybe specialized sort_func for each data type (to avoid make_real etc)
      

(define (c6)
  (let ((cv1 (make-complex-vector size))
	(cv2 (make-complex-vector size))
	(cv3 (make-complex-vector size)))
    (do ((j 0 (+ j 1)))
	((= j tries))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(complex-vector-set! cv1 i (complex i i))
	(complex-vector-set! cv2 i (complex i (- i)))
	(complex-vector-set! cv3 i (* (complex-vector-ref cv1 i) (complex-vector-ref cv2 i)))
	(unless (zero? (imag-part (cv3 i)))
	  (format *stderr* "(cv3 ~D): ~S~%" i (cv3 i)))))))

;(c6) ; 579: 99 complex-vector-set_p_ppp, 76 complex_vector_ref_p_pi, 57 opt_p_ppp_ssf, 52 complex_p_ii etc


(define* (cfft data n (dir 1)) ; complex data
  (unless n (set! n (length data)))
  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((temp (data j)))
	  (set! (data j) (data i))
	  (set! (data i) temp)))
    (do ((m (/ n 2) (/ m 2)))
        ((or (< m 2) 
             (< j m))
         (set! j (+ j m)))
     (set! j (- j m))))
  (do ((ipow (floor (log n 2)))
       (prev 1)
       (lg 0 (+ lg 1))
       (mmax 2 (* mmax 2))
       (pow (/ n 2) (/ pow 2))
       (theta (complex 0.0 (* pi dir)) (* theta 0.5)))
      ((= lg ipow))
    (do ((wpc (exp theta))
         (wc 1.0)
         (ii 0 (+ ii 1)))
	((= ii prev)
	 (set! prev mmax))
      (do ((jj 0 (+ jj 1))
           (i ii (+ i mmax))
           (j (+ ii prev) (+ j mmax)))
          ((>= jj pow))
        (let ((tc (* wc (data j))))
          (set! (data j) (- (data i) tc))
          (set! (data i) (+ (data i) tc))))
      (set! wc (* wc wpc))))
  data)

(define (c7)
  (let ((cv1 (make-complex-vector 128 1+i)))
    (do ((i 0 (+ i 1)))
	((= i tries))
      (cfft cv1 128))))

;(c7) ; 807: 300 eval, 56 lookup, 50 op_dox, 46 add_p_pp, 35 fx_s, 34 complex_vector_getter, 34 complex_vector_set_p_ppp etc

#|
(unless (equivalent? (cfft (vector 0.0 1+i 0.0 0.0)) #(1+1i -1+1i -1-1i 1-1i))
  (format *stderr* "cfft 1: ~S~%" (cfft (vector 0.0 1+i 0.0 0.0))))
(let-temporarily (((*s7* 'equivalent-float-epsilon) 1e-14))
  (unless (equivalent? (cfft (vector 0 0 1+i 0 0 0 1-i 0)) #(2 -2 -2 2 2 -2 -2 2))
    (format *stderr* "cfft 2: ~S~%" (cfft (vector 0 0 1+i 0 0 0 1-i 0)))))
|#



(define (dolph N gamma)
  (let ((vals (make-complex-vector N)))
    (let ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N))))
      (do ((den (/ 1.0 (cosh (* N (acosh alpha)))))
	   (freq (/ pi N))
	   (mult -1 (- mult))
	   (i 0 (+ i 1))
	   (phase (* -0.5 pi)))
	  ((= i N))
	(set! (vals i) (* mult den (cos (* N (acos (* alpha (cos phase)))))))
	(set! phase (+ phase freq))))
    (let ((pk 0.0)
	  (w (make-float-vector N)))
      (do ((i 0 (+ i 1))
	   (sum 0.0 0.0))
	  ((= i N))
	(do ((k 0 (+ k 1))
	     (cin (/ (* 2.0 0+1.0i pi i) N)))
	    ((= k N))
	  (set! sum (+ sum (* (vals k) (exp (* cin k))))))
	(set! (w i) (magnitude sum))
	(set! pk (max pk (w i))))
      (do ((i 0 (+ i 1)))
	  ((= i N))
	(set! (w i) (/ (w i) pk)))
      w)))

(define (c8)
  (let ((v (dolph 16 2.5))
	(vr #r(0.097 0.113 0.221 0.366 0.536 0.709 0.860 0.963 1.000 0.963 0.860 0.709 0.536 0.366 0.221 0.113)))
    (let-temporarily (((*s7* 'equivalent-float-epsilon) 1e-3))
      (unless (equivalent? v vr)
	(let ((pk 0.0))
	  (do ((i 0 (+ i 1)))
	      ((= i 16)
	       (format *stderr* "dolph pk: ~S~%" pk))
	    (let ((mx (magnitude (- (v i) (vr i)))))
	      (if (> mx pk)
		  (set! pk mx)))))))))


;;; closure sort func
;;; reverse!/reverse, append+float etc, iterate, map/for-each, object->string? equal?/equivalent?
;;; LA w/ complex [(and cfft)]
