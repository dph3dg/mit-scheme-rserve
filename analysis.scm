;; various examples of useful functions using rserve.scm
;; uses the open command of OSX to show plots otherwise should be general




;; use nonlinear fits in R
;; fn is R string
;; data is zipped list of x and y values
;; params is list of guesses of parameters
;; params in fn must be p[1] p[2] etc
;; eg a complicated multi line fn could be:
;; (define fn-s "q0 <- function(p,x){a1 <- p[1];a2 <- p[2];a3 <- p[3];return (a1/(1+(a3+x)^2/(a2^2))) };f <- function(p,x) {a4 <- p[4];n = 4;s = 0;for (i in 1:n){;s = s + (-q0(p,x)^(i-1))/(i^(3.0/2.0))}; return(a4*s)}")
;; (define guess 1 1 -20 2)
;; if x_vals and y_vals are already defined then call with :
;;  (fit fn-s (zip x_vals y_vals) guess))

(define (fit fn data params)
  (let* ((names (map (lambda (x) (string-append "a" (number->string x))) (iota (length params) 1)))
	(vals (map (lambda (x y) (string-append x "=" (number->string y) "," )) names params))
	(names-string-tmp (apply string-append (map (lambda(x) (string-append x ","))  names)))
	(vals-string-tmp (apply string-append vals))
	(names-string (string-append "(" (string-head names-string-tmp (- (string-length names-string-tmp) 1)) ")" ))
	(vals-string (string-append "(" (string-head vals-string-tmp (- (string-length vals-string-tmp) 1)) ")"))
	(fit-string (string-append "fit = nls(signal ~ f(c" names-string ",distance),start=list" vals-string ")"))
	(data-x (string-append "distance <- " (list->R-string (map first data))))
	(data-y (string-append "signal <- " (list->R-string (map second data))))
)
(display fn)
(newline)
(display fit-string)	
(newline)
(display data-x)
(newline)
(display data-y)
(map eval-no-return (list data-x data-y fn fit-string))
(eval "unname(coefficients(fit))")
))


;;;;;;;;; if data has third parameter it's assumed to be the error bar

(define (listplot d #!optional joined)
  (let ((x (map first d))
	(y (map second d))
	(se (if (= 3 (length (first d)))
		(map third d)
		'()))
	(desktop-file (string-append (directory-namestring (user-homedir-pathname)) "Desktop/Rplot.pdf")))
    (eval-no-return "library(ggplot2)")
    (eval-no-return (string-append "x <- " (list->R-string x)))
    (eval-no-return (string-append "y <- " (list->R-string y)))
    (if (eq? '() se)
	'()
	(eval-no-return (string-append 
			 "se <- " (list->R-string se)
			 "; limits <- aes(ymax=y+se, ymin=y-se)" )))
    (eval-no-return (string-append "pdf(file=\"" desktop-file "\")"))
    (let ((err-string (if (eq? '() se)
	"" 
	(string-append 
	 " + geom_errorbar(limits, width=" 
	 (number->string (/ (* 2.0 (- (apply max x) (apply min x))) 100))
	 ")"))))
;      (display err-string)
    (let ((cmd (string-append "p <- qplot(x,y)" err-string))
	  (cmd-line (string-append "p <- qplot(x,y,geom=\"line\")" err-string)))
      (if (default-object? joined)
	  (eval-no-return cmd)
	  (if (memq 'line joined)
	      	  (eval-no-return (string-append cmd-line (apply string-append (map r-cmd joined))))
		  (eval-no-return (string-append cmd (apply string-append (map r-cmd joined))))))
      (eval-no-return "print(p)")
      (eval-no-return "dev.off()")
      (run-shell-command (string-append "open  " desktop-file))))))
      

; example
;(listplot '((1 1) (2 4) (3 9) (4 16) (5 25) (6 36) (7 49) (8 64) (9 81)))
;(listplot '((1 1) (2 4) (3 9) (4 16) (5 25) (6 36) (7 49) (8 64) (9 81)) '(joined (x "some text")))
; any extra parameter will do will eventually be list of things  line, colour etc
;(define allowed-options '((joined "joined string") (print  "printy") (points-only "lines only")))
(define allowed-options '((joined " + geom_line(color=\"red\")") (line "") (x) (y) (title)))

(define (r-cmd op)
  (cond ((pair? op)
	 (cond ((eq? (car op) 'x) 
		(string-append " + xlab(\"" (cadr op) "\")"))
	       ((eq? (car op) 'y) 
		(string-append " + ylab(\"" (cadr op) "\")"))
	       ((eq? (car op) 'title) 
		(string-append " + ggtitle(\"" (cadr op) "\")"))
	       (else "")))
    	((assq op allowed-options) => cadr)
	(else "")))

(define (show-fit d1 d2) ;
  (let ((x1 (map first d1))
	(y1 (map second d1))
	(x2 (map first d2))
	(y2 (map second d2))
	(se (if (= 3 (length (first d1)))
		(map third d1)
		'()))
	(desktop-file (string-append (directory-namestring (user-homedir-pathname)) "Desktop/Rplot.pdf"))
	)
    (eval-no-return "library(ggplot2)")
    (eval-no-return (string-append "xdata <- " (list->R-string x1)))
    (eval-no-return (string-append "ydata <- " (list->R-string y1)))
    (eval-no-return (string-append "xfn <- " (list->R-string x2)))
    (eval-no-return (string-append "yfn <- " (list->R-string y2)))
    (if (eq? '() se)
	'()
	(eval-no-return (string-append 
			 "se <- " (list->R-string se)
			 "; limits <- aes(ymax=ydata+se, ymin=ydata-se)" )))
    (eval-no-return "data <- data.frame(x=xdata,y=ydata)")
    (eval-no-return "fn <- data.frame(x=xfn,y=yfn)")
    (eval-no-return "colnames(data) <- c(\"distance\", \"signal\")")
    (eval-no-return "colnames(fn) <- c(\"distance\", \"signal\")")
    (eval-no-return (string-append "pdf(file=\"" desktop-file "\")"))
;    (eval-no-return "pdf(file=\"~/Desktop/Rplot.pdf\")")
    (let ((err-string (if (eq? '() se)
	"" 
	(string-append 
	 " + geom_errorbar(limits, width=" 
	 (number->string (/ (* 2.0 (- (apply max x1) (apply min x1))) 100))
	 ")"))))
    (eval-no-return (string-append "p <- qplot(x=distance,y=signal,data=data,geom=\"point\") + geom_line(aes(x=distance,y=signal,colour=NULL), data=fn, colour=\"red\")" err-string))
      (eval-no-return "print(p)")
      (eval-no-return "dev.off()")
      (run-shell-command (string-append "open  " desktop-file)))))
      
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;; THESE EXAMPLES USE SCMUTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;; tries to emulate Fit from mathematica using linear combinations ;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
 
      (define data '((201 592 61) (244 401 25) (47 583 38) (287 402 15) (203 495 21) 
			    (58 173 15) (210 479 27) (202 504 14) (198 510 30)
			    (158 416 16) (165 393 14) (201 442 25) (157 317 52)
			    (131 311 16) (166 400 34) (160 337 31) (186 423 42)
			    (125 334 26) (218 533 16) (146 344 22)))

(define data-outliers (drop data 5))
(listplot data-outliers)

(define (linear-fit d p)     ; d = data and p = powers ie bx^0 + mx^1 .. 
  (let* ((d-matrix (apply matrix-by-rows d))
	 (weighting (if (= (length (first d)) 3)
			(m:nth-col d-matrix 2)
			(v:generate (length d) (lambda (i) 1))))
	(designm (m:generate (length d) (length p)
	    (lambda (i j)
	      (/ (expt (matrix-ref d-matrix i 0) (list-ref p j)) (vector-ref weighting i)))))
	(l (v:generate (length d)
		      (lambda (i)
			(/ (matrix-ref d-matrix i 1) (vector-ref weighting i))))))
    (svd-solve-linear-system designm l)))
(define (plot-fit d p)

  (let* ((best (vector->list (linear-fit d p)))
	 (x (map first d))
	 (fit-y 
	  (map (lambda (i) (apply + (map (lambda (j k) (* k (expt i j))) p best))) x)))
    (show-fit d (zip x fit-y))))





(linear-fit data-outliers '(0 1))
(plot-fit data-outliers '(0 1))

(define (straight-line-log-likelihood x y sigmay m b)
  (let ((a (apply + (map (lambda (i) (log (* (/ 1 (* (sqrt (* 2 pi)) i))))) sigmay)))
	(b (apply + (map (lambda (i j k) 
			   (let ((ymxb (- j (+ (* m i) b))))
			     (/ (* -0.5 (* ymxb ymxb)) (* k k)))) x y sigmay))))
(+ a b)))

(define (straight-line-log-prior m b)
  0)

(define (straight-line-log-posterior x y sigmay m b)
  (+ (straight-line-log-likelihood x y sigmay m b)
     (straight-line-log-prior m b)))


(define (linspace start stop count)
  (iota count start (/ (- stop start) (- count 1))))

(define (matrix->list m)
  (let ((c (m:num-cols m)))
    (define (get-all-cols m col-num)
      (if (= col-num c)
	  '()
	  (cons (vector->list (m:nth-col m col-num)) 
		(get-all-cols m (+ 1 col-num)))))
    (get-all-cols m 0)))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define (create-matrix-R m m-name)
  (eval-no-return (string-append m-name "= matrix (" (list->R-string 
						 (flatten (matrix->list m)))
				 ",nrow="
				 (number->string (m:num-rows m)) 
				 ",ncol="
				 (number->string (m:num-cols m))")")))

(define (density-plot m)
  (let ((desktop-file (string-append (directory-namestring 
				      (user-homedir-pathname)) 
				     "Desktop/Rplot.pdf")))
    (display "loading ggplot2\n")
    (eval-no-return "library(plot3D)")
    (display "loading reshape\n")
;    (eval-no-return "library(reshape)")
    (display "creating matrix in R\n")
    (create-matrix-R m "M")
;    (eval-no-return "M = matrix(c(0,0,0,1,2,3,4,5,6),nrow=3,ncol=3)")
    (display "setting pdf file on desktop\n")
    (eval-no-return (string-append "pdf(file=\"" desktop-file "\")"))
    (display "creating m density plot\n")
 ;   (eval-no-return "m <- ggplot(melt(M), aes(x = X1,y=X2)) + stat_density2d(geom=\"tile\", aes(fill = ..density..), contour = FALSE)")
;m + geom_density2d()
;    (eval-no-return "p <- persp(M)")
    (eval-no-return "p <- image2D(M, xaxt = \"n\", yaxt=\"n\")")
(eval-no-return "axis(1, at=seq(0,0.8,0.2), LETTERS[1:4])")
;axis(1, at=seq(0,0.8,0.2), labels=rownames(sample))
(eval-no-return "t <- seq(0,1,0.1)")
(eval-no-return "axis(2, t,labels=t*nrow(M))")

    (display "printing m density plot to desktop\n")
 ;     (eval-no-return "print(p)")
      (display "closing print file\n")
      (eval-no-return "dev.off()")
      (display "opening pdf\n")
;      (close)
      (run-shell-command (string-append "open  " desktop-file))))
