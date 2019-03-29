/*
 * Kaef, 2018-10-18
 * 
 * Just some test functions for my ulisp patches, nothing worthful now...
 * 
 */

const char LispLibrary[] PROGMEM =
/* "(pinmode 25 t)(digitalwrite 25 t)" */

/* some definitions to make live easier... */
"(defvar *INPUT* 0)"
"(defvar *OUTPUT* 1)"
"(defvar *INPUT_PULLUP* 2)"


/* some code (mostly) copied from ulisp.com: */
/* Benchmark tak: */
"(defun tak (x y z) \
  (if (not (< y x)) \
      z \
    (tak \
     (tak (1- x) y z) \
     (tak (1- y) z x) \
     (tak (1- z) x y))))"

"(defun q (n) \
   (if (<= n 2) \
     1 \
     (+ (q (- n (q (- n 1)))) (q (- n (q (- n 2)))))))"

/* speedup functions with one argument (see ulisp-forum): */
"(defun speedup (fn) \
  (let ((c nil)) \
    (lambda (x) \
      (or (cdr (assoc x c)) \
          (let ((r (funcall fn x))) \
            (setq c (cons (cons x r) c)) \
            r)))))"
            
/* load a program from sd-card, each lisp-function must be written in one line in the file! */
"(defun load (filename) \
    (with-sd-card (s filename) \
      (loop \
        (let ((line (read s))) \
        (if (null line) \
          (return) \
          (progn \
            (print line) \
            (eval line)))))))"

/* cat a program from sd-card: */
"(defun cat (filename) \
    (with-sd-card (s filename) \
      (loop \
        (let ((line (read-line s))) \
        (if (null line) \
          (return) \
          (print line))))))"

"(defun WriteReadProgram () \
  (print \"Write program\") \
  (with-sd-card (s \"/lisp.txt\" 2) \
    (write-string \"(defun sq (x) (* x x))\n\" s) \
    (write-string \"(defun tst () (print 42))\" s)) \
  (print \"Load program\") \
  (loadProgram \"/lisp.txt\") \
  (print (sq 123)))"

/* simple lisp program editor: */
/* http://forum.ulisp.com/t/simple-lisp-program-editor/54 */
"(defun e (l) \
  (loop \
   (let ((c (read))) \
     (when (eq c 'b) (return l)) \
     (setq l \
           (cond \
            ((eq c 'r) (read)) \
            ((eq c '?) (princ l) (terpri) l) \
            ((eq c 'c) (cons (read) l)) \
            ((atom l) (princ '*) l) \
            ((eq c 'd) (cons (car l) (e (cdr l)))) \
            ((eq c 'a) (cons (e (car l)) (cdr l))) \
            ((eq c 'x) (cdr l)) \
            (t (princ '!) (terpri) l))))))"

"(defun ed (l) (setq l (e (eval l))))"

"(defun color565 (r g b) \
  (logior (ash (logand r #xF8) 8) (ash (logand g #xFC) 3) (ash (logand b #xFF) -3)))"

/*
"(princ \"globals; \") (princ (globals)) (terpri)"
"(princ \"reset-reason: \") (princ (reset-reason)) (terpri)"
"(delay 100)(digitalwrite 25 nil)(pinmode 25 nil)"
*/

; // END OF LispLibrary[] definition
