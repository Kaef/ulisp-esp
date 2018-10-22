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
     
/* load a program from sd-card, each lisp-function must be written in one line in the file! */
"(defun loadProgram (filename) \
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
        (let ((line (read s))) \
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

/*
"(princ \"globals; \") (princ (globals)) (terpri)"
"(princ \"reset-reason: \") (princ (reset-reason)) (terpri)"
"(delay 100)(digitalwrite 25 nil)(pinmode 25 nil)"
*/

; // END OF LispLibrary[] definition
