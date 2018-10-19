/*
 * Kaef, 2018-10-18
 * 
 * Just some test functions for my ulisp patches, nothing worthful now...
 * 
 */

const char LispLibrary[] PROGMEM =
/* "(pinmode 25 t)(digitalwrite 25 t)" */

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

/* just for testing */
"(defun blink (pin x) (pinmode pin t) (digitalwrite pin x) (delay 250) (blink pin (not x)))"
"(defun fastblink (pin x) (pinmode pin t) (digitalwrite pin x) (delay 50) (fastblink pin (not x)))"
"(defun veryfastblink (pin x) (pinmode pin t) (digitalwrite pin x) (veryfastblink pin (not x)))"
"(defun b26 () (blink 26 t))"
"(defun fb26 () (fastblink 26 t))"
"(defun vfb26 () (veryfastblink 26 t))"
// */

/* lisp-server */
"(defvar *SERVER-IP* '(192 168 1 204))"
"(defvar *SERVER-PORT* 1234)"
"(defun lisp-server () \
  (with-client (s (ip *SERVER-IP*) *SERVER-PORT*) \
    (print \"Listening...(start 'nc -l [k] *SERVER-PORT*' on host)\") \
    (loop \
     (unless (= 0 (available s)) \
       (let ((line (read-line s))) \
         (print line) \
         (println (eval (read-from-string line)) s))) \
     (delay 1000))))"
"(defun ip (lis) \
  (let ((x 0)) \
    (mapc (lambda (n) (setq x (+ (ash x 8) n))) (reverse lis)) \
    x))"

"(defun println (x s) (princ x s) (princ #\\return s) (princ #\\newline s))"

/* this can't be stopped: "(blink 25 t)" */

/*
"(princ \"globals; \") (princ (globals)) (terpri)"
"(princ \"reset-reason: \") (princ (reset-reason)) (terpri)"
"(delay 100)(digitalwrite 25 nil)(pinmode 25 nil)"
*/

; // END OF LispLibrary[] definition
