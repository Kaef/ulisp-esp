const char LispLibrary[] PROGMEM =
/* "(pinmode 25 t)(digitalwrite 25 t)" */

/* load a program from sd-card, each lisp-function must be written in one line!*/
"(defun loadProgram (filename) \
    (with-sd-card (s filename) \
      (loop \
        (let ((line (read s))) \
        (if (null line) \
          (return) \
          (progn \
            (print line) \
            (eval line)))))))"

/* cat a program from sd-card:*/
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
  with-sd-card (s \"/lisp.txt\") \
    (eval (read s))) \
    */

"(defun blink (pin x) (pinmode pin t) (digitalwrite pin x) (delay 250) (blink pin (not x)))"
"(defun fastblink (pin x) (pinmode pin t) (digitalwrite pin x) (delay 50) (fastblink pin (not x)))"
"(defun veryfastblink (pin x) (pinmode pin t) (digitalwrite pin x) (veryfastblink pin (not x)))"
"(defun b26 () (blink 26 t))"
"(defun fb26 () (fastblink 26 t))"
"(defun vfb26 () (veryfastblink 26 t))"

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

; /* END OF LispLibrary[] definition


/* Bugs found in ulisp-2.4.esp-RC:

  ----------
  2018-10-12
  setq without left-hand-symbol reboots system
  ============================================
  522479> (setq 17)
  Guru Meditation Error: Core  1 panic'ed (LoadProhibited). Exception was unhandled.
  Core 1 register dump:
  PC      : 0x400d5ef3  PS      : 0x00060930  A0      : 0x800d5ba0  A1      : 0x3ffb1ef0  
  A2      : 0x00000000  A3      : 0x00000000  A4      : 0x3f803884  A5      : 0x3f803884  
  A6      : 0x3f803874  A7      : 0x00000000  A8      : 0x00000001  A9      : 0x3f80387b  
  A10     : 0x0000000d  A11     : 0x00000000  A12     : 0x4009155f  A13     : 0x3ffb1eb0  
  A14     : 0x00050723  A15     : 0x3ffb2034  SAR     : 0x0000001b  EXCCAUSE: 0x0000001c  
  EXCVADDR: 0x00000000  LBEG    : 0x4000c28c  LEND    : 0x4000c296  LCOUNT  : 0x00000000  
  
  Backtrace: 0x400d5ef3:0x3ffb1ef0 0x400d5b9d:0x3ffb1f10 0x400d6f95:0x3ffb1f50 0x400d704b:0x3ffb1f70 0x4013302a:0x3ffb1fa0
  
  Rebooting...
  ets Jun  8 2016 00:22:57

  ----------
  2018-10-11
  Memory-eater: (turn on #define printgcs)
  =============
  (defun count (n)(let ((longvalue n)) (print longvalue) (delay 10) (count (+ 1 longvalue))))
  WORSPACESIZE 4096: counting to 1305 and then: Error: No room

  (defun  count (n) (let ((longvalue (+ n 1)))(print longvalue)(delay 100) (count longvalue)))
  should fix it, count to 1306 an then Error: No room

  even:
  (defun  count (n) (let ((l (+ n 1)))(print l)(delay 100) (count l)))
  leads to the same error, counts to 1306 an then Error: No room

  A while ago I found that let is not gc-save, could this is the reason?
  
  (defun  count (n) (if (= (mod n 100) 0) (progn (gc)(count (+ n 1))) (let ((l (+ n 1)))(print l)(delay 100) (count l))))

  counts up to 1311 -> Error: No room

  Mailed to David, 11.10.2018 Kaef
  => happens when using let (with assignment) and tail recursive forms > see email

  ----------
 */
