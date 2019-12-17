# sd-card functions

* (ls) -- list directory -- this function has just side effects, printing the file-list at screen

  a later version may return a list of files (or list of (list of file, size))

* (rm filename) -- removes a file 'filename' from the sd-card
* (rmdir dirname) -- removes a (empty) directory from the sd-card
* (mkdir dirname) -- creates a directory 'dirname' on the sd-card
* (load), (save)  and (cat) functions are available through lisp-library, see LispLibrary.h and (list-library), (require)  
