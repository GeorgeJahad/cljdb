;;;; cljdb.el --- Clojure extensions to Emacs Jdb mode
;;;  (gud-jdb-find-source-using-classpath) and (jdb) derived from 
;;;        jdb mode in emacs gud.el
;;;  New portions: Copyright (C) 2009 George Jahad
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;

(eval-after-load "gud"
  '(progn
     (defun gud-jdb-find-source-using-classpath (p)
       "Find source file corresponding to fully qualified class p.
Convert p from jdb's output, converted to a pathname
relative to a classpath directory."
       (save-match-data
	 (let
	     ( ;; Replace dots with slashes and append ".java" to generate file
	      ;; name relative to classpath
	      (filename
	       (concat
		(mapconcat 'identity
			   (split-string
			    ;; Eliminate any subclass references in the class
			    ;; name string. These start with a "$"
			    ((lambda (x)
			       (if (string-match "$.*" x)
				   (replace-match "" t t x) p))
			     p)
			    "\\.") "/")
		".java"))
	      (cplist (append gud-jdb-sourcepath gud-jdb-classpath))
	      found-file)
	   (while (and cplist
		       (not (setq found-file
				  (file-readable-p
				   (concat (car cplist) "/" filename)))))
	     (setq cplist (cdr cplist)))
	   (if found-file (concat (car cplist) "/" filename)
	     (let*
		 ((len (length filename))
		  (cplist (append gud-jdb-sourcepath gud-jdb-classpath))
		  (clj-filename (concat (substring filename 0 (- len 4)) "clj")))
	       (while (and cplist
			   (not (setq found-file
				      (file-readable-p
				       (concat (car cplist) "/" clj-filename)))))
		 (setq cplist (cdr cplist)))
	       (if found-file (concat (car cplist) "/" clj-filename)))))))


     (defun jdb (command-line)
     "Run jdb with command line COMMAND-LINE in a buffer.
The buffer is named \"*gud*\" if no initial class is given or
\"*gud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value.

See `gud-jdb-use-classpath' and `gud-jdb-classpath' documentation for
information on how jdb accesses source files. Alternatively (if
`gud-jdb-use-classpath' is nil), see `gud-jdb-directories' for the
original source file access method.

For general information about commands available to control jdb from
gud, see `gud-mode'."
     (interactive
      (list (gud-query-cmdline 'jdb)))
     (setq gud-jdb-classpath nil)
     (setq gud-jdb-sourcepath nil)

     ;; Set gud-jdb-classpath from the CLASSPATH environment variable,
     ;; if CLASSPATH is set.
     (setq gud-jdb-classpath-string (getenv "CLASSPATH"))
     (if gud-jdb-classpath-string
	 (setq gud-jdb-classpath
	       (gud-jdb-parse-classpath-string gud-jdb-classpath-string)))
     (setq gud-jdb-classpath-string nil) ; prepare for next

     (gud-common-init command-line 'gud-jdb-massage-args
		      'gud-jdb-marker-filter)
     (set (make-local-variable 'gud-minor-mode) 'jdb)

     ;; If a -classpath option was provided, set gud-jdb-classpath
     (if gud-jdb-classpath-string
	 (setq gud-jdb-classpath
	       (gud-jdb-parse-classpath-string gud-jdb-classpath-string)))
     (setq gud-jdb-classpath-string nil) ; prepare for next
     ;; If a -sourcepath option was provided, parse it
     (if gud-jdb-sourcepath
	 (setq gud-jdb-sourcepath
	       (gud-jdb-parse-classpath-string gud-jdb-sourcepath)))

     (gud-def gud-remove "clear bp on %l"   "\C-d" "Remove breakpoint at current line")
     (gud-def gud-step   "step"          "\C-s" "Step one source line with display.")
     (gud-def gud-next   "next"          "\C-n" "Step one line (skip functions).")
     (gud-def gud-cont   "cont"          "\C-r" "Continue with display.")
     (gud-def gud-finish "step up"       "\C-f" "Continue until current method returns.")
     (gud-def gud-up     "up\C-Mwhere"   "<"    "Up one stack frame.")
     (gud-def gud-down   "down\C-Mwhere" ">"    "Up one stack frame.")
     (gud-def gud-run    "run"           nil    "Run the program.") ;if VM start using jdb
     (gud-def gud-print  "print %e"  "\C-p" "Evaluate Java expression at point.")

     (global-set-key (vconcat gud-key-prefix "\C-b") 'jdb-find-class)
     (setq clj-classes nil)

     (setq comint-prompt-regexp "^> \\|^.+\\[[0-9]+\\] ")
     (setq paragraph-start comint-prompt-regexp)
     (run-hooks 'jdb-mode-hook)

     (if gud-jdb-use-classpath
	 ;; Get the classpath information from the debugger
	 (progn
	   (if (string-match "-attach" command-line)
	       (gud-call "classpath"))
	   (fset 'gud-jdb-find-source
		 'gud-jdb-find-source-using-classpath))

       ;; Else create and bind the class/source association list as well
       ;; as the source file list.
       (setq gud-jdb-class-source-alist
	     (gud-jdb-build-class-source-alist
	      (setq gud-jdb-source-files
		    (gud-jdb-build-source-files-list gud-jdb-directories
						     "\\.java$"))))
       (fset 'gud-jdb-find-source 'gud-jdb-find-source-file)))

	
;(setq clj-classes (nreverse (gud-gdb-run-command-fetch-lines "classes" gud-comint-buffer)))

(defun jdb-next-token ()
  (save-excursion
    (forward-sexp)
    (forward-sexp)
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring-no-properties (point) end))))


(defun jdb-defn-name ()
  (save-excursion
    (beginning-of-defun)
    (forward-char)
    (jdb-next-token)))

(defun jdb-find-ns ()
  (interactive)
    (if (not (re-search-forward "\([ ]*ns" (buffer-size) t))
	(error "Clojure namespace not found in file"))
    (backward-sexp)
    (forward-sexp)
    (let ((end (point)))
      (backward-sexp)
      (if (not (string= (buffer-substring-no-properties (point) end) "ns"))
	  (progn
	    (forward-sexp)
	    (jdb-find-ns)))))
    

(defun jdb-ns ()
  (save-excursion
    (beginning-of-buffer)
    (jdb-find-ns)
    (let ((namespace  (jdb-next-token)))
      (while (string= (substring namespace 0 1) "#")
	(forward-sexp)
	(setq namespace (jdb-next-token)))
      namespace)))


(setq jdb-fixup-strings 
      '("-"  "_" 
	"+"  "_PLUS_" 
	">"  "_GT_" 
	"<"  "_LT_" 
	"="  "_EQ_" 
	"*"  "_STAR_" 
	"/"  "_SLASH_" 
	"!"  "_BANG_" 
	"?"  "_QMARK_"))

(defun jdb-fixup-name (name strings)
  (if (not (car strings))
      name
    (jdb-fixup-name
     (replace-regexp-in-string (car strings) (cadr strings) name)
     (cddr strings))))
	
(defun jdb-class-name ()
  (interactive)
  (jdb-fixup-name (format "%s$%s__" (jdb-ns) (jdb-defn-name))
		  jdb-fixup-strings))
    

(defun jdb-find-class-intern ( line)
  (interactive)
  (let ((classes clj-classes)
	(class-name  (jdb-class-name))
	(found nil))
    (while (and (not found) (car classes))
      (if (string-match class-name (car classes))
	  (setq found 
		(jdb-set-breakpoint
		 (format "%s:%s" (car classes) line))))
      (setq classes (cdr classes)))
    found))


(defun jdb-find-class ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (if (not (jdb-find-class-intern line))
	(progn
	  (if gud-comint-buffer
	      (setq clj-classes 
		    (gud-gdb-run-command-fetch-lines 
		     "classes" gud-comint-buffer)))
	  (if (not (jdb-find-class-intern line))
	      (message 
	       (format "unable to set breakpoint for line %s in %s" 
		       line (jdb-class-name)))
	    (message
	     (format "set breakpoint for line %s in %s"
		     line (jdb-class-name)))))
      (message
       (format "set breakpoint for line %s in %s"
	       line (jdb-class-name))))))
	  


(defun jdb-set-breakpoint (string)
  (interactive)
  (let ((output 
	 (gud-gdb-run-command-fetch-lines 
	  (format "stop at %s" string)
	  gud-comint-buffer)))
    (if (string-match "Unable to set" (car output))
	(progn 
	  (gud-gdb-run-command-fetch-lines 
	   (format "clear %s" string)
	   gud-comint-buffer)
	  nil)
      output)))

	  
(defvar cljdb-version .3)))