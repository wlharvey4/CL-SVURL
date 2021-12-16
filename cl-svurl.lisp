;;; lolh/cl-svurl --- SVURL in Common-Lisp      -*- mode:lisp; -*-
;;; Time-stamp: <2021-12-16 11:42:43 lolh>


;;; Author: LOLH <email>
;;; Created: 2021-12-15
;;; Keywords: svurl, url
;;; Version: 0.1

;;; Commentary:
;;; Save urls and jpegs using Common-Lisp.  Uses package :quri to parse URLs.
;;; See https://github.com/fukamachi/quri
;;; 7 URI components -- scheme, userinfo, host name, port, path, query, fragment

;;; Usage:
;;; (lolh/cl-svurl:svurl-init [dir])
;;; $ ccl -Qb -l cl-svurl.[lisp|.dx64fsl] -- [args...]

;;; NOTE:
;;; This program requires CLOZURE COMMON-LISP, but it can be made portable
;;; with a small amount of work.  It relies upon CCL:FILE-DATA-SIZE instead
;;; of using common-lisp's file-length function.  To make portable, use the
;;; trivial-file-size package instead.

;;; Code:

(defpackage :lolh/cl-svurl
  (:use
   :common-lisp)
  (:export
   :svurl-init))
(ql:quickload :quri :silent t)
(in-package :lolh/cl-svurl)


(defvar *CLARGS* CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*)

(defvar *DEFAULT-DIR*
  (merge-pathnames
   (make-pathname :directory '(:absolute "Volumes" "Mt.Whitney" "Documents_from_Paddy" ".saved"))
   *DEFAULT-PATHNAME-DEFAULTS*))


(defstruct file-sz
  "Holds a file name and the file's size in bytes."
  file  ; FILE-NAME.TYPE
  size  ; UNSIGNED-BYTE
  mod)  ; FILE-WRITE-DATE


(defstruct svurl
  "Holds a directory DIR and a list of files FILES (and sizes) in the directory.
The files and sizes are in the form of an association list:
((file-name . file-size)...).
After a procedure has been run, will also hold a list of duplicate files DUPS.
SAVED, USED, and ORIGINS hold lists of urls associated with the files."
  (dir *DEFAULT-DIR*) ; PATHNAME
  files    ; LIST OF FILE-SZ STRUCTS
  dups     ; LIST OF FILE-NAMES
  saved    ; LIST OF URLS
  used     ; LIST OF URLS
  origins) ; LIST OF URLS


(defun ucla ()
  (princ "The command line arguments are: ")
  (prin1 *CLARGS*))


(defun svurl-init (&optional dir)
  "Initialize a new SVURL structure.
Create a new structure; add files to it, then find duplicates."
  (let ((sv (make-svurl :dir (parse-namestring (or dir *DEFAULT-DIR*)))))
    (get-files sv)
    (sort-files-find-dups sv)
    (prog2 (pprint sv) sv)))






(defun get-files (sv)
  "Get the jp*g files present in the directory DIR of structure SVURL SV.
Place them into slot FILES along with their sizes in bytes and mod times."
  (let* ((d (svurl-dir sv))
	 (fp1 (merge-pathnames (make-pathname :name :wild :type "jp*g") d))
	 (fp2 (merge-pathnames (make-pathname :name :wild :type "JP*G") d))
	 (fls (nconc
	   (directory fp1 :directories nil :all nil)
	   (directory fp2 :directories nil :all nil))))
    (setf (svurl-files sv)
	  (mapcar
	   (lambda (f) (make-file-sz
			:file (file-namestring f)
			:size (file-size f) ; instead of file-length
			:mod (file-write-date f)))
	   fls))))


;;; NOTE: To make portable, this can be obtained via trivial-file-size package
(defun file-size (f)
  "Return the size in bytes of the given file F.
This uses the CCL procedure CCL:FILE-DATA-SIZE to avoid opening the file and
counting its bytes manually.  Returns NIL if file is not found. From CCL manual:
`Returns size of file's data fork, without needing to open the file.'"
  (ccl:file-data-size f))
	  

(defun sort-files-find-dups (sv)
  "Sort the files in SV and place all duplicates into slot DUPS.
This uses a quicksort algorithm, modified to report duplicates."
  (labels ((qs (ls &aux (pivot (car ls)))
	     (if (cdr ls)
		 (nconc (qs (remove-if-not #'(lambda (fsz) (minusp (file-sz-cmp fsz pivot sv))) (cdr ls)))
		        (remove-if-not #'(lambda (fsz) (zerop  (file-sz-cmp fsz pivot sv))) (cdr ls))
			(qs (remove-if-not #'(lambda (fsz) (plusp  (file-sz-cmp fsz pivot sv))) (cdr ls))))
		 ls)))
    (qs (svurl-files sv))))


(defun file-sz-cmp (fsz pivot sv)
  "Compare two file-sz structures (FSZ1, FSZ2) by length and bytes.
If they are equal sizes, compare them byte-by-byte for identity.
Return -1 if the first file is smaller, or equal but not identical.
Return +1 if the first file is larger.
If they are the same size, send to file-sz-byte-cmp to check for identity.
Return 0 if the two are identical, or -1 if not."
  (let ((sz1 (file-sz-size fsz))
	(sz2 (file-sz-size pivot)))
    (if (< sz1 sz2)
	-1
	(if (> sz1 sz2)
	    1
	    (if (file-sz-byte-cmp fsz pivot sv)
		(prog1 0 (process-dups fsz pivot sv))
		-1)))))


(defun file-sz-byte-cmp (fsz1 fsz2 sv)
  "Compare equally-sized files to determine if they are duplicates.
Return T if the files are duplicates, or NIL if not."
  (let ((dir (svurl-dir sv)))
    (with-open-file (s1 (merge-pathnames (pathname (file-sz-file fsz1)) dir) :element-type '(unsigned-byte 8))
      (with-open-file (s2 (merge-pathnames (pathname (file-sz-file fsz2)) dir) :element-type '(unsigned-byte 8))
	(loop
	  for b1 = (read-byte s1 nil)
	  and b2 = (read-byte s2 nil)
	  until (and (null b1)
		     (null b2))
	  always (eql b1 b2))))))


(defun process-dups (fsz pivot sv)
  "Add a duplicate pair FSZ and PIVOT to the svurl-dups slot.
DUPS is an association list in which for each a-list, the car is the original
and the cdr include all duplicates of the original.  The original is the
duplicate with an earlier mod time.

((f1 f2 f3 f4) (f5 f6 f7 f8))
f1 is original, f2, f3, f4 are duplicates of it.
f5 is original, f6, f7, f8 are duplicates of it."

  (let* ((dups (svurl-dups sv))
	 (f (file-sz-file fsz))   ; file being compared
	 (ft (file-sz-mod fsz))   ; mod time file
	 (p (file-sz-file pivot)) ; pivot file
	 (pt (file-sz-mod pivot)) ; mod time pivot file
	 (o (if (< ft pt) f p))   ; original file (earlier mod time)
	 (d (if (> ft pt) f p))   ; duplicate file (later mod time)
	 (ls (assoc o dups :test #'string=))) ; list of dups for original
    (if (consp ls) ; if list of dups for original already exists
        (pushnew d (cdr ls) :test #'string=) ; add another duplicate to it
	(setf (svurl-dups sv) (acons o (list d) dups))))) ; else create it
    

(when *CLARGS* (ucla)(ccl:quit))

;;; lolh/cl-svurl ends here
