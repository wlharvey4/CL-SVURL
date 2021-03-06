;;; lolh/cl-svurl --- SVURL in Common-Lisp      -*- mode:lisp; -*-
;;; Time-stamp: <2021-12-17 13:40:39 lolh>


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
;;; It also uses CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*.
;;; Based upon a problem renaming a file onto an attached /Volumes drive,
;;; am using unexported ccl::copy-file function.

;;; Code:

(defpackage :lolh/cl-svurl
  (:use
   :common-lisp)
  (:export
   :svurl-init
   :svurl-move-files))
(ql:quickload :quri :silent t)
(in-package :lolh/cl-svurl)


(defconstant +CLARGS+ CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*)

(defconstant +DEFAULT-SOURCE+
  (merge-pathnames
   (make-pathname :directory '(:relative "Downloads"))
   (parse-namestring "~"))
  "/Users/<name>/Downloads/")

(defconstant +DEFAULT-DESTINATION+
  (merge-pathnames
   (make-pathname :directory '(:absolute "Volumes" "Mt.Whitney" "Documents_from_Paddy" ".saved"))
   *DEFAULT-PATHNAME-DEFAULTS*))

(defconstant +DEFAULT-MOVEDUPS+
  (merge-pathnames
   (make-pathname :directory '(:absolute "Volumes" "Mt.Whitney" "Documents_from_Paddy" ".savedups"))
   *DEFAULT-PATHNAME-DEFAULTS*))


(defun as-directory (s)
  "Make sure s is a directory name, e.g., ends with /.
Ignore nil and empty strings"
  (if (or (null s)
	  (string= s "")
	  (string= s " ")
	  (char= (elt s (1- (length s))) #\/))
      s
      (concatenate 'string s "/")))


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
  (source      +DEFAULT-SOURCE+)      ; PATHNAME
  (destination +DEFAULT-DESTINATION+) ; PATHNAME
  (movedups    +DEFAULT-MOVEDUPS+)    ; PATHNAME
  files    ; LIST OF FILE-SZ STRUCTS
  sorted   ; LIST OF SORTED FILE-SZ STRUCTS
  dups     ; LIST OF FILE-NAMES
  saved    ; LIST OF URLS
  used     ; LIST OF URLS
  origins) ; LIST OF URLS


(defun ucla ()
  (princ "The command line arguments are: ")
  (prin1 +CLARGS+))


(defun svurl-init (&key source destination movedups)
  "Initialize a new SVURL structure.
Create a new structure; add files to it, then find duplicates."
  (let ((sv (make-svurl
	     :source (parse-namestring (if source (as-directory source) +DEFAULT-SOURCE+))
	     :destination (parse-namestring (if destination (as-directory destination) +DEFAULT-DESTINATION+))
	     :movedups (parse-namestring (if movedups (as-directory movedups) +DEFAULT-MOVEDUPS+)))))
    (get-files sv)
    (sort-files-find-dups sv)
    (prog2 (pprint sv) sv)))


(defun svurl-move-files (sv &key (handle-dups :move))
  (handle-dups sv handle-dups)
  (dolist (fsz (svurl-files sv))
    (let ((file (pathname (file-sz-file fsz))))
      (and
       (ccl:copy-file (merge-pathnames file (svurl-source sv))
		      (merge-pathnames file (svurl-destination sv)))
       (delete-file (merge-pathnames file (svurl-source sv)))))))
       ;; (format t "rename-file ~s ~s~%"
       ;; 	       (merge-pathnames file (svurl-source sv))
       ;; 	       (merge-pathnames file (svurl-destination sv)))))))





(defun get-files (sv)
  "Get the jp*g files present in the directory DIR of structure SVURL SV.
Place them into slot FILES along with their sizes in bytes and mod times."
  (let* ((d (svurl-source sv))
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
		 ;; NOTE: file-sz-cmp removes duplicates from svurl-files slot
		 ;; in the middle of sorting, but it appears to be okay because
		 ;; sort-files-find-dups creates a local copy of sv to start as
		 ;; the ls parameter of qs.
		 (nconc
		  (qs
		   (remove-if-not
		    #'(lambda (fsz) (minusp (file-sz-cmp fsz pivot sv))) ls))
		  (remove-if-not
		   #'(lambda (fsz) (zerop  (file-sz-cmp fsz pivot sv))) ls)
		  (qs
		   (remove-if-not
		    #'(lambda (fsz) (plusp  (file-sz-cmp fsz pivot sv))) ls)))
		 ls)))
    (setf (svurl-sorted sv) (qs (svurl-files sv)))))


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
	    (if (eq fsz pivot)
		0
		(if (file-sz-byte-cmp fsz pivot sv)
		    ;; NOTE: process-dups modifies svurl-files slot of sv
		    ;; by deleting the duplicate in the middle of sorting
		    (prog1 0 (process-dups fsz pivot sv))
		    -1))))))


(defun file-sz-byte-cmp (fsz1 fsz2 sv)
  "Compare equally-sized files to determine if they are duplicates.
Return T if the files are duplicates, or NIL if not."
  (let ((dir (svurl-source sv)))
    (with-open-file
	(s1 (merge-pathnames (pathname (file-sz-file fsz1)) dir)
	    :element-type '(unsigned-byte 8))
      (with-open-file
	  (s2 (merge-pathnames (pathname (file-sz-file fsz2)) dir)
	      :element-type '(unsigned-byte 8))
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

  (let* ((dups (svurl-dups sv))   ; alist of dup files
	 (files (svurl-files sv)) ; list of files
	 (ft (file-sz-mod fsz))   ; mod time file
	 (pt (file-sz-mod pivot)) ; mod time pivot file
	 (o (if (< ft pt) fsz pivot))   ; original file-sz (earlier mod time)
	 (d (if (> ft pt) fsz pivot))   ; duplicate file-sz (later mod time)
	 (ls ; list of dups for original; might not exist yet
	   (assoc (file-sz-file o) dups :test #'string=)))
    (if (consp ls) ; if list of dups for original already exists
        (pushnew   ; add another duplicate to it
	 (file-sz-file d)
	 (cdr ls)
	 :test #'string=)
	(setf (svurl-dups sv) ; else create new duplicate set
	      (acons
	       (file-sz-file o)
	       (list (file-sz-file d)) dups)))
    (setf (svurl-files sv) ; removing the duplicates from svurl-files in the
			   ; middle of sorting seems to work because a copy is
			   ; being used to sort
	  (remove d files))))
    

(defun handle-dups (sv how)
  ":move or :delete the duplicate files in SV according to HOW.
The duplicate files are either moved from source to movedups directory
or deleted.  The dups slot is then nil'ed."
  (let ((source (svurl-source sv))
	(movedups (svurl-movedups sv)))
    (dolist (files (svurl-dups sv))
      (dolist (file (cdr files))
	(let* ((pfile (pathname file))
	       (sfile (merge-pathnames pfile source))
	       (mfile (merge-pathnames pfile movedups)))
	  (case how
	    (:move (and (ccl:copy-file sfile mfile)
			;; rename-file does not work on attached /Volume
			;; so am using ccl::copy-file and delete-file instead
			(delete-file sfile)))
			;; (format t "~s renamed to ~s~%" sfile mfile)))
	    (:delete (and (delete-file sfile)
			  (format t "~s deleted~%" (namestring sfile)))))))))
  (setf (svurl-dups sv) nil))

    
(when +CLARGS+ (ucla)(ccl:quit))

;;; lolh/cl-svurl ends here
