;;; -*- lisp -*-
;;; $Header: utils.lisp $

;;; Copyright (c) 2011, Andrea Chiumenti.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :kuma)

(defgeneric read-sequence (seq stream &key start end) (:method (seq stream &key (start 0) end) 
							(funcall 'cl:read-sequence seq stream :start start :end end)))

(defvar *tmp-path* (make-pathname :directory '(:absolute "tmp")))

(defparameter +crlf+ (babel:string-to-octets
		      (format nil "~a~a" #\Return #\Linefeed)
		      :encoding :utf-8))

(defparameter +http-header-separator+ (babel:string-to-octets
                                       (format nil "~a~a~a~a" #\Return #\Linefeed #\Return #\Linefeed)
                                       :encoding :utf-8))

(defparameter +tab-string+ (format nil "~a" #\Tab))

(defun make-http-line (string)
  (format nil "~a~c~c" string #\Return #\Newline))

(defun etag-file (file)
  (let ((namestring (or (and (pathnamep file) (namestring file)) file)))
    (when (and (fad:file-exists-p file) (not (fad:directory-exists-p file)))
      (with-accessors ((inode iolib.syscalls:stat-ino)
		       (size iolib.syscalls:stat-size)
		       (mtime iolib.syscalls:stat-mtime))
          (iolib.syscalls:stat namestring)
        (format nil "~x-~x-~x" inode size (+ +unix-time-delta+ mtime))))))

(defun get-mime (pathname)
  (or (gethash (pathname-type pathname) *mime-types*)
      "application/octet-stream"))

(defun squeezable-p (mime-type)
  (loop for regex in *squeezable-mime-types*
     for squeezable-p = nil
     until squeezable-p
     do (setf squeezable-p (cl-ppcre:scan 
			    (cl-ppcre:create-scanner regex :case-insensitive-mode t)
			    mime-type))
     return squeezable-p))