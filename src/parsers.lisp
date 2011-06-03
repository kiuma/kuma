;;; -*- lisp -*-
;;; $Header: parsers.lisp $

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

(defparameter +tab-string+ (format nil "~a" #\Tab))

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun trim-white-spaces (string)
  (cl-ppcre:regex-replace "^\\s*(.*\\S)\\s*$" string "\\1"))

(defun %parse-header-value (string)
  (trim-white-spaces string))

(defun %parse-http-message (header-lines)
  (let ((val (cl-ppcre:split "\\s" (first header-lines))))
    (list :method (first val) :request-uri (second val) :http-version (third val))))

(defun %parse-headers (header-lines)
  (let ((headers nil))
    (loop for line in (rest header-lines)
       unless (zerop (length line))
       do (if (or (string-equal line +tab-string+ :end1 1)
                  (string-equal line "" :end1 1))
              (when (not (null headers))
                (nconc (first headers) (%parse-header-value line))) 
              (multiple-value-bind (ignore strings)
                  (cl-ppcre:scan-to-strings  
                   "([\\#,\\$,\\&,\\',\\*,\\+,\\-,\\.,0-9,A-Z,\\^,\\_,\\`,a-z,\\|\\~]*:)(.*)" 
                   line)
                (declare (ignore ignore))
                (setf strings (coerce strings 'list))
                (when (first strings)
                  (push (subseq (first strings) 0 (- (length (first strings)) 1)) headers)
                  (push (%parse-header-value (second strings)) headers)))))
    (reverse headers)))

(defun %parse-multi-value (string)
  (let ((values (split-sequence:split-sequence #\, string))
        (regex "([^;]*)(;q=)([0-9]+(\.[0-9]+))"))
    (map 'list #'first 
         (sort (loop for val in values
                  when val
                  collect (list (cl-ppcre:regex-replace-all regex val "\\1")
                                (multiple-value-bind (quality matched)
                                    (cl-ppcre:regex-replace-all regex val "\\3")
                                  (if matched
                                      (safe-read-from-string quality)
                                      1))))
               (lambda (a b) (> (second a) (second b)))))))