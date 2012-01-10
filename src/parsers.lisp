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

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun trim-white-spaces (string)
  (cl-ppcre:regex-replace "^\\s*(.*\\S)\\s*$" string "\\1"))

(defun %parse-header-value (string)
  (trim-white-spaces string))

(defun %parse-http-request-line (buffer)
  (let ((val (cl-ppcre:split "\\s+" (babel:octets-to-string 
				     buffer
				     :encoding :utf-8))))
    (list :method (first val) :request-uri (second val) :http-version (third val))))

(defun %parse-headers (buffer)
  (let ((headers nil)
	(header-lines (cl-ppcre:split "\\r\\n"
				      (babel:octets-to-string 
				       buffer
				       :encoding :utf-8))))
    (loop for line in header-lines
       unless (zerop (length line))
       do (if (cl-ppcre:all-matches "^\\s" line)
              (when (not (null headers))
                (nconc (first headers) (%parse-header-value line))) 
              (multiple-value-bind (ignore strings)
                  (cl-ppcre:scan-to-strings  
                   "([\\#\\$\\&\\'\\*\\+\\-\\.0-9A-Za-z\\^\\_\\`\\|\\~]*:)(.*)" 
                   line)
                (declare (ignore ignore))
                (setf strings (coerce strings 'list))
                (when (first strings)
		  (if headers
		      (nconc headers (list (subseq (first strings) 0 (- (length (first strings)) 1))
					   (%parse-header-value (second strings))))
		      (setf headers (list (subseq (first strings) 0 (- (length (first strings)) 1))
					  (%parse-header-value (second strings)))))))))
    headers))

(defun %parse-multi-value (string)
  (let ((values (split-sequence:split-sequence #\, string))
        (regex "([^;]*)(;\\s*q\\s*=\\s*)([0-9]+(\.[0-9]+)?)"))
    (map 'list #'(lambda (str) (trim-white-spaces (first str))) 
         (sort (loop for val in values
                  when val
                  collect (list (cl-ppcre:regex-replace-all regex val "\\1")
                                (multiple-value-bind (quality matched)
                                    (cl-ppcre:regex-replace-all regex val "\\3")
                                  (if matched
                                      (safe-read-from-string quality)
                                      1))))
               (lambda (a b) (> (second a) (second b)))))))


(defun parse-rfc1738-value (item)
  (cl-ppcre:regex-replace-all "(%[0-9,a-f,A-F]{2})+" 
			      (cl-ppcre:regex-replace-all "(&#[\\d]+;)+" item
							  (lambda (target-string start end match-start match-end reg-starts reg-ends)
							    (declare (ignore start end reg-starts reg-ends)) 
							    (string (code-char (parse-integer 
										(cl-ppcre:regex-replace-all "[^0-9]"
													    (subseq target-string 
														    match-start
														    match-end) 
													    ""))))))
			      (lambda (target-string start end match-start match-end reg-starts reg-ends)
				(declare (ignore start end reg-starts reg-ends)) 
				(babel:octets-to-string (coerce (flet ((parse-hex (token) (parse-integer (subseq token 1) :radix 16)))
								  (mapcar #'parse-hex 
									  (cl-ppcre:all-matches-as-strings 
									   "%[0-9a-fA-F]{2}"
									   (subseq target-string 
										   match-start 
										   match-end))))					 
								'(vector (unsigned-byte 8)))
							:encoding :utf-8))))

(defun parse-rfc1738-key-value (key-value)
  (loop for item in (cl-ppcre:split "=" key-value)
       collect (parse-rfc1738-value item)))

(defun parse-rfc1738 (string)
  (loop for key-value in (cl-ppcre:split "&" string)
       for parsed-key-value = (parse-rfc1738-key-value key-value)
       collect (first parsed-key-value)
       collect (second parsed-key-value)))


(defun make-x-www-form-urlencoded-hash-table (k-v-list)
  (let ((ht (make-hash-table)))
    (mapcar #'(lambda (x) (let ((key (alexandria:make-keyword (string-upcase (first x))))) 
			    (setf (gethash key ht)
				  (cons (second x) 
					(gethash key ht)))))
	    k-v-list)
    (loop for k being the hash-keys in ht using (hash-value v)
	 do (setf (gethash k ht) (reverse v))
       finally (return ht))))

(defun boundary (content-type)
  (let ((regex (cl-ppcre:create-scanner "(multipart/form-data\\s*\\;\\s*)(boundary\\s*=\\s*)(\\S*)" :case-insensitive-mode t)))
    (multiple-value-bind (replacement matched)
	  (cl-ppcre:regex-replace-all regex content-type "\\3")
      (and matched replacement))))

(defun %quoted-char-decode (char-code &key (external-format :utf-8))
  (let ((l (if (listp char-code) char-code (list char-code))))
    (format nil "~a" (octets-to-string (coerce l '(vector (unsigned-byte 8))) 
				       :external-format (if (stringp external-format) 
							    (intern (string-upcase external-format) :keyword)
							    external-format)))))

(defun %quoted-decode (string &key (external-format :utf-8) (attribute-p t))
  (let ((sentence (or (and attribute-p (cl-ppcre:regex-replace-all "_" string " "))
                      (cl-ppcre:regex-replace-all "=\\n" string ""))))
    (cl-ppcre:regex-replace-all "(=[0-9,A-F]{2})+"
                                sentence
                                #'(lambda (match register)
                                    (declare (ignore register))
                                    (%quoted-char-decode 
                                     (loop for i from 0 below (length match) by 3
                                        collect (parse-integer (subseq match (+ i 1) (+ i 3)) :radix 16))
                                     :external-format external-format))
                                :simple-calls t)))

(defun %base64-decode (string &key (external-format :utf-8))
  (if external-format
      (octets-to-string (base64:base64-string-to-usb8-array string) 
			:external-format (if (stringp external-format) 
					     (intern (string-upcase external-format) :keyword)
					     external-format))
      (base64:base64-string-to-usb8-array string)))


(defun %parse-range (string)
  (let ((regex (cl-ppcre:create-scanner "^[\\s]*bytes=([\\d]*)(-)([\\d]*)[\\s]*$"
					:case-insensitive-mode t)))
    (when string
      (multiple-value-bind (scan strings)
	  (cl-ppcre:scan-to-strings regex string)
	(declare (ignore scan))
	(unless strings
	  (signal-http-error +http-request-range-not-satisfiable+))
	(if (string-equal (aref strings 0) "-")
	    (list (* -1 (parse-integer (aref strings 1))))
	    (if (and (= (length strings) 3) (string-not-equal "" (aref strings 2)))
		(let ((from (parse-integer (aref strings 0)))
		      (to (parse-integer (aref strings 2))))
		  (if (< from to)
		      (list from to)
		      (signal-http-error +http-request-range-not-satisfiable+)))
		(list (parse-integer (aref strings 0)))))))))