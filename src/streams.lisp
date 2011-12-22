;;; -*- lisp -*-
;;; $Header: steams.lisp $

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

(defgeneric create-chunk (stream))

(defclass chunked-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((stream :reader chunked-stream-stream :initarg :stream)
   (chunk-size :reader chunk-size :initarg :chunk-size)
   (chunk-buffer :accessor chunk-buffer :initform nil)
   (chunk-buffer-size :accessor chunk-buffer-size :initform 0)
   (chunk-pointer :accessor chunk-pointer :initform 0)
   (stream-read-p :accessor stream-read-p :initform nil))
  (:default-initargs :chunk-size *chunk-size*))

(defmethod initialize-instance :after ((stream chunked-stream) &rest arguments)
  (declare (ignore arguments))
  (setf (slot-value stream 'chunk-size)
	(max (slot-value stream 'chunk-size) 10)))

(defmethod stream-element-type ((stream chunked-stream))
  '(unsigned-byte 8))

(defmethod create-chunk ((stream chunked-stream))
  (let ((first-chunk nil))
    (with-accessors ((seq chunk-buffer)
		     (chunk-buffer chunk-buffer)
		     (chunk-buffer-size chunk-buffer-size)
		     (chunk-pointer chunk-pointer))
	stream
      (unless chunk-buffer
	(setf first-chunk t	    
	      chunk-buffer (make-array (chunk-size stream) 
				       :element-type '(unsigned-byte 8))))
      (if (stream-read-p stream)
	  0
	  (let* ((chunk-size-length (length (format nil "~x" (length seq))))
		 (new-seq (make-array (- (length seq) 
					 chunk-size-length 
					 2 ; chunk-size CRLF
					 2 ; chunk CRLF
					 )
				      :element-type '(unsigned-byte 8)))
		 (bytes-read 0)
		 (pointer 0))
	    (setf bytes-read (read-sequence new-seq (chunked-stream-stream stream)))
	    (let ((bytes-read-seq (babel:string-to-octets 
				   (let* ((hex-bytes (format nil "~x" bytes-read))
					  (str (if first-chunk 
						  (make-http-line hex-bytes)
						  (format nil "~a~a~a" 
							  #\Return 
							  #\Linefeed
							  (make-http-line hex-bytes)))))
				     (if (> bytes-read 0)
					 str
					 (make-http-line str))) 
							  :encoding :utf-8)))
	      (setf (subseq seq pointer (length bytes-read-seq)) bytes-read-seq)
	      (incf pointer (length bytes-read-seq))
	      (if (= bytes-read 0)		  
		  (setf (stream-read-p stream) t)		  
		  (progn
		    (setf (subseq seq pointer (+ pointer bytes-read)) (subseq new-seq 0 bytes-read))
		    (incf pointer bytes-read))))	  
	    (setf chunk-pointer 0
		  chunk-buffer-size pointer))))))

(defmethod stream-read-byte ((stream chunked-stream))
  (with-accessors ((chunk-pointer chunk-pointer)
		   (stream-read-p stream-read-p)
		   (chunk-buffer chunk-buffer)
		   (chunk-buffer-size chunk-buffer-size))
      stream
    (if (and stream-read-p (= chunk-pointer chunk-buffer-size))
	:eof
	(progn
	  (unless (and chunk-buffer (< chunk-pointer chunk-buffer-size))
	    (create-chunk stream))
	  (let ((byte nil))
	    (setf byte (aref chunk-buffer chunk-pointer))
	    (incf chunk-pointer)
	    byte)))))

(defmethod stream-read-sequence ((stream chunked-stream) seq start end &key)
  (let* ((start1 (or start 0))
	 (seq-length (length seq))
	 (end1 (min (- (or end seq-length) 1)
		    (- seq-length 1))))
    (loop for i from start1 to end1
       for byte = (stream-read-byte stream)
       for bytes-read = 0
       when (eq byte :eof) return bytes-read
       do (setf (aref seq i) byte
		bytes-read (+ bytes-read 1))
       finally (return bytes-read))))

(defmethod close ((stream chunked-stream) &key abort)
  (close (chunked-stream-stream stream) :abort abort))

