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


;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-read-buffer (stream))

(defclass range-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((pathname :reader range-stream-pathname :initarg :pathname)
   (file-size :reader range-stream-file-size)
   (file-stream :reader range-stream-file-stream)
   (read-buffer :accessor range-stream-read-buffer 
		:initform (make-array *default-buffer-size* 
				      :element-type '(unsigned-byte 8)))
   (buffer-pointer :accessor buffer-pointer :initform 0)
   (read-buffer-pointer :accessor read-buffer-pointer :initform 0)
   (stream-read-p :accessor stream-read-p :initform nil)
   (begin :reader range-stream-begin)
   (end :reader range-stream-end)
   (byte-range :reader range-stream-byte-range :initarg :byte-range)))

(defmethod initialize-instance :after ((stream range-stream) &rest arguments)
  (declare (ignore arguments))
  (with-accessors ((pathname range-stream-pathname)
		   (byte-range range-stream-byte-range))
      stream
    (let ((file-size (iolib.syscalls:stat-size
		      (iolib.syscalls:stat (namestring pathname)))))
      (if (< (first byte-range) 0)
	  (setf (slot-value stream 'end) (- file-size 1)
		(slot-value stream 'begin) (+ file-size (first byte-range)))
	  (if (= 2 (length byte-range))
	      (setf (slot-value stream 'end) (second byte-range)
		    (slot-value stream 'begin) (first byte-range))
	      (setf (slot-value stream 'end) (- file-size 1)
		    (slot-value stream 'begin) (first byte-range))))
      (if (or (< (range-stream-begin stream) 0)
	      (>= (range-stream-end stream) file-size))
	  (signal-http-error +http-request-range-not-satisfiable+)
	  (setf (slot-value stream 'file-stream) (open pathname 
						       :element-type '(unsigned-byte 8))
		(slot-value stream 'file-size) file-size)))))

(defmethod stream-element-type ((stream range-stream))
  '(unsigned-byte 8))

(defmethod close ((stream range-stream) &key abort)
  (close (range-stream-file-stream stream) :abort abort))

(defmethod create-read-buffer ((stream range-stream))
  (with-accessors ((read-buffer-pointer read-buffer-pointer)
		   (buffer-pointer buffer-pointer)
		   (file-stream range-stream-file-stream)
		   (begin range-stream-begin)
		   (read-buffer range-stream-read-buffer))
      stream
    (setf read-buffer-pointer 0)
    (when (= buffer-pointer 0)
      (file-position file-stream (setf buffer-pointer begin)))
    (read-sequence read-buffer file-stream)))

(defmethod stream-read-byte ((stream range-stream))
  (with-accessors ((buffer-pointer buffer-pointer)
		   (read-buffer-pointer read-buffer-pointer)
		   (stream-read-p stream-read-p)
		   (read-buffer range-stream-read-buffer)
		   (end range-stream-end))
      stream
    (if (> buffer-pointer end)
	(progn
	  :eof)
	(progn
	  (when (or (= buffer-pointer 0) 
		    (= read-buffer-pointer (length read-buffer)))
	    (create-read-buffer stream))
	  (let ((byte nil))
	    (setf byte (aref read-buffer read-buffer-pointer))
	    (incf read-buffer-pointer)
	    (incf buffer-pointer)
	    byte)))))