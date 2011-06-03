;;; -*- lisp -*-
;;; $Header: buffer-utils.lisp $

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

(defun make-buffer (initial-size)
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun buffer-length (buffer)
  (length buffer))

(defun buffer-ref (buffer index)
  (aref buffer index))

(defun buffer-subseq (buffer start end)
  (let ((sub (make-array (- end start) :element-type '(unsigned-byte 8))))
    (replace sub buffer :start2 start)
    sub))

(defun buffer-search (subsequence buffer)
  (search subsequence buffer))

(defun buffer-delete-from-head (buffer size-to-remove)
  (replace buffer buffer :start2 size-to-remove)
  (decf (fill-pointer buffer) size-to-remove)
  buffer)

(defun buffer-clear (buffer)
  (setf (fill-pointer buffer) 0))

(defun buffer-append (buffer bytes size)
  (let* ((old-size (length buffer))
         (new-size (+ old-size size)))
    (loop :while (< (array-dimension buffer 0) new-size) :do
       (setf buffer (adjust-array buffer
                                  (* 2 (array-dimension buffer 0))
                                  :element-type (array-element-type buffer)
                                  :fill-pointer (fill-pointer buffer))))
    (setf (fill-pointer buffer) new-size)
    (replace buffer bytes :start1 old-size :end2 size)
    buffer))