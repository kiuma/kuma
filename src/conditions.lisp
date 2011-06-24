;;; -*- lisp -*-
;;; $Header: conditions.lisp $

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

(define-condition http-condition ()
  ((code :reader error-code :initarg :code)
   (description :reader error-description :initarg :description)
   (reason :reader error-reason :initarg :reason))
  (:report (lambda (condition stream)
	     (with-accessors ((code error-code)
			      (description error-description)
			      (reason error-reason))
		 condition
	       (format stream "~a:> ~a: ~a - ~a" 
		       (type-of condition)
		       code
		       description
		       (or reason "")))))
  (:default-initargs :reason nil))

(define-condition http-bad-request-condition (http-condition)
  ()
  (:default-initargs :code (first +http-bad-request+) :description (second +http-bad-request+)))

(define-condition http-unauthorized-condition (http-condition)
  ()
  (:default-initargs :code (first +http-unauthorized+) :description (second +http-unauthorized+)))

(define-condition http-payment-required-condition (http-condition)
  ()
  (:default-initargs :code (first +http-payment-required+) :description (second +http-payment-required+)))

(define-condition http-forbidden-condition (http-condition)
  ()
  (:default-initargs :code (first +http-forbidden+) :description (second +http-forbidden+)))

(define-condition http-not-found-condition (http-condition)
  ()
  (:default-initargs :code (first +http-not-found+) :description (second +http-not-found+)))

(define-condition http-method-not-allowed-condition (http-condition)
  ()
  (:default-initargs :code (first +http-method-not-allowed+) :description (second +http-method-not-allowed+)))

(define-condition http-not-acceptable-condition (http-condition)
  ()
  (:default-initargs :code (first +http-not-acceptable+) :description (second +http-not-acceptable+)))

(define-condition http-proxy-authentication-required-condition (http-condition)
  ()
  (:default-initargs :code (first +http-proxy-authentication-required+) :description (second +http-proxy-authentication-required+)))

(define-condition http-request-timeout-condition (http-condition)
  ()
  (:default-initargs :code (first +http-request-timeout+) :description (second +http-request-timeout+)))

(define-condition http-conflict-condition (http-condition)
  ()
  (:default-initargs :code (first +http-conflict+) :description (second +http-conflict+)))

(define-condition http-gone-condition (http-condition)
  ()
  (:default-initargs :code (first +http-gone+) :description (second +http-gone+))) 

(define-condition http-lenght-required-condition (http-condition)
  ()
  (:default-initargs :code (first +http-lenght-required+) :description (second +http-lenght-required+))) 

(define-condition http-precondition-failed-condition (http-condition)
  ()
  (:default-initargs :code (first +http-precondition-failed+) :description (second +http-precondition-failed+))) 

(define-condition http-request-entity-too-large-condition (http-condition)
  ()
  (:default-initargs :code (first +http-request-entity-too-large+) :description (second +http-request-entity-too-large+))) 

(define-condition http-request-uri-too-large-condition (http-condition)
  ()
  (:default-initargs :code (first +http-request-uri-too-large+) :description (second +http-request-uri-too-large+))) 

(define-condition http-unsupported-media-type-condition (http-condition)
  ()
  (:default-initargs :code (first +http-unsupported-media-type+) :description (second +http-unsupported-media-type+))) 

(define-condition http-request-range-not-satisfiable-condition (http-condition)
  ()
  (:default-initargs :code (first +http-request-range-not-satisfiable+) :description (second +http-request-range-not-satisfiable+))) 

(define-condition http-expectation-failed-condition (http-condition)
  ()
  (:default-initargs :code (first +http-expectation-failed+) :description (second +http-expectation-failed+))) 

(define-condition http-internal-server-error-condition (http-condition)
  ()
  (:default-initargs :code (first +http-internal-server-error+) :description (second +http-internal-server-error+))) 

(define-condition http-not-implemented-condition (http-condition)
  ()
  (:default-initargs :code (first +http-not-implemented+) :description (second +http-not-implemented+))) 

(define-condition http-bad-gateway-condition (http-condition)
  ()
  (:default-initargs :code (first +http-bad-gateway+) :description (second +http-bad-gateway+))) 

(define-condition http-service-unavailable-condition (http-condition)
  ()
  (:default-initargs :code (first +http-service-unavailable+) :description (second +http-service-unavailable+))) 

(define-condition http-gateway-timeout-condition (http-condition)
  ()
  (:default-initargs :code (first +http-gateway-timeout+) :description (second +http-gateway-timeout+))) 

(define-condition http-http-version-not-supported-condition (http-condition)
  ()
  (:default-initargs :code (first +http-http-version-not-supported+) :description (second +http-http-version-not-supported+))) 