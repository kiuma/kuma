;;; -*- lisp  -*-
;;; $Header: src/constants.lisp $

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

(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))

(define-constant +http-continue+ '(100 "Continue"))
(define-constant +http-switching-protocols+ '(101 "Switching Protocols"))

(define-constant +http-ok+ '(200 "Ok"))
(define-constant +http-created+ '(201 "Created"))
(define-constant +http-accepted+ '(202 "Accepted"))
(define-constant +http-non-authoritative-information+ '(203 "Non-Authoritative Information"))
(define-constant +http-no-content+ '(204 "No Content"))
(define-constant +http-reset-content+ '(205 "Reset Content"))
(define-constant +http-partial-content+ '(206 "Partial Content"))

(define-constant +http-multiple-choices+ '(300 "Multiple Choiches"))
(define-constant +http-moved-permanently+ '(301 "Moved Permanently"))
(define-constant +http-found+ '(302 "Found"))
(define-constant +http-see-other+ '(303 "See Other"))
(define-constant +http-not-modified+ '(304 "Not Modifieds"))
(define-constant +http-use-proxy+ '(305 "Use Proxy"))
(define-constant +http-temporary-redirect+ '(307 "Temporary Redirect"))

(define-constant +http-bad-request+ '(400 "Bad Request"))
(define-constant +http-unauthorized+ '(401 "Unauthorized"))
(define-constant +http-payment-required+ '(402 "Payment Required"))
(define-constant +http-forbidden+ '(403 "Forbidden"))
(define-constant +http-not-found+ '(404 "Not Found"))
(define-constant +http-method-not-allowed+ '(405 "Method Not Allowed"))
(define-constant +http-not-acceptable+ '(406 "Not Acceptable"))
(define-constant +http-proxy-authentication-required+ '(407 "Proxy Authentication Required"))
(define-constant +http-request-timeout+ '(408 "Request Timeout"))
(define-constant +http-conflict+ '(409 "Conflict"))
(define-constant +http-gone+ '(410 "Gone"))
(define-constant +http-lenght-required+ '(411 "Lenght Required"))
(define-constant +http-precondition-failed+ '(412 "Precondition Failed"))
(define-constant +http-request-entity-too-large+ '(413 "Request Entity Too Large"))
(define-constant +http-request-uri-too-large+ '(414 "Request URI Too Large"))
(define-constant +http-unsupported-media-type+ '(415 "Unsupported Media Type"))
(define-constant +http-request-range-not-satisfiable+ '(416 "Request Range Not Satisfiable"))
(define-constant +http-expectation-failed+ '(417 "Expectation Failed"))

(define-constant +http-internal-server-error+ '(500 "Internal Server Error"))
(define-constant +http-not-implemented+ '(501 "Not Implemented"))
(define-constant +http-bad-gateway+ '(502 "Bad Gateway"))
(define-constant +http-service-unavailable+ '(503 "Service Unavailable"))
(define-constant +http-gateway-timeout+ '(504 "Gateway Timeout"))
(define-constant +http-http-version-not-supported+ '(505 "HTTP Version Not Supported"))

(define-constant +http-http-results+ (append +http-continue+ +http-switching-protocols+
                                    +http-ok+ +http-created+ +http-accepted+ +http-non-authoritative-information+ 
                                    +http-no-content+ +http-reset-content+ +http-partial-content+
                                    +http-multiple-choices+ +http-moved-permanently+ +http-found+ +http-see-other+ +http-not-modified+
                                    +http-use-proxy+ +http-temporary-redirect+
                                    +http-bad-request+ +http-unauthorized+ +http-payment-required+ +http-forbidden+
                                    +http-not-found+ +http-method-not-allowed+ +http-not-acceptable+ +http-proxy-authentication-required+
                                    +http-request-timeout+ +http-conflict+ +http-gone+ +http-lenght-required+ +http-precondition-failed+
                                    +http-request-entity-too-large+ +http-request-uri-too-large+ +http-unsupported-media-type+
                                    +http-request-range-not-satisfiable+ +http-expectation-failed+
                                    +http-internal-server-error+ +http-not-implemented+ +http-bad-gateway+ +http-service-unavailable+
                                    +http-gateway-timeout+ +http-http-version-not-supported+))

(define-constant +unix-time-delta+ (encode-universal-time 0 0 0 1 1 1970))

(deftype ub8 () '(unsigned-byte 8))