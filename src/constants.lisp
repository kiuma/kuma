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

(defconstant +http-continue+ '(100 "Continue"))
(defconstant +http-switching-protocols+ '(101 "Switching Protocols"))

(defconstant +http-ok+ '(200 "Ok"))
(defconstant +http-created+ '(201 "Created"))
(defconstant +http-accepted+ '(202 "Accepted"))
(defconstant +http-non-authoritative-information+ '(203 "Non-Authoritative Information"))
(defconstant +http-no-content+ '(204 "No Content"))
(defconstant +http-reset-content+ '(205 "Reset Content"))
(defconstant +http-partial-content+ '(206 "Partial Content"))

(defconstant +http-multiple-choices+ '(300 "Multiple Choiches"))
(defconstant +http-moved-permanently+ '(301 "Moved Permanently"))
(defconstant +http-found+ '(302 "Found"))
(defconstant +http-see-other+ '(303 "See Other"))
(defconstant +http-not-modified+ '(304 "Not Modifieds"))
(defconstant +http-use-proxy+ '(305 "Use Proxy"))
(defconstant +http-temporary-redirect+ '(307 "Temporary Redirect"))

(defconstant +http-bad-request+ '(400 "Bad Request"))
(defconstant +http-unauthorized+ '(401 "Unauthorized"))
(defconstant +http-payment-required+ '(402 "Payment Required"))
(defconstant +http-forbidden+ '(403 "Forbidden"))
(defconstant +http-not-found+ '(404 "Not Found"))
(defconstant +http-method-not-allowed+ '(405 "Method Not Allowed"))
(defconstant +http-not-acceptable+ '(406 "Not Acceptable"))
(defconstant +http-proxy-authentication-required+ '(407 "Proxy Authentication Required"))
(defconstant +http-request-timeout+ '(408 "Request Timeout"))
(defconstant +http-conflict+ '(409 "Conflict"))
(defconstant +http-gone+ '(410 "Gone"))
(defconstant +http-lenght-required+ '(411 "Lenght Required"))
(defconstant +http-precondition-failed+ '(412 "Precondition Failed"))
(defconstant +http-request-entity-too-large+ '(413 "Request Entity Too Large"))
(defconstant +http-request-uri-too-large+ '(414 "Request URI Too Large"))
(defconstant +http-unsupported-media-type+ '(415 "Unsupported Media Type"))
(defconstant +http-request-range-not-satisfiable+ '(416 "Request Range Not Satisfiable"))
(defconstant +http-expectation-failed+ '(417 "Expectation Failed"))

(defconstant +http-internal-server-error+ '(500 "Internal Server Error"))
(defconstant +http-not-implemented+ '(501 "Not Implemented"))
(defconstant +http-bad-gateway+ '(502 "Bad Gateway"))
(defconstant +http-service-unavailable+ '(503 "Service Unavailable"))
(defconstant +http-gateway-timeout+ '(504 "Gateway Timeout"))
(defconstant +http-http-version-not-supported+ '(505 "HTTP Version Not Supported"))

(defconstant +http-http-results+ (append +http-continue+ +http-switching-protocols+
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

(defconstant +unix-time-delta+ (encode-universal-time 0 0 0 1 1 1970))