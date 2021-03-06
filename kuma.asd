;;; -*- lisp -*-
;;; $Header: kuma.asd $

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



(asdf:defsystem #:kuma
  :version "0.1"
  :license "Public Domain"
  :author "Andrea Chiumenti"
  :serial t
  :depends-on (#:iolib 
	       #:babel 
	       #:cl-ppcre 
	       #:split-sequence 
	       #:net-telent-date 
	       #:flexi-streams 
	       #:cl-fad 
	       #:salza2 
	       #:trivial-gray-streams 
	       #:thread-pool
	       #:alexandria
	       #:cl-base64)
  :components ((:module src
                        :components ((:file "package")
                                     (:file "constants" :depends-on ("package"))
				     (:file "conditions" :depends-on ("constants"))
                                     (:file "global-variables" :depends-on ("package"))
                                     (:file "mime-types" :depends-on ("package"))
                                     (:file "utils" :depends-on ("constants" "global-variables" "mime-types" "conditions"))
                                     (:file "streams" :depends-on ("utils"))
                                     (:file "parsers" :depends-on ("utils"))
				     (:file "vo" :depends-on ("utils" "parsers" "streams"))
				     (:file "bodyworkers" :depends-on ("parsers"))
                                     (:file "kuma" :depends-on ("parsers" "bodyworkers"))))))

