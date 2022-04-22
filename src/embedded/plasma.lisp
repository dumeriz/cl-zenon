(defpackage :zenon-api-embedded-plasma
  (:use :cl)
  (:nicknames :plasma)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

;; todo: add support for json-object argument (or typed args?)

(in-package :zenon-api-embedded-plasma)

(make-define-api-macro "embedded.plasma")

(define-api-type plasma "currentPlasma" "maxPlasma" "qsrAmount")
(define-api-type fusion-entry "qsrAmount" "beneficiary" "expirationHeight" "id")
(define-api-type fusion-entries "count" ("list" fusion-entry) "qsrAmount")

;; Need to assign a different name because 'get' is taken by common-lisp
(define-api get-plasma "embedded.plasma.get" :args (address) :type plasma)
(define-embedded-plasma-api "getEntriesByAddress" :args (address page count) :type fusion-entries)

;; not working yet, see top
;;(define-embedded-plasma api "getRequiredPoWForAccountBlock" :args (:json address block-type to-address data))
