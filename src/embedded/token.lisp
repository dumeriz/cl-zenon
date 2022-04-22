(defpackage :zenon-api-embedded-token
  (:use :cl)
  (:nicknames :token)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-token)

(make-define-api-macro "embedded.token")

(define-api-type token
    "name" "symbol" "domain" "totalSupply" "decimals" "owner" "tokenStandard"
    "maxSupply" "isBurnable" "isMintable" "isUtility")

(define-embedded-token-api "getAll" :type token :aggregate :list)
(define-embedded-token-api "getByOwner" :args (address page count) :type token :aggregate :list)
(define-embedded-token-api "getByZts" :args (zts) :type token)
