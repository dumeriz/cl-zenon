(defpackage :zenon-api-embedded-swap
  (:use :cl)
  (:nicknames :swap)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

;; todo: hashtable in level 0 of result needs support to enable typed 'getAssets'

(in-package :zenon-api-embedded-swap)

(make-define-api-macro "embedded.swap")

(define-api-type asset "keyIdHash" "qsr" "znn")
(define-api-type amounts "qsr" "znn")
(define-api-type legacy-pillar "keyIdHash" "numPillars")

(define-embedded-swap-api "getAssetsByKeyIdHash" :args (sha256) :type asset)
(define-embedded-swap-api "getAssets")
(define-embedded-swap-api "getLegacyPillars" :type legacy-pillar :aggregate :array)
