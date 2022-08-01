(defpackage :zenon-api-embedded-spork
  (:use :cl)
  (:nicknames :spork)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-spork)

(make-define-api-macro "embedded.spork")

(define-api-type spork "id" "name" "description" "activated" "enforcementHeight")

(define-embedded-spork-api "getAll" :args (page-index page-size) :type spork :aggregate :list)
