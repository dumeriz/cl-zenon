(defpackage :zenon-api-embedded-sentinel
  (:use :cl)
  (:nicknames :sentinel)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-sentinel)

(make-define-api-macro "embedded.sentinel")

(define-api-type sentinel "owner" "registrationTimestamp" "isRevocable" "revokeCooldown" "active")
(define-api-type reward "epoch" "znnAmount" "qsrAmount")
(define-api-type uncollected-reward "address" "znnAmount" "qsrAmount")

(define-embedded-sentinel-api "getByOwner" :args (owner-address) :type sentinel)
(define-embedded-sentinel-api "getAllActive" :args (page count) :type sentinel :aggregate :list)
(define-embedded-sentinel-api "getDepositedQsr" :args (address))
(define-embedded-sentinel-api "getUncollectedReward" :args (address) :type uncollected-reward)
(define-embedded-sentinel-api "getFrontierRewardByPage" :args (address page count) :type reward)
