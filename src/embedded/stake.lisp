(defpackage :zenon-api-embedded-stake
  (:use :cl)
  (:nicknames :stake)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-stake)

(make-define-api-macro "embedded.stake")

(define-api-type stake "amount" "weightedAmount" "startTimestamp" "expirationTimestamp" "address" "id")
(define-api-type stakes "totalAmount" "totalWeightedAmount" "count" ("list" stake))
(define-api-type uncollected-reward "address" "znnAmount" "qsrAmount")
(define-api-type reward "epoch" "znnAmount" "qsrAmount")

(define-embedded-stake-api "getEntriesByAddress" :args (address page count) :type stakes)
(define-embedded-stake-api "getUncollectedReward" :args (address) :type uncollected-reward)
(define-embedded-stake-api "getFrontierRewardByPage" :args (address page count) :type reward)
