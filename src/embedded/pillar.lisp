(defpackage :zenon-api-embedded-pillar
  (:use :cl)
  (:nicknames :pillar)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-pillar)

(make-define-api-macro "embedded.pillar")

(define-api-type pillar-current-stats "producedMomentums" "expectedMomentums")
(define-api-type delegation-amount "name" "status" "weight")
(define-api-type reward "epoch" "znnAmount" "qsrAmount")
(define-api-type uncollected-reward "address" "znnAmount" "qsrAmount")
(define-api-type pillar
    "name" "rank" "type" "ownerAddress" "producerAddress" "withdrawAddress"
    "giveMomentumRewardPercentage" "giveDelegateRewardPercentage" "isRevocable"
    "revokeCooldown" "revokeTimestamp" ("currentStats" pillar-current-stats)
    "weight")

(define-embedded-pillar-api "getQsrRegistrationCost")
(define-embedded-pillar-api "checkNameAvailability" :args (name))
(define-embedded-pillar-api "getAll" :args (page count) :type pillar :aggregate :list)
(define-embedded-pillar-api "getByOwner" :args (owner-address) :type pillar :aggregate :array)
(define-embedded-pillar-api "getByName" :args (name) :type pillar)
(define-embedded-pillar-api "getDelegatedPillar" :args (owner-address) :type delegation-amount)
(define-embedded-pillar-api "getDepositedQsr" :args (address))
(define-embedded-pillar-api "getUncollectedReward" :args (owner-address) :type uncollected-reward)
(define-embedded-pillar-api "getFrontierRewardByPage" :args (address page count) :type reward :aggregate :list)
