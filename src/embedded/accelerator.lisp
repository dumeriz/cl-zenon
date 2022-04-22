(defpackage :zenon-api-embedded-accelerator
  (:use :cl)
  (:nicknames :accelerator)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-embedded-accelerator)

(make-define-api-macro "embedded.accelerator")

(define-api-type votes "no" "yes" "total" "id")

(define-api-type pillar-vote "id" "name" "vote")

(define-api-type project-phase "id" "projectID" "name" "description" "url"
		 "znnFundsNeeded" "qsrFundsNeeded" "creationTimestamp" "acceptedTimestamp"
		 "status")

(define-api-type phases ("phase" project-phase) ("votes" votes))

(define-api-type accelerator-project "id" "owner" "name" "description" "url"
		 "znnFundsNeeded" "qsrFundsNeeded" "creationTimestamp" "lastUpdateTimestamp"
		 "status" "phaseIds" ("votes" votes) ("phases" phases))

(define-embedded-accelerator-api "getAll" :args (page count) :type accelerator-project :aggregate :paged)
(define-embedded-accelerator-api "getProjectById" :args (id) :type accelerator-project)
(define-embedded-accelerator-api "getPhaseById" :args (id) :type project-phase)
(define-embedded-accelerator-api "getVoteBreakdown" :args (id) :type votes)
;; needs to test support for array args first
;(define-embedded-accelerator-api "getPillarVotes" :args (name hashes) :type pillar-vote :aggregate :array)
