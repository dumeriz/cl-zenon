(defpackage :zenon-api-stats
  (:use :cl)
  (:nicknames :stats)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-stats)

(make-define-api-macro "stats")

(define-api-type os-info
    "os" "platform" "platformFamily" "platformVersion" "kernelVersion"
    "memoryTotal" "memoryFree" "numCPU" "numGoroutine")

(define-api-type peer "publicKey" "ip" "name")
(define-api-type network-info "numPeers" ("peers" peer) ("self" peer))

(define-stats-api "osInfo" :type os-info)
(define-stats-api "processInfo")
(define-stats-api "networkInfo" :type network-info)
