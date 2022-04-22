(defpackage :zenon-api-ledger
  (:use :cl)
  (:nicknames :ledger)
  (:import-from :api-builder
		:make-define-api-macro
		:extract-from-hashmap
		:define-api-type
		:define-api))

(in-package :zenon-api-ledger)

(make-define-api-macro "ledger")

(define-api-type momentum
    "version" "chainIdentifier" "hash" "previousHash" "height" "timestamp"
    "data" "content" "changesHash" "publicKey" "signature" "producer")

(define-api-type momentum-acknowledged "hash" "height")

(define-api-type token
    "name" "symbol" "domain" "totalSupply" "decimals" "owner" "tokenStandard"
    "maxSupply" "isBurnable" "isMintable" "isUtility")

(define-api-type account-block
  "version" "chainIdentifier" "blockType" "hash" "previousHash" "height"
  ("momentumAcknowledged" momentum-acknowledged) "address" "toAddress"
  "amount" "tokenStandard" "fromBlockHash"
  ("descendantBlocks" account-block) "data" "fusedPlasma" "difficulty"
  "nonce" "basePlasma" "usedPlasma" "changesHash" "publicKey" "signature"
  ("token" token) ("confirmationDetail" confirmation-detail)
  ("pairedAccountBlock" account-block))

(define-api-type confirmation-detail
  "numConfirmations" "momentumHeight" "momentumHash" "momentumTimestamp")

(define-api-type detailed-momentum
    ("blocks" account-block) ("momentum" momentum))

(define-api-type balance-info "balance" ("token" token))

(defun ht-balance-info (data)
  (extract-from-hashmap data 'balance-info))

(define-api-type account-info
    "address" "accountHeight" ("balanceInfoMap" ht-balance-info))

(define-ledger-api "getFrontierAccountBlock" :args (address) :type account-block)
(define-ledger-api "getUnconfirmedBlocksByAddress" :args (address page count) :type account-block :aggregate :paged)
(define-ledger-api "getUnreceivedBlocksByAddress" :args (address page count) :type account-block :aggregate :paged)
(define-ledger-api "getAccountBlockByHash" :args (hash) :type account-block)
(define-ledger-api "getAccountBlocksByHeight" :args (address from-height count) :type account-block :aggregate :paged)
(define-ledger-api "getAccountBlocksByPage" :args (address page count) :type account-block :aggregate :paged)
(define-ledger-api "getFrontierMomentum" :type momentum)
(define-ledger-api "getMomentumBeforeTime" :args (timestamp) :type momentum)
(define-ledger-api "getMomentumsByPage" :args (page count) :type momentum :aggregate :list)
(define-ledger-api "getMomentumByHash" :args (hash) :type momentum)
(define-ledger-api "getMomentumsByHeight" :args (height count) :type momentum :aggregate :list)
(define-ledger-api "getDetailedMomentumsByHeight" :args (height count) :type detailed-momentum :aggregate :list)
(define-ledger-api "getAccountInfoByAddress" :args (address) :type account-info)

;; todo: subscriptions.
;; How to subscribe? - Intercept messages from event emitter and check for the id.
;; ->
;; (defparameter *conn* nil)
;; (event-emitter:on :open *client* (lambda (conn) (setf *conn* conn)))
;; (zi/conn:connect)
;; (event-emitter:on :message (slot-value *conn* 'jsonrpc/connection::socket)
;;                            (lambda (msg) (format t "~A~%" msg)))
