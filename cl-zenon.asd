(defsystem "cl-zenon"
  :version "0.1.0"
  :author "Dumeril"
  :license "MIT"
  :depends-on ("alexandria"
	       "str"
               "jsonrpc")

  :components ((:module "src"
		:serial t
                :components
                ((:file "connection")
		 (:file "api-builder")
		 (:file "ledger")
		 (:file "stats")
		 (:file "wallet")
		 (:file "embedded/accelerator")
		 (:file "embedded/pillar")
		 (:file "embedded/plasma")
		 (:file "embedded/sentinel")
		 (:file "embedded/stake")
		 (:file "embedded/spork")
		 (:file "embedded/swap")
		 (:file "embedded/token"))))
  :description "Zenon API bindings"
  :in-order-to ((test-op (test-op "cl-zenon/tests"))))

;;; Tests need to be updated before I add them
;;(defsystem "cl-zenon/tests"
;;  :author "Dumeril"
;;  :license "MIT"
;;  :depends-on ("com.zenoninfo"
;;               "rove")
;;  :components ((:module "tests"
;;                :components
;;                ((:file "util")
;;		 (:file "api"))))
;;  :description "Test system for cl-zenon"
;;  :perform (test-op (op c) (symbol-call :rove :run c)))
