(defpackage :zenon-node-connection
  (:use :cl)
  (:import-from :jsonrpc
		:make-client
		:client-connect
		:client-disconnect
		:call)
  (:nicknames :zenon/conn)
  (:export :with-node-connection
	   :with-node-at
	   :*connection*))

(in-package :zenon-node-connection)

(defparameter *connection* "ws://127.0.0.1:35998")
(defparameter *client* (jsonrpc:make-client))

(defun connect (&optional endpoint)
  (let ((conn (if endpoint endpoint *connection*)))
    (format t "Connecting to ~A~%" conn)
    (if (str:starts-with? "http" conn)
	(jsonrpc:client-connect *client* :url conn :mode :tcp)
	(jsonrpc:client-connect *client* :url conn :mode :websocket))))

(defun disconnect ()
  (format t "Disconnecting~%")
  (jsonrpc:client-disconnect *client*))

(defmacro with-node-connection (var &body body)
  `(let ((,var *client*)
	 (connected))
     (unwind-protect (progn
		       (connect *connection*)
		       (setf connected t)
		       ,@body)
       (when connected (disconnect)))))

(defmacro with-node-at (var node-address &body body)
  `(let ((*connection* ,node-address))
     (handler-case
	 (with-node-connection ,var
	   (progn ,@body))
       ;; todo: this is not nice error handling
       (usocket:socket-error (c)
	 (format t "~A~%" c))
       (jsonrpc:jsonrpc-error (c)
	 (format t "~A~%" c)))))

