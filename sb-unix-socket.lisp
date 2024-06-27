(defpackage #:sb-unix-socket
  (:documentation "Unix socket server and client utilities for sbcl")
  (:use #:cl)
  (:export
   #:make-unix-socket
   #:bind
   #:listen-socket
   #:accept
   #:connect
   #:receive
   #:send
   #:close-socket
   #:shutdown)
  (:export
   #:address-string)
  (:export
   #:server
   #:make-server
   #:socket
   #:address
   #:name
   #:handler
   #:start
   #:stop))

(in-package #:sb-unix-socket)


;;; Thin interface on top of sb-bsd-sockets

(defun make-unix-socket (&key type abstractp)
  "Create a local-socket (a.k.a unix domain socket).
TYPE should be one of :stream or :datagram."
  (check-type type (member :stream :datagram))
  (make-instance
   (if abstractp
       'sb-bsd-sockets:local-abstract-socket
       'sb-bsd-sockets:local-socket)
   :type type))

(defgeneric bind (socket address)
  (:method ((socket sb-bsd-sockets:local-socket) (address string))
    (sb-bsd-sockets:socket-bind socket address)))

(setf (documentation 'bind 'function)
      (documentation 'sb-bsd-sockets:socket-bind 'function))

(defgeneric listen-socket (socket backlog)
  (:method ((socket sb-bsd-sockets:local-socket) backlog)
    (sb-bsd-sockets:socket-listen socket backlog)))

(setf (documentation 'listen-socket 'function)
      (documentation 'sb-bsd-sockets:socket-listen 'function))

(defgeneric accept (socket)
  (:method ((socket sb-bsd-sockets:local-socket))
    (sb-bsd-sockets:socket-accept socket)))

(setf (documentation 'accept 'function)
      (documentation 'sb-bsd-sockets:socket-accept 'function))

(defgeneric connect (socket address)
  (:method ((socket sb-bsd-sockets:local-socket) (address string))
    (sb-bsd-sockets:socket-connect socket address)))

(setf (documentation 'connect 'function)
      (documentation 'sb-bsd-sockets:socket-connect 'function))

(defgeneric receive (socket
                     &key buffer length
                       ;; &key (:oob t) (:peek t) (:waitall t)
                       ;; (:dontwait t) (:element-type t)
                     )
  (:method ((socket sb-bsd-sockets:local-socket)
            &key buffer length)
    (sb-bsd-sockets:socket-receive
     socket buffer
     (if (or length buffer) length 1024))))

(setf (documentation 'receive 'function)
      (documentation 'sb-bsd-sockets:socket-receive 'function))

(defgeneric send (socket
                  &key buffer length address external-format)
  (:method ((socket sb-bsd-sockets:local-socket)
            &key buffer length address (external-format :utf-8))
    (sb-bsd-sockets:socket-send
     socket buffer length
     ;; :address address
     :external-format external-format)))

(setf (documentation 'send 'function)
      (documentation 'sb-bsd-sockets:socket-send 'function))

(defgeneric close-socket (socket)
  (:method ((socket sb-bsd-sockets:local-socket))
    (sb-bsd-sockets:socket-close socket)))

(setf (documentation 'close-socket 'function)
      (documentation 'sb-bsd-sockets:socket-close 'function))

(defgeneric shutdown (socket &key direction)
  (:method ((socket sb-bsd-sockets:local-socket) &key direction)
    (sb-bsd-sockets:socket-shutdown socket :direction direction)))

(setf (documentation 'shutdown 'function)
      (documentation 'sb-bsd-sockets:socket-shutdown 'function))



;;; Server class

(defclass server ()
  ((socket
    :initform nil
    :initarg :socket
    :accessor socket
    :documentation "The unix socket.")
   (address
    :initarg :address
    :accessor address
    :type string
    :documentation "The address the server is listening on. Should be a path, unless the
socket is abstract.")
   (backlog
    :initarg :backlog
    :accessor backlog
    :type (integer 1)
    :documentation "How many of pending connections are before the kernel rejects new
incoming connections.")
   (name
    :initform "unix-socket-server"
    :initarg :name
    :accessor name
    :documentation "A friendly name for this server, this is use to give nice names to the
threads spawned by the server.")
   (handler
    :initform nil
    :initarg :handler
    :accessor handler)
   (stop-p
    :initform nil
    :initarg :stop-p
    :accessor stop-p
    :documentation "A flag used to ask the server to stop accepting new connections (and
exit it's event loop)."))
  (:documentation "Class to help manage a unix socket that accepts connections."))

(defun make-server (name
                    &key (type :stream)
                      abstractp address handler
                      (backlog 1))
  "Create a local-socket (a.k.a unix domain socket).
TYPE should be one of :stream or :datagram."
  (check-type type (member :stream :datagram))
  (check-type address string)
  (make-instance
   'server
   :name name
   :socket (make-unix-socket
            :type type :abstractp abstractp)
   :address address
   :handler handler
   :backlog backlog))

(defgeneric accept-connections (server)
  (:method ((server server))
    ;; TODO save that thread into server?
    (bt:make-thread
     (lambda ()
       (loop :until (stop-p server) :do
         (multiple-value-bind (client-socket peer-address)
             ;; TODO accept is blocking, if we want to stop the thread
             ;; immediatly... we would need to wake it somehow, or
             ;; perhaps just terminate the thread.
             (accept (socket server))
           (funcall (handler server) server client-socket peer-address))))
     :name (name server))))

;; TODO (defgeneric started-p)
;; (sb-bsd-sockets:socket-open-p (socket server))
;; check if thread exists and is running

(defgeneric start (server)
  (:method ((server server))
    ;; TODO make sure the server is not already started
    (setf (stop-p server) nil)
    (bind (socket server) (address server))
    ;; close the socket if listen-socket fails
    (handler-case
         (listen-socket (socket server) (backlog server))
      (error (condition)
        (declare (ignore condition))
        (close-socket server)
        ;; TODO Do I need to delete the file too?
        ))
    (accept-connections server)))

(defgeneric stop (server)
  (:method ((server server))
    (setf (stop-p server) t)))

(defmethod send ((server server) &rest args)
  (apply #'send (socket server) args))

(defmethod close-socket ((server server))
  (close-socket (socket server)))


;;; Other utils

;; TODO read (slur) until there's nothing? (perhaps use socket-make-stream

(defun address-string (address)
  "Convert an address (vector of octets) to a string (it assumes utf-8 encoding)."
  (when address
    (if (stringp address)
        address
        (let ((strlen (position 0 address)))
          (if (zerop strlen)
              'null
              (sb-ext:octets-to-string
               address
               :external-format :utf-8
               :end strlen))))))
