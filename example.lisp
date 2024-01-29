(defpackage #:unix-socket-example
  (:documentation "Server and client example.")
  (:use #:cl #:sb-unix-socket))

(in-package #:unix-socket-example)


;;; Server

(defun handle-incoming-connection (server client-socket peer-address)
  (bt:make-thread
   (lambda ()
     ;; Read
     (multiple-value-bind
           (buffer length address)
         (receive client-socket :length 1024)
       (declare (ignore address))
       ;; Print
       (terpri)
       (format t "~&Received ~d bytes from ~s: ~%"
               length
               (address-string peer-address))
       (write-sequence buffer *standard-output* :end length)
       ;; Send it back to the client
       (send client-socket :buffer buffer :length length)
       ;; Stop
       (shutdown client-socket :direction :io)
       (close-socket client-socket)))
   :name (format nil "~a: ~a"
                 (name server)
                 (address-string peer-address))))

(defvar *s* (make-server
             "example"
             :type :stream
             :address "/tmp/example.sock"
             :handler 'handle-incoming-connection))

#++
(progn
  (start *s*)

  (stop *s*)

  (close-socket *s*)

  ;; TODO maybe: re-create socket?
  )


;;; Client

(defun send-string (string)
  (let* ((address "/tmp/example.sock")
         (client-socket (make-unix-socket :type :stream)))
    (connect client-socket address)
    (send client-socket :buffer string)
    (multiple-value-bind
          (buffer length address)
        (receive client-socket)
      (let ((stream *standard-output*))
        (terpri stream)
        (format stream "~&Received ~d bytes from the server ~s: ~%"
                length (address-string address))
        (unless (zerop length)
          (write-sequence buffer stream :end length))
        nil))
    (close-socket client-socket)))


#++
(send-string "hi")
