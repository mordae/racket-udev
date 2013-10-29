#lang racket/base
;
; Udev Bindings
;

(require racket/contract
         racket/promise)

(require "private/ffi.rkt")

(provide list-devices
         device-changed-evt
         device-info
         exn:fail:udev?)


;; Create just one context when it's actually needed.
(define ctx
  (delay (udev-new)))


(define/contract (list-devices #:subsystem (subsystem #f))
                 (->* () (#:subsystem string?)
                      (hash/c string? (hash/c symbol? any/c)))
  (let ((enumerator (udev-enumerate-new (force ctx))))
    (when subsystem
      (udev-enumerate-add-match-subsystem! enumerator subsystem))
    (udev-enumerate-scan-devices! enumerator)
    (for/hash ((syspath (udev-enumerate-get enumerator)))
      (let ((device (udev-device-new-from-syspath (force ctx) syspath)))
        (values syspath (udev-device-get-properties device))))))


(define/contract (device-info syspath)
                 (-> string? (hash/c symbol? any/c))
  (udev-device-get-properties
    (udev-device-new-from-syspath (force ctx) syspath)))


(define/contract (device-changed-evt)
                 (-> evt?)
  (let ((monitor (udev-monitor-new-from-netlink (force ctx))))
    (udev-monitor-enable-receiving! monitor)
    (wrap-evt (udev-monitor-evt monitor)
              (lambda (device)
                (hash-set (udev-device-get-properties device)
                          'ACTION (udev-device-get-action device))))))


; vim:set ts=2 sw=2 et:
