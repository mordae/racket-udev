#lang racket/base
;
; Udev Bindings
;

(require racket/contract
         racket/promise
         racket/set)

(require "private/ffi.rkt")

(provide
  (contract-out
    (exn:fail:udev? predicate/c)
    (device? predicate/c)

    (rename make-device device (-> string? device?))

    (device-sys-path (-> device? string?))
    (device-path (-> device? (or/c #f string?)))
    (device-sys-name (-> device? (or/c #f string?)))
    (device-node (-> device? (or/c #f string?)))
    (device-initialized? (-> device? boolean?))

    (device-properties (-> device? (hash/c symbol? string?)))
    (device-links (-> device? (listof string?)))
    (device-tags (-> device? (set/c symbol?)))

    (device-changed-evt (->* ()
                             (#:subsystems (listof symbol?)
                              #:tags (listof symbol?))
                             (evt/c symbol? device?)))

    (list-devices (->* ()
                       (#:subsystems (listof symbol?)
                        #:tags (listof symbol?))
                       (listof string?)))))


;; Create just one context when it's actually needed.
(define ctx
  (delay (udev-new)))


(struct device
  (pointer))


(define (list-devices #:subsystems (subsystems null)
                      #:tags (tags null))
  (let ((enumerator (udev-enumerate-new (force ctx))))
    (for ((subsystem subsystems))
      (udev-enumerate-add-match-subsystem enumerator subsystem))

    (for ((tag tags))
      (udev-enumerate-add-match-tag enumerator tag))

    (udev-enumerate-scan-devices enumerator)
    (udev-enumerate-get enumerator)))


(define (make-device syspath)
  (device (udev-device-new-from-syspath (force ctx) syspath)))


(define (device-changed-evt #:subsystems (subsystems null)
                            #:tags (tags null))
  (let ((monitor (udev-monitor-new-from-netlink (force ctx))))
    (for ((subsystem subsystems))
      (udev-monitor-filter-add-match-subsystem-devtype monitor subsystem #f))

    (for ((tag tags))
      (udev-monitor-filter-add-match-tag monitor tag))

    (udev-monitor-enable-receiving monitor)

    (wrap-evt (udev-monitor-evt monitor)
              (λ (pointer)
                (let ((action (udev-device-get-action pointer)))
                  (values action (device pointer)))))))


(define (device-sys-path device)
  (udev-device-get-syspath (device-pointer device)))

(define (device-path device)
  (udev-device-get-devpath (device-pointer device)))

(define (device-sys-name device)
  (udev-device-get-sysname (device-pointer device)))

(define (device-node device)
  (udev-device-get-devnode (device-pointer device)))

(define (device-initialized? device)
  (udev-device-get-is-initialized (device-pointer device)))

(define (device-properties device)
  (udev-device-get-properties (device-pointer device)))

(define (device-links device)
  (udev-device-get-devlinks (device-pointer device)))

(define (device-tags device)
  (udev-device-get-tags (device-pointer device)))


; vim:set ts=2 sw=2 et:
