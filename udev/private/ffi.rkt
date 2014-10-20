#lang racket/base
;
; Udev FFI Bindings
;

(require
  (rename-in ffi/unsafe (-> -->)))

(require racket/set
         ffi/unsafe/define
         ffi/unsafe/alloc)

(require misc1/throw)

(provide
  (all-defined-out))


(struct exn:fail:udev exn:fail
  ())


(define-ffi-definer define-scheme #f)
(define-ffi-definer define-udev (ffi-lib "libudev" '("1" "")))


(define-syntax-rule (define-ffi-wrapper ((name arg ...) proc) body ...)
  (define ((name arg ...) proc)
    (let ((wrapper (begin body ...))
          (name (object-name proc)))
      (if name (procedure-rename wrapper name) wrapper))))


(define (success? v)
  (if (integer? v) (= 0 v) v))

(define-ffi-wrapper ((check-result) proc)
  (λ args
    (unless (success? (apply proc args))
      (let ((name (or (object-name proc) 'udev)))
        (throw exn:fail:udev name "operation failed")))))

(define-ffi-wrapper ((producing-checked-result) proc)
  (λ args
    (let ((result (apply proc args)))
      (unless (success? result)
        (let ((name (or (object-name proc) 'udev)))
          (throw exn:fail:udev name "operation failed")))
      (values result))))


(define _dev_t _uint32)
(define _byte/char
  (make-ctype _byte char->integer integer->char))


(define-scheme scheme-socket-to-ports
               (_fun _long
                     _string/utf-8
                     _int
                     (in : (_ptr o _scheme))
                     (out : (_ptr o _scheme))
                     --> _void
                     --> (begin
                           (register-finalizer in close-input-port)
                           (register-finalizer out close-output-port)
                           (values in out)))
               #:c-id scheme_socket_to_ports)


(define-cpointer-type _udev-pointer)
(define-cpointer-type _udev-list-entry-pointer)
(define-cpointer-type _udev-device-pointer)
(define-cpointer-type _udev-monitor-pointer)
(define-cpointer-type _udev-enumerate-pointer)


(define-syntax-rule (define-getters container-type (name c-id value-type) ...)
  (begin
    (define-udev name (_fun container-type --> value-type) #:c-id c-id) ...))

(define-syntax-rule (define-setters container-type (name c-id value-type) ...)
  (begin
    (define-udev name
                 (_fun container-type value-type --> _int)
                 #:c-id c-id
                 #:wrap (check-result)) ...))


(define-getters _udev-list-entry-pointer
  (udev-list-entry-get-next
   udev_list_entry_get_next
   _udev-list-entry-pointer/null)

  (udev-list-entry-get-name udev_list_entry_get_name _string/utf-8)
  (udev-list-entry-get-value udev_list_entry_get_value _string/utf-8))


(define (udev-list-entry->hash list-entry)
  (if list-entry
      (hash-set (udev-list-entry->hash (udev-list-entry-get-next list-entry))
                (string->symbol (udev-list-entry-get-name list-entry))
                (udev-list-entry-get-value list-entry))
      '#hasheq()))


(define (udev-list-entry->set list-entry)
  (if list-entry
      (set-add (udev-list-entry->set (udev-list-entry-get-next list-entry))
               (string->symbol (udev-list-entry-get-name list-entry)))
      (seteq)))


(define (udev-list-entry->list list-entry)
  (if list-entry
      (cons (udev-list-entry-get-name list-entry)
            (udev-list-entry->list (udev-list-entry-get-next list-entry)))
      (list)))


(define-udev udev-unref
             (_fun _udev-pointer --> _udev-pointer/null)
             #:c-id udev_unref
             #:wrap (releaser))

(define-udev udev-new
             (_fun --> _udev-pointer/null)
             #:c-id udev_new
             #:wrap (allocator udev-unref))

(define-udev udev-device-unref
             (_fun _udev-device-pointer --> _void)
             #:c-id udev_device_unref
             #:wrap (releaser))


(define-syntax-rule (define-device-allocator name c-id type)
  (define-udev name type
               #:c-id c-id
               #:wrap (compose (allocator udev-device-unref)
                               (producing-checked-result))))

(define-device-allocator udev-device-new-from-syspath
                         udev_device_new_from_syspath
                         (_fun _udev-pointer
                               _string/utf-8
                               --> _udev-device-pointer/null))

(define-device-allocator udev-device-new-from-devnum
                         udev_device_new_from_devnum
                         (_fun _udev-pointer
                               _byte/char
                               _dev_t
                               --> _udev-device-pointer/null))

(define-device-allocator udev-device-new-from-subsystem-sysname
                         udev_device_new_from_subsystem_sysname
                         (_fun _udev-pointer
                               _symbol
                               _string/utf-8
                               --> _udev-device-pointer/null))

(define-device-allocator udev-device-new-from-device-id
                         udev_device_new_from_device_id
                         (_fun _udev-pointer
                               _string/utf-8
                               --> _udev-device-pointer/null))

(define-device-allocator udev-device-new-from-environment
                         udev_device_new_from_environment
                         (_fun _udev-pointer
                               --> _udev-device-pointer/null))

(define-device-allocator udev-device-get-parent-with-subsystem-devtype
                         udev_device_get_parent_with_subsystem_devtype
                         (_fun _udev-device-pointer
                               _symbol
                               _string/utf-8
                               --> _udev-device-pointer/null))


(define-getters _udev-device-pointer
  (udev-device-get-devpath udev_device_get_devpath _string/utf-8)
  (udev-device-get-subsystem udev_device_get_subsystem _symbol)
  (udev-device-get-devtype udev_device_get_devtype _string/utf-8)
  (udev-device-get-syspath udev_device_get_syspath _string/utf-8)
  (udev-device-get-sysname udev_device_get_sysname _string/utf-8)
  (udev-device-get-devnode udev_device_get_devnode _string/utf-8)
  (udev-device-get-driver udev_device_get_driver _string/utf-8)
  (udev-device-get-action udev_device_get_action _symbol)
  (udev-device-get-devnum udev_device_get_devnum _dev_t)
  (udev-device-get-seqnum udev_device_get_seqnum _ullong)
  (udev-device-get-is-initialized udev_device_get_is_initialized _bool)

  (udev-device-get-usec-since-initialized
   udev_device_get_usec_since_initialized
   _string/utf-8))


(define-syntax-rule (define-device-list-getters (name c-id convert) ...)
  (begin
    (define-udev name
                 (_fun _udev-device-pointer
                       --> (result : _udev-list-entry-pointer/null)
                       --> (convert result))
                 #:c-id c-id) ...))


(define-device-list-getters
  (udev-device-get-devlinks
   udev_device_get_devlinks_list_entry
   udev-list-entry->list)

  (udev-device-get-properties
   udev_device_get_properties_list_entry
   udev-list-entry->hash)

  (udev-device-get-tags
   udev_device_get_tags_list_entry
   udev-list-entry->set)

  (udev-device-get-sysattrs
   udev_device_get_sysattr_list_entry
   udev-list-entry->set))


(define-udev udev-device-get-sysattr-value
             (_fun _udev-device-pointer _symbol --> _string/utf-8)
             #:c-id udev_device_get_sysattr_value)

(define-udev udev-device-get-property-value
             (_fun _udev-device-pointer _symbol --> _string/utf-8)
             #:c-id udev_device_get_property_value)


(define-udev udev-monitor-unref
             (_fun _udev-monitor-pointer --> _udev-monitor-pointer/null)
             #:c-id udev_monitor_unref
             #:wrap (releaser))

(define-udev udev-monitor-new-from-netlink
             (_fun _udev-pointer
                   (_symbol = 'udev)
                   --> _udev-monitor-pointer/null)
             #:c-id udev_monitor_new_from_netlink
             #:wrap (allocator udev-monitor-unref))

(define-udev udev-monitor-filter-add-match-subsystem-devtype
             (_fun _udev-monitor-pointer _symbol _string/utf-8 --> _int)
             #:c-id udev_monitor_filter_add_match_subsystem_devtype
             #:wrap (check-result))

(define-udev udev-monitor-filter-add-match-tag
             (_fun _udev-monitor-pointer _symbol --> _int)
             #:c-id udev_monitor_filter_add_match_tag
             #:wrap (check-result))

(define-udev udev-monitor-enable-receiving
             (_fun _udev-monitor-pointer --> _int)
             #:c-id udev_monitor_enable_receiving
             #:wrap (check-result))

(define-udev udev-monitor-filter-update
             (_fun _udev-monitor-pointer --> _int)
             #:c-id udev_monitor_filter_update
             #:wrap (check-result))

(define-udev udev-monitor-filter-remove
             (_fun _udev-monitor-pointer --> _int)
             #:c-id udev_monitor_filter_remove
             #:wrap (check-result))

(define-device-allocator udev-monitor-receive-device
                         udev_monitor_receive_device
                         (_fun _udev-monitor-pointer
                               --> _udev-device-pointer/null))

(define-getters _udev-monitor-pointer
  (udev-monitor-get-fd udev_monitor_get_fd _int))


(define (udev-monitor-evt monitor)
  (let-values (((in out) (scheme-socket-to-ports
                           (udev-monitor-get-fd monitor)
                           "udev-monitor" 0)))
    (wrap-evt in (λ (in)
                   (udev-monitor-receive-device monitor)))))


(define-udev udev-enumerate-unref
             (_fun _udev-enumerate-pointer --> _udev-enumerate-pointer/null)
             #:c-id udev_enumerate_unref
             #:wrap (releaser))

(define-udev udev-enumerate-new
             (_fun _udev-pointer --> _udev-enumerate-pointer/null)
             #:c-id udev_enumerate_new
             #:wrap (allocator udev-enumerate-unref))

(define-setters _udev-enumerate-pointer
  (udev-enumerate-add-match-subsystem
   udev_enumerate_add_match_subsystem
   _symbol)

  (udev-enumerate-add-nomatch-subsystem
   udev_enumerate_add_nomatch_subsystem
   _symbol)

  (udev-enumerate-add-match-sysname
   udev_enumerate_add_match_sysname
   _string/utf-8)

  (udev-enumerate-add-match-tag
   udev_enumerate_add_match_tag
   _symbol)

  (udev-enumerate-add-syspath
   udev_enumerate_add_syspath
   _string/utf-8))

(define-udev udev-enumerate-add-match-sysattr
             (_fun _udev-enumerate-pointer _symbol _string/utf-8 --> _int)
             #:c-id udev_enumerate_add_match_sysattr
             #:wrap (check-result))

(define-udev udev-enumerate-add-nomatch-sysattr
             (_fun _udev-enumerate-pointer _symbol _string/utf-8 --> _int)
             #:c-id udev_enumerate_add_nomatch_sysattr
             #:wrap (check-result))

(define-udev udev-enumerate-add-match-property
             (_fun _udev-enumerate-pointer _symbol _string/utf-8 --> _int)
             #:c-id udev_enumerate_add_match_property
             #:wrap (check-result))

(define-udev udev-enumerate-add-match-is-initialized
             (_fun _udev-enumerate-pointer --> _int)
             #:c-id udev_enumerate_add_match_is_initialized
             #:wrap (check-result))

(define-udev udev-enumerate-scan-devices
             (_fun _udev-enumerate-pointer --> _int)
             #:c-id udev_enumerate_scan_devices
             #:wrap (check-result))

(define-udev udev-enumerate-scan-subsystems
             (_fun _udev-enumerate-pointer --> _int)
             #:c-id udev_enumerate_scan_subsystems
             #:wrap (check-result))

(define-udev udev-enumerate-get
             (_fun _udev-enumerate-pointer
                   --> (result : _udev-list-entry-pointer/null)
                   --> (udev-list-entry->list result))
             #:c-id udev_enumerate_get_list_entry)


; vim:set ts=2 sw=2 et:
