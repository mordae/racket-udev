#lang racket/base
;
; FFI Bindings
;

(require (for-syntax racket/base)
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/define
         racket/contract
         racket/provide
         racket/set)

(provide (filtered-out
           (lambda (name)
             (and (regexp-match? #rx"^(udev|exn:)" name)
                  (not (regexp-match? #rx"list-entry" name))
                  (not (regexp-match? #rx"-unref$" name))
                  (not (regexp-match? #rx"-pointer-tag$" name))
                  (not (regexp-match? #rx"-fd$" name))
                  (regexp-replace #rx"-pointer\\?$" name "?")))
           (all-defined-out)))


(define-struct/contract (exn:fail:udev exn:fail) ())

(define libudev (ffi-lib "libudev" '("1" "")))

(define-syntax (define-udev stx)
  (syntax-case stx ()
    ((_ name type)
     (with-syntax ((symbol (regexp-replace* #rx"[?!]"
                             (regexp-replace* #rx"[-/]"
                               (symbol->string (syntax-e #'name)) "_") "")))
       #'(define name (get-ffi-obj symbol libudev type))))))

(define-syntax (define-scheme stx)
  (syntax-case stx ()
    ((_ name type)
     (with-syntax ((symbol (regexp-replace* #rx"[?!]"
                             (regexp-replace* #rx"[-/]"
                               (symbol->string (syntax-e #'name)) "_") "")))
       #'(define name (get-ffi-obj symbol #f type))))))


(define (with-finalizer result finalizer)
  (if result
    (register-finalizer result finalizer)
    (raise (exn:fail:udev "udev call failed" (current-continuation-marks))))
  result)

(define (check-result result)
  (unless (= 0 result)
    (raise (exn:fail:udev "udev call failed" (current-continuation-marks)))))

(define (with-checked-result result)
  (unless result
    (raise (exn:fail:udev "udev call failed" (current-continuation-marks))))
  result)


(define _byte/char
  (make-ctype _byte char->integer integer->char))

(define _dev_t _uint32)


(define-scheme scheme-socket-to-ports
               (_fun _long
                     _string/utf-8
                     _int
                     (inp : (_ptr o _scheme))
                     (outp : (_ptr o _scheme))
                     --> _void
                     --> (begin
                           (register-finalizer inp close-input-port)
                           (register-finalizer outp close-output-port)
                           (values inp outp))))


(define-cpointer-type _udev-pointer)
(define-cpointer-type _udev-list-entry-pointer)
(define-cpointer-type _udev-device-pointer)
(define-cpointer-type _udev-monitor-pointer)
(define-cpointer-type _udev-enumerate-pointer)


(define-syntax-rule (define-getter-definer definer-name container-type)
  (define-syntax-rule (definer-name name type)
    (define-udev name (_fun container-type --> type))))

(define-syntax-rule (define-setter-definer definer-name container-type)
  (define-syntax-rule (definer-name name type)
    (define-udev name (_fun container-type
                            type
                            --> (result : _int)
                            --> (check-result result)))))


(define-udev udev-list-entry-get-next
             (_fun _udev-list-entry-pointer
                   --> _udev-list-entry-pointer/null))

(define-getter-definer define-list-entry-getter _udev-list-entry-pointer/null)
(define-list-entry-getter udev-list-entry-get-name _string/utf-8)
(define-list-entry-getter udev-list-entry-get-value _string/utf-8)


(define/contract (udev-list-entry->hash list-entry)
                 (-> (or/c #f udev-list-entry-pointer?)
                     (hash/c symbol? (or/c #f string?)))
  (if list-entry
    (hash-set (udev-list-entry->hash (udev-list-entry-get-next list-entry))
              (string->symbol (udev-list-entry-get-name list-entry))
              (udev-list-entry-get-value list-entry))
    '#hasheq()))

(define/contract (udev-list-entry->set list-entry)
                 (-> (or/c #f udev-list-entry-pointer?) (set/c symbol?))
  (if list-entry
    (set-add (udev-list-entry->set (udev-list-entry-get-next list-entry))
             (string->symbol (udev-list-entry-get-name list-entry)))
    (set)))

(define/contract (udev-list-entry->list list-entry)
                 (-> (or/c #f udev-list-entry-pointer?) (listof string?))
  (if list-entry
    (cons (udev-list-entry-get-name list-entry)
          (udev-list-entry->list (udev-list-entry-get-next list-entry)))
    (list)))


(define-udev udev-unref
             (_fun _udev-pointer
                   --> _udev-pointer/null))

(define-udev udev-new
             (_fun --> (result : _udev-pointer/null)
                   --> (with-finalizer result udev-unref)))


(define-getter-definer define-device-getter _udev-device-pointer)

(define-device-getter udev-device-unref _udev-device-pointer/null)

(define-udev udev-device-new-from-syspath
             (_fun _udev-pointer
                   _string/utf-8
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-new-from-devnum
             (_fun _udev-pointer
                   _byte/char
                   _dev_t
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-new-from-subsystem-sysname
             (_fun _udev-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-new-from-device-id
             (_fun _udev-pointer
                   _string/utf-8
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-new-from-environment
             (_fun _udev-pointer
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-get-parent
             (_fun _udev-device-pointer
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-udev udev-device-get-parent-with-subsystem-devtype
             (_fun _udev-device-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define-device-getter udev-device-get-devpath _string/utf-8)
(define-device-getter udev-device-get-subsystem _string/utf-8)
(define-device-getter udev-device-get-devtype _string/utf-8)
(define-device-getter udev-device-get-syspath _string/utf-8)
(define-device-getter udev-device-get-sysname _string/utf-8)
(define-device-getter udev-device-get-devnode _string/utf-8)
(define-device-getter udev-device-get-driver _string/utf-8)
(define-device-getter udev-device-get-action _string/utf-8)
(define-device-getter udev-device-get-devnum _dev_t)
(define-device-getter udev-device-get-seqnum _ullong)
(define-device-getter udev-device-get-usec-since-initialized _ullong)

(define-udev udev-device-get-is-initialized
             (_fun _udev-device-pointer
                   --> _bool))

(define-syntax-rule (define-device-list-getter name convertor)
  (define-udev name
               (_fun _udev-device-pointer
                     --> (result : _udev-list-entry-pointer/null)
                     --> (convertor result))))

(define-device-list-getter udev-device-get-devlinks-list-entry
                           udev-list-entry->hash)
(define-device-list-getter udev-device-get-properties-list-entry
                           udev-list-entry->hash)
(define-device-list-getter udev-device-get-tags-list-entry
                           udev-list-entry->set)
(define-device-list-getter udev-device-get-sysattr-list-entry
                           udev-list-entry->set)

(define udev-device-get-devlinks udev-device-get-devlinks-list-entry)
(define udev-device-get-properties udev-device-get-properties-list-entry)
(define udev-device-get-tags udev-device-get-tags-list-entry)
(define udev-device-get-sysattr udev-device-get-sysattr-list-entry)

(define-udev udev-device-get-sysattr-value
             (_fun _udev-device-pointer
                   _string/utf-8
                   --> _string/utf-8))

(define-udev udev-device-get-property-value
             (_fun _udev-device-pointer
                   _string/utf-8
                   --> _string/utf-8))


(define-getter-definer define-monitor-getter _udev-monitor-pointer)

(define-udev udev-monitor-new-from-netlink
             (_fun _udev-pointer
                   (_symbol = 'udev)
                   --> (result : _udev-monitor-pointer/null)
                   --> (with-finalizer result udev-monitor-unref)))

(define-udev udev-monitor-filter-add-match-subsystem-devtype
             (_fun _udev-monitor-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-monitor-filter-add-match-tag
             (_fun _udev-monitor-pointer
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-monitor-getter udev-monitor-unref _udev-monitor-pointer/null)
(define-monitor-getter udev-monitor-get-fd _int)

(define-udev udev-monitor-enable-receiving!
             (_fun _udev-monitor-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-monitor-filter-update!
             (_fun _udev-monitor-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-monitor-filter-remove!
             (_fun _udev-monitor-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-monitor-receive-device
             (_fun _udev-monitor-pointer
                   --> (result : _udev-device-pointer/null)
                   --> (with-finalizer result udev-device-unref)))

(define (udev-monitor-evt monitor)
  (let-values (((in out) (scheme-socket-to-ports
                           (udev-monitor-get-fd monitor)
                           "udev-monitor"
                           0)))
    (wrap-evt in (lambda (in)
                   (udev-monitor-receive-device monitor)))))


(define-setter-definer define-enumerate-setter _udev-enumerate-pointer)
(define-getter-definer define-enumerate-getter _udev-enumerate-pointer)

(define-udev udev-enumerate-new
             (_fun _udev-pointer
                   --> (result : _udev-enumerate-pointer/null)
                   --> (with-finalizer result udev-enumerate-unref)))

(define-enumerate-getter udev-enumerate-unref _udev-enumerate-pointer/null)

(define-enumerate-setter udev-enumerate-add-match-subsystem! _string/utf-8)
(define-enumerate-setter udev-enumerate-add-nomatch-subsystem! _string/utf-8)

(define-udev udev-enumerate-add-match-sysattr!
             (_fun _udev-enumerate-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-enumerate-add-nomatch-sysattr!
             (_fun _udev-enumerate-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-enumerate-add-match-property!
             (_fun _udev-enumerate-pointer
                   _string/utf-8
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-enumerate-setter udev-enumerate-add-match-sysname! _string/utf-8)
(define-enumerate-setter udev-enumerate-add-match-tag! _string/utf-8)
(define-enumerate-setter udev-enumerate-add-match-parent! _udev-device-pointer)
(define-enumerate-setter udev-enumerate-add-syspath! _string/utf-8)

(define-udev udev-enumerate-add-match-is-initialized!
             (_fun _udev-enumerate-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-enumerate-scan-devices!
             (_fun _udev-enumerate-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-enumerate-scan-subsystems!
             (_fun _udev-enumerate-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-udev udev-enumerate-get-list-entry
             (_fun _udev-enumerate-pointer
                   --> (result : _udev-list-entry-pointer/null)
                   --> (udev-list-entry->list result)))

(define udev-enumerate-get udev-enumerate-get-list-entry)


; vim:set ts=2 sw=2 et:
