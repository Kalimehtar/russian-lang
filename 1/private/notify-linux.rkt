#lang racket/base
;; xdg-desktop-portal Notification.AddNotification, as in
;; plyer platforms/linux/notification.py NotifyDesktopPortals.
(require racket/port
         racket/system)

(provide show-notification)

(define (gvariant-escape s)
  (define src (if (string? s) s ""))
  (define out (open-output-string))
  (for ([c (in-string src)])
    (case c
      [(#\\) (write-string "\\\\" out)]
      [(#\') (write-string "\\'" out)]
      [else (write-char c out)]))
  (get-output-string out))

(define (notification-dict title body)
  (string-append
   "{'title': <'" (gvariant-escape title)
   "'>, 'body': <'" (gvariant-escape body) "'>}"))

(define (show-notification title message app-name icon timeout)
  (define gdbus (find-executable-path "gdbus"))
  (unless gdbus
    (error 'show-notification "gdbus not found"))
  (define dict (notification-dict title message))
  (define sink (open-output-nowhere))
  (define ok
    (parameterize ([current-output-port sink]
                   [current-error-port sink])
      (system* gdbus
               "call" "--session"
               "--dest" "org.freedesktop.portal.Desktop"
               "--object-path"
               "/org/freedesktop/portal/desktop"
               "--method"
               "org.freedesktop.portal.Notification.AddNotification"
               ""
               dict)))
  (unless ok
    (error 'show-notification
           "gdbus AddNotification failed"))
  (void app-name icon timeout))
