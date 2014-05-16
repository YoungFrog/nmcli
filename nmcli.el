;;; nmcli.el --- Control NetworkManager via nmcli    -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nicolas Richard

;; Author: Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Keywords: convenience, tools, unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'split-escaped)
(require 'dash)

(defstruct yf/nm-connection
  name uuid type timestamp timestamp-real autoconnect readonly dbus-path)

(defun yf/nm-existing-fields ()
  (--> (shell-command-to-string "nmcli -t -f nonexistantfield c")
    (split-string it)
    (last it)
    (car it)
    (split-string it ",")))

(defvar yf/nm-connection-list nil)
(defun yf/nm-connection-list (&optional update)
  "Construct a list of the available `yf/nm-connection' "
  (or (and (not update)
           yf/nm-connection-list)
      (setq yf/nm-connection-list
            (let* ((fields '("name"
                             "uuid"
                             "type"
                             "timestamp"
                             "timestamp-real"
                             "autoconnect"
                             "readonly"
                             "dbus-path"))
                   (result)
                   (fields-props (mapcar
                                  (lambda (field)
                                    (intern (format ":%s" field)))
                                  fields)))
              (mapcar
               (lambda (connection-line)
                 (apply #'make-yf/nm-connection
                        (let ((fields fields-props)
                              (connection (split-escaped-string connection-line ?: ?\\))
                              result)
                          (while fields
                            (push (pop fields) result)
                            (push (pop connection) result))
                          (nreverse result))))
               (split-string (shell-command-to-string
                              (format
                               "nmcli -t -f %s c"
                               (mapconcat 'identity fields ",")))
                             "\n"
                             t))))))
(defun yf/nm-online-p ()
  (string-match-p
   "\\bconnected\\b"
   (shell-command-to-string
    "LANG=C nmcli -t -f state nm status")))
(defun yf/nm-connect (connection)
  (interactive (list (ido-completing-read-with-printfun
                      "Connection: "
                      (yf/nm-connection-list))))
  (shell-command (format "nmcli con up uuid %s"
                         (yf/nm-connection-uuid connection))))
(defun yf/nm-list ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "*Connection list*")
    (nmcli-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;; (defun yf/nm-list ()
;;   (interactive)
;;   (with-help-window (get-buffer-create "*Connection list*")
;;     (let ((table (etable-create-table
;;                   (mapcar
;;                    (lambda (connection)
;;                      (mapcar
;;                       (lambda (elt)
;;                         (funcall
;;                          (intern
;;                           (format "yf/nm-connection-%s" elt))
;;                          connection))
;;                       '("name"
;;                         "uuid"
;;                         "type"
;;                         "timestamp"
;;                         "timestamp-real"
;;                         "autoconnect"
;;                         "readonly"
;;                         "dbus-path")))
;;                    (yf/nm-connection-list)))))
;;       (with-current-buffer
;;           standard-output
;;         (etable-draw table  (point))))))

(defconst nmcli-list-default-format
  [("Name" 15 t)
   ("UUID"   15 t)
   ("Type"  15 t)
   ("Timestamp"     12 t)
   ("Timestamp-real"  12 t)
   ("Autoconnect" 4 t)
   ("Readonly" 4 t)
   ("Dbus-path" 25 t)]
  "Vector of fields shown by default in nmcli-mode. Each field is
  a list (string width sortable) where string is the name of a
  field as supported by nmcli.")
(defcustom nmcli-list-format nil
  "Vector of fields for nmcli-mode. Each element should be a
  string (the name of a field) or a list (string width sortable).
  See nmcli-list-default-format.")
(defun nmcli-list-format ()
  nmcli-list-default-format ;; FIXME: allow customization by using nmcli-list-format
  )
(define-derived-mode nmcli-mode tabulated-list-mode "NetworkManager Connections"
  "Major mode for manipulating NetworkManager connections"
;("NAME" "UUID" "TYPE" "TIMESTAMP" "TIMESTAMP-REAL" "AUTOCONNECT" "READONLY" "DBUS-PATH")
  (setq tabulated-list-format (nmcli-list-format))
  (setq tabulated-list-sort-key (cons "Timestamp" nil))
  (setq tabulated-list-entries #'nmcli-list-entries)
  (tabulated-list-init-header))
(defun nmcli-list-entries ()
  (mapcar #'nmcli-list--format-entry (yf/nm-connection-list)))

(defun nmcli-list--format-entry (connection)
  (list
   connection
   (vconcat
    (mapcar
     (lambda (field)
       (let ((fieldname (car field)))
         (funcall
          (intern
           (format "yf/nm-connection-%s" (downcase fieldname)))
          connection)))
     (nmcli-list-format)))))



(provide 'nmcli)
;;; nmcli.el ends here
