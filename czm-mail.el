;;; czm-mail.el --- mail helper functions and tweaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; Keywords: mail, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'message)

(defun czm-mail-message-tab ()
  "Call `mail-abbrev-insert-alias' in address fields.
Otherwise, call `message-tab'."
  (interactive)
  (if (and (message-point-in-header-p)
           (save-excursion
             (beginning-of-line)
             (looking-at "^\\([A-Za-z]-\\)?\\(To\\|Cc\\|Bcc\\|From\\|Reply-to\\):")))
      (call-interactively #'mail-abbrev-insert-alias)
    (message-tab)))

(require 'rmailsum)

;; (defun rmail-parse-address-basic (from)
;;   "Extract name from email address FROM.
;; Returns name if found, otherwise returns the email address."
;;   (if (string-match "\\([^<]*\\)<\\([^>]+\\)>" from)
;;       (let ((name (match-string 1 from))
;;             (addr (match-string 2 from)))
;;         ;; If name is empty or just whitespace, return addr
;;         (if (string-match "\\`[ \t]*\\'" name)
;;             addr
;;           ;; Otherwise return name with any trailing whitespace removed
;;           (replace-regexp-in-string "[ \t]*\\'" "" name)))
;;     from))

(defvar rmail-summary-address-width 53)

(defun czm-mail-parse-date ()
  "Parse and format the date from mail headers."
  (save-excursion
    (if (not (re-search-forward "^Date:" nil t))
        "      "
      (let ((case-fold-search t))
        (cond
         ;; Format: DD-MMM
         ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
                             (line-end-position) t)
          (format "%2d-%3s"
                  (string-to-number (buffer-substring
                                     (match-beginning 2)
                                     (match-end 2)))
                  (buffer-substring
                   (match-beginning 4) (match-end 4))))
         ;; Format: MMM-DD
         ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
                             (line-end-position) t)
          (format "%2d-%3s"
                  (string-to-number (buffer-substring
                                     (match-beginning 4)
                                     (match-end 4)))
                  (buffer-substring
                   (match-beginning 2) (match-end 2))))
         ;; Format: YYYY-MM-DD
         ((re-search-forward "\\(19\\|20\\)\\([0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)"
                             (line-end-position) t)
          (format "%2s%2s%2s"
                  (buffer-substring
                   (match-beginning 2) (match-end 2))
                  (buffer-substring
                   (match-beginning 3) (match-end 3))
                  (buffer-substring
                   (match-beginning 4) (match-end 4))))
         (t "??????"))))))

(defun czm-mail-clean-field (field)
  "Clean up a mail header FIELD by handling newlines and RFC2047 decoding."
  (when field
    (let ((decoded (rfc2047-decode-string field)))
      ;; Handle multiple lines, discard up to the last newline
      (let ((newline (string-search "\n" decoded)))
        (while newline
          (setq decoded (substring decoded (1+ newline)))
          (setq newline (string-search "\n" decoded))))
      ;; Remove any remaining newlines
      (replace-regexp-in-string "\n+" " " decoded))))

(defun czm-mail-format-address (field)
  "Format address FIELD to fit within `rmail-summary-address-width'."
  (if (null field)
      "                         "
    (let* ((clean-field (czm-mail-clean-field field))
           (len (length clean-field))
           (mch (string-match "[@%]" clean-field))
           (a (- rmail-summary-address-width 11)))
      (format (concat "%" (format "%s" rmail-summary-address-width) "s")
              (if (or (not mch) (<= len rmail-summary-address-width))
                  (substring clean-field (max 0 (- len rmail-summary-address-width)))
                (let ((lo (cond ((< (- mch a) 0) 0)
                                ((< len (+ mch 11))
                                 (- len rmail-summary-address-width))
                                (t (- mch a)))))
                  (substring clean-field
                             lo
                             (min len (+ lo rmail-summary-address-width)))))))))

(defun czm-mail-get-from-or-to-field ()
  "Get either From or To field depending on whether the From field is the current user."
  (save-excursion
    (goto-char (point-min))
    (let* ((from (and (re-search-forward "^From:[ \t]*" nil t)
                      (buffer-substring
                       (1- (point))
                       (progn
                         (while (progn (forward-line 1)
                                       (looking-at "[ \t]")))
                         (forward-char -1)
                         (skip-chars-backward " \t")
                         (point)))))
           (from-stripped (mail-strip-quoted-names from)))
      (if (or (null from)
              (string-match
               (or rmail-user-mail-address-regexp
                   (concat "^\\("
                           (regexp-quote (user-login-name))
                           "\\($\\|@\\)\\|"
                           (regexp-quote user-mail-address)
                           "\\>\\)"))
               from-stripped))
          ;; If From is current user, get To field instead
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^To:[ \t]*" nil t)
              (let ((to (buffer-substring
                         (point)
                         (progn (end-of-line)
                                (skip-chars-backward " \t")
                                (point)))))
                (concat "to: " to))))
        from))))

(defun czm-mail-get-subject ()
  "Get the subject line from mail headers."
  (save-excursion
    (if (re-search-forward "^Subject:" nil t)
        (let (pos str)
          (skip-chars-forward " \t")
          (setq pos (point))
          (forward-line 1)
          (setq str (buffer-substring pos (1- (point))))
          (while (looking-at "[ \t]")
            (setq str (concat str " "
                              (buffer-substring (match-end 0)
                                                (line-end-position))))
            (forward-line 1))
          str)
      (when (re-search-forward "[\n][\n]+" nil t)
        (buffer-substring (point) (progn (end-of-line) (point)))))))

(defun czm-mail-header-summary ()
  "Return a message summary based on the message headers.
The value is a list of two strings, the first and second parts of the summary.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (goto-char (point-min))
  (list
   (concat (czm-mail-parse-date)
           "  "
           (czm-mail-format-address (czm-mail-get-from-or-to-field)))
   (concat (czm-mail-get-subject)
           "\n")))

(advice-add #'rmail-header-summary :override #'czm-mail-header-summary)

(defun czm-mail-read-file-advice (orig-fun prompt &rest args)
  (if (and current-prefix-arg
           (equal prompt "Run rmail on RMAIL file: "))
      (let ((default-directory rmail-secondary-file-directory))
        (apply orig-fun prompt args))
    (apply orig-fun prompt args)))

(advice-add 'read-file-name :around #'czm-mail-read-file-advice)

(provide 'czm-mail)
;;; czm-mail.el ends here
