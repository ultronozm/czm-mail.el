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

(defun czm-mail-header-summary ()
  "Return a message summary based on the message headers.
The value is a list of two strings, the first and second parts of the summary.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (goto-char (point-min))
  (list
   (concat (save-excursion
	            (if (not (re-search-forward "^Date:" nil t))
		               "      "
	              ;; Match month names case-insensitively
	              (cond ((let ((case-fold-search t))
			                     (re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
					                                      (line-end-position) t))
		                    (format "%2d-%3s"
			                           (string-to-number (buffer-substring
						                                           (match-beginning 2)
						                                           (match-end 2)))
			                           (buffer-substring
			                            (match-beginning 4) (match-end 4))))
		                   ((let ((case-fold-search t))
			                     (re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
					                                      (line-end-position) t))
		                    (format "%2d-%3s"
			                           (string-to-number (buffer-substring
						                                           (match-beginning 4)
						                                           (match-end 4)))
			                           (buffer-substring
			                            (match-beginning 2) (match-end 2))))
		                   ((re-search-forward "\\(19\\|20\\)\\([0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)"
		                                       (line-end-position) t)
		                    (format "%2s%2s%2s"
			                           (buffer-substring
			                            (match-beginning 2) (match-end 2))
			                           (buffer-substring
			                            (match-beginning 3) (match-end 3))
			                           (buffer-substring
			                            (match-beginning 4) (match-end 4))))
		                   (t "??????"))))
	          "  "
	          (save-excursion
	            (let* ((field (and (re-search-forward "^From:[ \t]*" nil t)
                                (buffer-substring
                                 (1- (point))
                                 (progn
                                   (while (progn (forward-line 1)
                                                 (looking-at "[ \t]")))
                                   (forward-char -1)
                                   (skip-chars-backward " \t")
                                   (point)))))
                    ;; (field (and (re-search-forward "^From:[ \t]*" nil t)
                    ;;            (let ((raw-from (buffer-substring
                    ;;                             (1- (point))
                    ;;                             (progn
                    ;;                               (while (progn (forward-line 1)
                    ;;                                             (looking-at "[ \t]")))
                    ;;                               (forward-char -1)
                    ;;                               (skip-chars-backward " \t")
                    ;;                               (point)))))
                    ;;              ;; Decode any MIME encoding in the Field field
                    ;;              (setq raw-from (rfc2047-decode-string raw-from))
                    ;;              ;; Extract the name or address
                    ;;              (rmail-parse-address-basic raw-from))))
                    len mch lo newline)
               ;; If there are multiple lines in FIELD,
               ;; discard up to the last newline in it.
               (while (and (stringp field)
                           (setq newline (string-search "\n" field)))
                 (setq field (substring field (1+ newline))))
	              (if (or (null field)
		                     (string-match
			                     (or rmail-user-mail-address-regexp
			                         (concat "^\\("
				                                (regexp-quote (user-login-name))
				                                "\\($\\|@\\)\\|"
				                                (regexp-quote user-mail-address)
				                                "\\>\\)"))
			                     field))
		                 ;; No Field field, or it's this user.
		                 (save-excursion
		                   (goto-char (point-min))
		                   (if (not (re-search-forward "^To:[ \t]*" nil t))
			                      nil
		                     (setq field
			                          (concat "to: "
				                                 (mail-strip-quoted-names
				                                  (buffer-substring
				                                   (point)
				                                   (progn (end-of-line)
					                                         (skip-chars-backward " \t")
					                                         (point)))))))))
	              (if (null field)
		                 "                         "
		               ;; We are going to return only 25 characters of the
		               ;; address, so make sure it is RFC2047 decoded before
		               ;; taking its substring.  This is important when the address is not on the same line as the name, e.g.:
		               ;; To: =?UTF-8?Q?=C5=A0t=C4=9Bp=C3=A1n_?= =?UTF-8?Q?N=C4=9Bmec?=
		               ;; <stepnem@gmail.com>
		               (setq field (rfc2047-decode-string field))
                 ;; We cannot tolerate any leftover newlines in Field,
                 ;; as that disrupts the rmail-summary display.
                 ;; Newlines can be left in Field if it was malformed,
                 ;; e.g. had unbalanced quotes.
                 (setq field (replace-regexp-in-string "\n+" " " field))
		               (setq len (length field))
		               (setq mch (string-match "[@%]" field))
                 (let ((a (- rmail-summary-address-width 11)))
		                 (format (concat "%" (format "%s" rmail-summary-address-width) "s")
			                        (if (or (not mch) (<= len rmail-summary-address-width))
			                            (substring field (max 0 (- len rmail-summary-address-width)))
			                          (substring field
				                                    (setq lo (cond ((< (- mch a) 0) 0)
						                                                 ((< len (+ mch 11))
						                                                  (- len rmail-summary-address-width))
						                                                 (t (- mch a))))
				                                    (min len (+ lo rmail-summary-address-width))))))))))
   (concat (if (re-search-forward "^Subject:" nil t)
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
	            (re-search-forward "[\n][\n]+" nil t)
	            (buffer-substring (point) (progn (end-of-line) (point))))
	          "\n")))

(advice-add #'rmail-header-summary :override #'czm-mail-header-summary)

(provide 'czm-mail)
;;; czm-mail.el ends here
