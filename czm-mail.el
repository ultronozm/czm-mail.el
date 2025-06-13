;;; czm-mail.el --- Mail helper functions and tweaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/czm-mail.el
;; Package-Requires: ((emacs "29.1"))
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

;; A hodgepodge of mail-related functions and tweaks.

;; 1. `czm-mail-message-tab' gives smarter tab completion in message
;; composition buffers -- inserting aliases in header files, and
;; otherwise calling `message-tab'.

;; 2. `rmail-header-summary' is overrided to allow for longer sender
;; names and, for messages sent by us, to show their recipients.

;; 3. `read-file-name' is advised so that when `rmail' is called with
;; a prefix argument, it reads from `rmail-secondary-file-directory'.

;; 4. A command `czm-mail-mailrc-add-entry' for storing email aliases,
;; adapted from Dimitri Fontaine's blog:
;; https://tapoueh.org/blog/2009/09/improving-~-.mailrc-usage/).

;; 5. A command `czm-mail-refile-and-store-link' for refiling and
;; storing links in some specified target rmail file.

;; Sample config:

;; (keymap-set message-mode-map "TAB" #'czm-mail-message-tab)
;; (setopt czm-mail-refile-file "~/mail/scheduled.rmail")
;; (keymap-set rmail-mode-map "S" #'czm-mail-refile-and-store-link)
;; (keymap-global-set "C-c C-@" #'czm-mail-mailrc-add-entry)
;; (czm-mail-setup)
;;
;; or, with use-package:
;;
;; (use-package czm-mail
;;   :after rmail
;;   :bind
;;   ("C-c C-@" . czm-mail-mailrc-add-entry)
;;   (:map rmail-mode-map
;;         ("S" . czm-mail-refile-and-store-link))
;;   (:map message-mode-map
;;         ("TAB" . czm-mail-message-tab)
;;         ("C-c C-a" . czm-mail-insert-diff-as-attachment))
;;   :custom
;;   (czm-mail-refile-file "~/mail/scheduled.rmail")
;;   :config
;;   (czm-mail-setup))

;;; Code:

(require 'message)
(require 'mail-parse)
(require 'rmail)
(require 'rmailsum)


;;; Message Composition

;;;###autoload
(defun czm-mail-message-tab ()
  "Use `mail-abbrev-insert-alias' in headers, otherwise `message-tab'."
  (interactive)
  (if (message-point-in-header-p)
      (call-interactively #'mail-abbrev-insert-alias)
    (message-tab)))

;;; Refile Functions

(defcustom czm-mail-refile-file nil
  "File to refile messages to when using `czm-mail-refile-and-store-link'."
  :type 'string
  :group 'czm-mail)

(declare-function org-store-link "org")

;;;###autoload
(defun czm-mail-refile-and-store-link ()
  "Refile current message and store an org link to it."
  (interactive)
  (unless czm-mail-refile-file
    (user-error "Please set czm-mail-refile-file"))
  (rmail-output czm-mail-refile-file)
  (let ((buffer (find-file-noselect czm-mail-refile-file)))
    (with-current-buffer buffer
      (rmail-last-message)
      (require 'org)
      (org-store-link nil t))))

;;; Mailrc Management

;; This code is adapted from Dimitri Fontaine's blog:
;; https://tapoueh.org/blog/2009/09/improving-~-.mailrc-usage/

(defun czm-mail--bounds-of-email-address ()
  "Return begin and end position of email at point, including full name."
  (save-excursion
    (let* ((search-point (point))
           (start (save-excursion
                    (if (re-search-backward "[:,]" (line-beginning-position) t)
                        (1+ (point))
                      (line-beginning-position))))
           (end (save-excursion
                  (goto-char search-point)
                  (if (re-search-forward "[:,]" (line-end-position) t)
                      (1- (point))
                    (line-end-position)))))
      (cons start end))))

(defun czm-mail--email-address ()
  "Return email address at point."
  (let* ((bounds (czm-mail--bounds-of-email-address))
         (email-address-text
          (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (mail-header-parse-address email-address-text)))

(defun czm-mail--generate-default-alias (address)
  "Generate a default alias from an email ADDRESS.
ADDRESS should be a cons cell of (email . name) as returned by
mail-header-parse-address.  Returns a string suitable for use as an
email alias."
  (if (cdr address)
      (let* ((name (cdr address))
             (email (car address))
             (domain-parts (split-string (cadr (split-string email "@")) "\\."))
             (domain-part (if (> (length domain-parts) 1)
                              (nth (- (length domain-parts) 2) domain-parts)
                            (car domain-parts)))
             (cleaned-name (downcase (replace-regexp-in-string
                                      "[^a-zA-Z ]" ""
                                      name)))
             (dashed-name (replace-regexp-in-string " +" "-" cleaned-name)))
        (concat dashed-name "-" domain-part))
    (car address)))

;;;###autoload
(defun czm-mail-mailrc-add-entry (alias)
  "Add email at point to mail aliases file.
If ALIAS is empty, generate a default alias based on the name and domain."
  (interactive
   (let* ((addr (thing-at-point 'email-address))
          (default-alias (when addr (czm-mail--generate-default-alias addr))))
     (list (read-string (format "Alias%s: "
                                (if default-alias
                                    (format " (default %s)" default-alias)
                                  ""))
                        nil nil default-alias))))
  (let ((address (thing-at-point 'email-address))
        (buffer (find-file-noselect mail-personal-alias-file t)))
    (when address
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (if (search-forward (concat "alias " alias) nil t)
              (error "Alias %s is already present in .mailrc" alias)))
        (save-current-buffer
          (save-excursion
            (goto-char (point-max))
            (insert (format "\nalias %s \"%s <%s>\""
                            alias (cdr address) (car address)))
            (rebuild-mail-abbrevs)))))))

;;; Rmail Summary Display

(defun czm-mail-parse-date ()
  "Parse date from mail headers."
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
      (let ((newline (string-search "\n" decoded)))
        (while newline
          (setq decoded (substring decoded (1+ newline)))
          (setq newline (string-search "\n" decoded))))
      (replace-regexp-in-string "\n+" " " decoded))))

(defcustom czm-mail-summary-address-width 53 "Width of the address field in the RMAIL summary display."
  :type 'integer
  :group 'rmail)

(defun czm-mail-format-address (field)
  "Format address FIELD to fit within `czm-mail-summary-address-width'."
  (if (null field)
      "                         "
    (let* ((clean-field (czm-mail-clean-field field))
           (len (length clean-field))
           (mch (string-match "[@%]" clean-field))
           (a (- czm-mail-summary-address-width 11)))
      (format (concat "%" (format "%s" czm-mail-summary-address-width) "s")
              (if (or (not mch) (<= len czm-mail-summary-address-width))
                  (substring clean-field (max 0 (- len czm-mail-summary-address-width)))
                (let ((lo (cond ((< (- mch a) 0) 0)
                                ((< len (+ mch 11))
                                 (- len czm-mail-summary-address-width))
                                (t (- mch a)))))
                  (substring clean-field
                             lo
                             (min len (+ lo czm-mail-summary-address-width)))))))))

(defun czm-mail-get-from-or-to-field ()
  "Get From or To field depending on whether we are the sender."
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

(defun czm-mail--get-subject ()
  "Get subject from mail headers."
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

(defun czm-mail--header-summary ()
  "Return a message summary based on the message headers.
The value is a list of two strings, the first and second parts of the summary.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (goto-char (point-min))
  (list
   (concat (czm-mail-parse-date)
           "  "
           (czm-mail-format-address (czm-mail-get-from-or-to-field)))
   (concat (czm-mail--get-subject)
           "\n")))

;;; Read File Name Advice

(defun czm-mail--read-file-advice (orig-fun prompt &rest args)
  "Advice to set default directory for RMAIL files.
ORIG-FUN is the original function, PROMPT is the prompt, and ARGS are
the arguments."
  (if (and current-prefix-arg
           (equal prompt "Run rmail on RMAIL file: "))
      (let ((default-directory rmail-secondary-file-directory))
        (apply orig-fun prompt args))
    (apply orig-fun prompt args)))

;;; Patch/Diff Attachment

;;;###autoload
(defun czm-mail-insert-diff-as-attachment ()
  "Insert a diff buffer as a MIME attachment in the current mail buffer.
Searches for diff buffers and allows selecting one to attach as a patch."
  (interactive)
  (let* ((diff-buffers (cl-remove-if-not 
                        (lambda (buf)
                          (member (buffer-name buf) '("*vc-diff*" "*diff*")))
                        (buffer-list)))
         (diff-buffer-names (mapcar #'buffer-name diff-buffers))
         (selected-buffer-name (if (= 1 (length diff-buffer-names))
                                   (car diff-buffer-names)
                                 (completing-read "Attach diff buffer: " diff-buffer-names))))
    (if selected-buffer-name
        (progn
          (mml-attach-buffer selected-buffer-name
                             "text/x-patch" 
                             "changes" 
                             "attachment"
                             "changes.patch")
          (message "Patch attached from %s" selected-buffer-name))
      (message "No diff buffer found"))))

;;; Setup

(defun czm-mail-setup ()
  "Set up all czm-mail functionality."
  ;; Email parsing for mailrc functionality
  (put 'email-address 'bounds-of-thing-at-point #'czm-mail--bounds-of-email-address)
  (put 'email-address 'thing-at-point #'czm-mail--email-address)
  ;; Advice for file reading and header summary
  (advice-add 'read-file-name :around #'czm-mail--read-file-advice)
  (advice-add 'rmail-header-summary :override #'czm-mail--header-summary))

(provide 'czm-mail)
;;; czm-mail.el ends here
