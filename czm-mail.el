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

;; 2. `read-file-name' is advised so that when `rmail' is called with
;; a prefix argument, it reads from `rmail-secondary-file-directory'.

;; 3. A command `czm-mail-mailrc-add-entry' for storing email aliases,
;; adapted from Dimitri Fontaine's blog:
;; https://tapoueh.org/blog/2009/09/improving-~-.mailrc-usage/).

;; 4. A command `czm-mail-refile-and-store-link' for refiling and
;; storing links in some specified target rmail file.

;; 5. Commands `czm-mail-attach-buffer-or-file' and
;; `czm-mail-compose-with-attachments' for attaching a buffer's underlying
;; file or its contents, and composing mail from the visible buffers.

;; The optional library czm-mail-mairix provides `czm-mail-mairix-menu',
;; a Transient menu for Mairix commands and a search syntax reference.

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

(defgroup czm-mail nil
  "Mail composition and Rmail helpers."
  :group 'mail
  :prefix "czm-mail-")

(declare-function rmail-summary-output "rmailsum" (&optional file-name n))
(declare-function rmail-summary-goto-msg "rmailsum" (&optional n nowarn skip-rmail))
(declare-function rmail-summary-msg-number "rmailsum" ())


;;; Message Composition

(defcustom czm-mail-debbugs-cc-default nil
  "Default address offered by `czm-mail-message-insert-debbugs-cc'."
  :type '(choice (const :tag "No default" nil) string)
  :group 'czm-mail)

(defvar-keymap czm-mail--debbugs-cc-minibuffer-map
  :parent minibuffer-local-map
  "TAB" #'mail-abbrev-insert-alias)

;;;###autoload
(defun czm-mail-message-insert-debbugs-cc (address)
  "Add an X-Debbugs-Cc header containing ADDRESS, preserving point.
Leave an existing X-Debbugs-Cc header unchanged.  Interactively, read
an address with TAB alias completion and `czm-mail-debbugs-cc-default'."
  (interactive
   (progn
     (require 'mailabbrev)
     (list (let ((minibuffer-local-map czm-mail--debbugs-cc-minibuffer-map))
             (read-string "X-Debbugs-Cc: " nil nil czm-mail-debbugs-cc-default)))))
  (save-excursion
    (message-add-header (format "X-Debbugs-Cc: %s" address))))

;;;###autoload
(defun czm-mail-message-tab ()
  "Use `mail-abbrev-insert-alias' in headers, otherwise `message-tab'."
  (interactive)
  (if (message-point-in-header-p)
      (call-interactively #'mail-abbrev-insert-alias)
    (message-tab)))

;;;###autoload
(defun czm-mail-attach-buffer-or-file
    (buffer &optional type description disposition filename)
  "Attach BUFFER's underlying file, or its contents if it has no file.
BUFFER may be a buffer or its name.  TYPE, DESCRIPTION, and DISPOSITION
are optional MIME properties, as for `mml-attach-file'.
If BUFFER visits a modified file, ask whether to save it before attaching.
Declining attaches the file's saved contents.  For a buffer without a file,
FILENAME is the suggested attachment name; prompt for it interactively.
With a prefix argument, use default MIME properties without prompting."
  (interactive
   (let* ((buffer (read-buffer "Attach from buffer: " (buffer-name) t))
          (file (buffer-file-name (get-buffer buffer)))
          (no-prompt current-prefix-arg)
          (type (if no-prompt
                    (or (and file (mm-default-file-type file))
                        "application/octet-stream")
                  (mml-minibuffer-read-type
                   (if file (file-name-nondirectory file) buffer))))
          (description (unless no-prompt
                         (mml-minibuffer-read-description)))
          (disposition
           (if no-prompt
               (if file
                   (mml-content-disposition type (file-name-nondirectory file))
                 "attachment")
             (mml-minibuffer-read-disposition
              type nil (and file (file-name-nondirectory file)))))
          (filename (unless file
                      (read-string "Filename for attachment: "
                                   (concat buffer ".txt")))))
     (list buffer type description disposition filename)))
  (let* ((source (or (get-buffer buffer)
                     (user-error "No buffer named %s" buffer)))
         (file (buffer-file-name source)))
    (if file
        (progn
          (with-current-buffer source
            (when (and (buffer-modified-p)
                       (y-or-n-p
                        (format "Buffer `%s' is modified. Save before attaching? "
                                (buffer-name source))))
              (save-buffer)))
          (mml-attach-file file type description disposition))
      (mml-attach-buffer (buffer-name source)
                         type description disposition filename))))

;;;###autoload
(defun czm-mail-compose-with-attachments (&optional arg)
  "Prepare a message with the current buffer attached.
With prefix ARG, attach all visible buffers in the selected frame instead.
Use each buffer's underlying file if it has one; otherwise use its contents.
Ask whether to save modified file buffers before attaching them."
  (interactive "P")
  (let ((buffers (if arg
                     (delete-dups (mapcar #'window-buffer (window-list)))
                   (list (current-buffer)))))
    (compose-mail)
    (save-excursion
      (rfc822-goto-eoh)
      (forward-line)
      (open-line 2)
      (dolist (buffer buffers)
        (czm-mail-attach-buffer-or-file buffer)))))

;;; Refile Functions

(defcustom czm-mail-refile-file nil
  "File to refile messages to when using `czm-mail-refile-and-store-link'."
  :type 'string
  :group 'czm-mail)

(declare-function org-store-link "org")

(defun czm-mail--rmail-store-link-to-last-message (file)
  "Store an Org link to the last message in Rmail FILE."
  (require 'ol-rmail)
  (with-current-buffer (find-file-noselect file)
    (rmail-last-message)
    (org-store-link nil t)))

;;;###autoload
(defun czm-mail-refile-and-store-link ()
  "Refile current message and store an org link to it."
  (interactive)
  (unless czm-mail-refile-file
    (user-error "Please set czm-mail-refile-file"))
  (require 'rmailout)
  (rmail-output czm-mail-refile-file)
  (czm-mail--rmail-store-link-to-last-message czm-mail-refile-file))

;;;###autoload
(defun czm-mail-rmail-summary-output-and-store-link (&optional file-name count)
  "Output COUNT summary messages to FILE-NAME and store an Org link.
The link refers to the last message appended to the destination file.
FILE-NAME defaults to `rmail-default-file'; COUNT defaults to one.
Interactively, prompt for the file and use the numeric prefix as COUNT."
  (interactive
   (progn
     (require 'rmailout)
     (list (rmail-output-read-file-name)
           (prefix-numeric-value current-prefix-arg))))
  (require 'rmailsum)
  (let ((target (or file-name rmail-default-file)))
    (rmail-summary-output target (or count 1))
    (czm-mail--rmail-store-link-to-last-message target)))

;;;###autoload
(defun czm-mail-rmail-summary-visit-message (&optional number nowarn)
  "Visit summary message NUMBER in the selected window.
NUMBER defaults to the message on the current summary line.
If NOWARN is non-nil, suppress warnings about missing messages."
  (interactive "P")
  (require 'rmailsum)
  (when (rmail-summary-goto-msg number nowarn t)
    (let ((message (rmail-summary-msg-number))
          (buffer rmail-buffer))
      (when (buffer-live-p buffer)
        (switch-to-buffer buffer)
        (rmail-show-message message)))))

;;;###autoload
(defun czm-mail-rmail-compose-buffer-p (buffer-or-name _action)
  "Return non-nil for a compose buffer displayed from Rmail.
Recognize the standard *unsent and *mail* buffer names.  This is a
`display-buffer-alist' predicate; it does not choose a display action."
  (let ((name (if (bufferp buffer-or-name)
                  (buffer-name buffer-or-name)
                buffer-or-name)))
    (and (stringp name)
         (or (string-prefix-p "*unsent" name) (string= name "*mail*"))
         (with-current-buffer (window-buffer (selected-window))
           (derived-mode-p 'rmail-mode 'rmail-summary-mode)))))

;;;###autoload
(defun czm-mail-rmail-message-buffer-p (buffer-or-name _action)
  "Return non-nil for the message buffer of the current Rmail summary.
This is a `display-buffer-alist' predicate."
  (let ((buffer (get-buffer buffer-or-name)))
    (and (derived-mode-p 'rmail-summary-mode)
         (buffer-live-p buffer)
         (eq buffer rmail-buffer))))

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
  ;; Advice for file reading
  (advice-add 'read-file-name :around #'czm-mail--read-file-advice))

(provide 'czm-mail)
;;; czm-mail.el ends here
