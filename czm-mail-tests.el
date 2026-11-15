;;; -*- lexical-binding: t; -*-
;;; czm-mail-tests.el --- Tests for mail helpers

(require 'ert-x)
(require 'czm-mail)

(ert-deftest czm-mail-alias-completion-data ()
  "Offer annotated aliases only in address headers, without editing text."
  (require 'mailabbrev)
  (let ((mail-personal-alias-file nil)
        (mail-abbrevs (make-abbrev-table))
        (mail-abbrev-aliases-need-to-be-resolved nil))
    (define-mail-abbrev "alice-math" "Alice <alice@example.test>")
    (dolist (header '("To: " "Cc: " "X-Debbugs-Cc: " "To: other@example.test,\n "))
      (with-temp-buffer
        (message-mode)
        (insert header "alice-math\nSubject: Test\n"
                mail-header-separator "\nBody.\n")
        (goto-char (+ (length header) 6))
        (let ((text (buffer-string))
              (position (point)))
          (dotimes (_ 2)
            (pcase-let* ((`(,begin ,end ,table . ,properties)
                          (czm-mail-alias-completion-at-point))
                        (predicate (plist-get properties :predicate)))
              (should (equal (buffer-substring begin end) "alice-math"))
              (should (equal (all-completions "" table predicate) '("alice-math")))
              (should (equal (funcall (plist-get properties :annotation-function)
                                     "alice-math")
                             " Alice <alice@example.test>"))
              (should (= (point) position))
              (should (equal (buffer-string) text)))))
        (search-forward "Subject: ")
        (should-not (czm-mail-alias-completion-at-point))
        (goto-char (point-max))
        (should-not (czm-mail-alias-completion-at-point))))))

(ert-deftest czm-mail-alias-completion-expansion ()
  "Accept aliases repeatedly, including groups and partial completions."
  (require 'mailabbrev)
  (dolist (message-mail-alias-type '(abbrev nil))
    (let ((mail-personal-alias-file nil)
          (mail-abbrevs (make-abbrev-table))
          (mail-abbrev-aliases-need-to-be-resolved nil)
          (completion-styles '(basic))
          (completion-cycle-threshold nil))
      (define-mail-abbrev "alice" "alice@example.test")
      (define-mail-abbrev "alice-math" "Alice <alice@example.test>")
      (define-mail-abbrev "team-math" "alice-math, Bob <bob@example.test>")
      (with-temp-buffer
        (message-mode)
        (setq-local auto-fill-function nil)
        (czm-mail-setup)
        (insert "To: alice\nSubject: Test\n" mail-header-separator "\nBody.\n")
        (goto-char (1+ (length "To: alice")))
        (pcase-let ((`(,_ ,_ ,_ . ,properties)
                     (czm-mail-alias-completion-at-point)))
          (funcall (plist-get properties :exit-function) "alice" 'exact))
        (should (equal (buffer-substring (line-beginning-position) (point))
                       "To: alice"))
        (insert "-ma")
        (completion-at-point)
        (should (equal (buffer-substring (line-beginning-position) (point))
                       "To: Alice <alice@example.test>"))
        (insert ", team-")
        (completion-at-point)
        (should (equal (buffer-substring (line-beginning-position) (point))
                       (concat "To: Alice <alice@example.test>, "
                               "Alice <alice@example.test>, Bob <bob@example.test>")))))))

(ert-deftest czm-mail-message-debbugs-cc ()
  "Insert the header once while preserving the position in the body."
  (require 'mailabbrev)
  (let ((mail-personal-alias-file nil)
        (mail-abbrevs (make-abbrev-table)))
    (with-temp-buffer
      (message-mode)
      (insert "To: recipient@example.test\nSubject: Test\n"
              mail-header-separator "\nBody.\n")
      (goto-char (point-max))
      (let ((position (point-marker)))
        (czm-mail-message-insert-debbugs-cc "list@example.test")
        (czm-mail-message-insert-debbugs-cc "other@example.test")
        (should (= (point) position))
        (should (equal (message-fetch-field "X-Debbugs-Cc")
                       "list@example.test"))
        (goto-char (point-min))
        (should (= (how-many "^X-Debbugs-Cc:") 1))))))

(ert-deftest czm-mail-rmail-summary-workflow ()
  "Output a summary message, link to its copy, and visit another message."
  (require 'rmailsum)
  (require 'org)
  (ert-with-temp-directory directory
    (let* ((source (expand-file-name "source.rmail" directory))
           (target (expand-file-name "target.rmail" directory))
           (rmail-default-file target)
           (rmail-delete-after-output nil)
           (rmail-display-summary nil)
           (org-stored-links nil)
           buffers)
      (with-temp-file source
        (dolist (id '("first" "second"))
          (insert (format "From sender@example.test Sat Nov  7 10:00:00 2026\nFrom: sender@example.test\nTo: reader@example.test\nDate: Sat, 7 Nov 2026 10:00:00 +0000\nSubject: %s\nMessage-ID: <%s@example.test>\n\nBody.\n\n"
                          id id))))
      (with-temp-file target)
      (unwind-protect
          (save-window-excursion
            (let ((mail (find-file-noselect source)))
              (push mail buffers)
              (with-current-buffer mail
                (rmail-summary)
                (let ((summary (current-buffer)))
                  (push summary buffers)
                  (goto-char (point-min))
                  (czm-mail-rmail-summary-output-and-store-link)
                  (should (equal (caar org-stored-links)
                                 (concat "rmail:" target "#first@example.test")))
                  (push (find-buffer-visiting target) buffers)
                  (switch-to-buffer summary)
                  (czm-mail-rmail-summary-visit-message 2)
                  (should (eq (window-buffer) mail))
                  (should (= rmail-current-message 2))))))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest czm-mail-rmail-display-predicates ()
  "Match compose and message targets only in the Rmail context."
  (require 'rmailsum)
  (let ((source (generate-new-buffer " mail summary"))
        (message-buffer (generate-new-buffer " mail message")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (setq major-mode 'rmail-summary-mode)
          (setq-local rmail-buffer message-buffer)
          (should (czm-mail-rmail-compose-buffer-p "*unsent test*" nil))
          (should (czm-mail-rmail-message-buffer-p message-buffer nil))
          (should-not (czm-mail-rmail-message-buffer-p source nil))
          (setq major-mode 'fundamental-mode)
          (should-not (czm-mail-rmail-compose-buffer-p "*unsent test*" nil))
          (should-not (czm-mail-rmail-message-buffer-p message-buffer nil)))
      (kill-buffer source)
      (kill-buffer message-buffer))))

(provide 'czm-mail-tests)
;;; czm-mail-tests.el ends here
