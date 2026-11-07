;;; -*- lexical-binding: t; -*-
;;; czm-mail-tests.el --- Tests for mail helpers

(require 'ert-x)
(require 'czm-mail)

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
