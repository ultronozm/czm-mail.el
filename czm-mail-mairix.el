;;; -*- lexical-binding: t; -*-
;;; czm-mail-mairix.el --- Optional Mairix search menu

;; Copyright (C) 2026 Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; URL: https://github.com/ultronozm/czm-mail.el
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

;; A Transient menu for the built-in Mairix mail search interface.
;; Loading czm-mail does not load this optional library or its dependencies.
;; Bind `czm-mail-mairix-menu' to a convenient key to use the menu.

;;; Code:

(require 'mairix)
(require 'transient)

(defconst czm-mail-mairix-syntax-help-text
  "Mairix Search Syntax:

word          : match word in message body and major headers
t:word        : match word in To: header
c:word        : match word in Cc: header
f:word        : match word in From: header
a:word        : match word in To:, Cc: or From: headers (address)
s:word        : match word in Subject: header
b:word        : match word in message body
m:word        : match word in Message-ID: header
n:word        : match word in name of attachment
F:flags       : match message flags (s=seen,r=replied,f=flagged,-=negate)
p:substring   : match substring of path
d:start-end   : match date range
z:low-high    : match messages in size range

Advanced syntax:
bs:word       : match word in Subject: header or body
s:word1,word2 : match both words in Subject:
s:word1/word2 : match either word or both words in Subject:
s:~word       : match messages not containing word in Subject:
s:substring=  : match substring in any word in Subject:
s:^substring= : match left-anchored substring in any word in Subject:
s:substring=2 : match substring with <=2 errors in any word in Subject:"
  "Quick reference for Mairix search syntax.")

;;;###autoload
(defun czm-mail-mairix-syntax-help ()
  "Display a quick reference for Mairix search syntax."
  (interactive)
  (with-help-window "*Mairix Syntax Help*"
    (princ czm-mail-mairix-syntax-help-text)))

;;;###autoload (autoload 'czm-mail-mairix-menu "czm-mail-mairix" nil t)
(transient-define-prefix czm-mail-mairix-menu ()
  "Mairix search commands."
  :info-manual "(mairix-el)Top"
  [["Search"
    ("RET" "Prompted search" mairix-search)
    ("f" "Search by sender" mairix-search-from-this-article)
    ("t" "Search thread" mairix-search-thread-this-article)]
   ["Widget search"
    ("w" "Widget search" mairix-widget-search)
    ("b" "Widget based on article" mairix-widget-search-based-on-article)]
   ["Saved searches"
    ("s" "Use saved search" mairix-use-saved-search)
    ("S" "Save last search" mairix-save-search)
    ("e" "Edit saved searches" mairix-edit-saved-searches)]]
  [:class transient-row
          ("u" "Update database" mairix-update-database)
          ("?" "Show syntax help" czm-mail-mairix-syntax-help)
          ("i" "Info docs" (lambda () (interactive) (info "(mairix-el)")))])

(provide 'czm-mail-mairix)
;;; czm-mail-mairix.el ends here
