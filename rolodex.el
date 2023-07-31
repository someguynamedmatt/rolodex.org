;;; rolodex.el --- link, and keep track of, personal contacts in Org-mode -*- lexical-binding: t -*-

;; Author: Matt Young <dev@mttyng.com>
;; Homepage: https://github.com/someguynamedmatt/rolodex
;; Keywords: contacts, people, rolodex
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.1")(org-ql)(ivy)(org-contacts)

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package keeps record of conversations, or mentions ("@'s") of
;; people in a rolodex file. Each person/entity is held as a top-level
;; heading in a user-defined org file (rolodex.org, by default). On
;; creation a prompt will ask for a name and an alias.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-ql)
(require 'ivy)
(require 'org-contacts)

;;;; Variables
(defcustom rolodex--main-file-name "rolodex.org"
  "The rolodex main file name (default: rolodex.org)."
  :type 'string
  :group 'rolodex)

(defvar rolodex--main-file (format "%s/%s" org-dir rolodex--main-file-name))

(defun rolodex/org-find-heading-in-file (heading file_with_full_path)
  "This defun will find an org-node headline HEADING in FILE_WITH_FULL_PATH.
When the target headline is found, return a marker to its location."
  (let (visiting m buffer)
    (catch 'found
      (setq visiting (org-find-base-buffer-visiting file_with_full_path))
      (setq buffer (or visiting (find-file-noselect file_with_full_path)))
      (setq m (org-find-exact-headline-in-buffer
               heading buffer))
      (when (and (not m) (not visiting)) (kill-buffer buffer))
      (and m (throw 'found m)))))


(defun rolodex/find-contact-interactive ()
  "This defun searches through the rolodex file for a selectable contact and
returns it if it is present."
  (print rolodex--main-file)
  (ivy-read "Contact: " (org-ql-select rolodex--main-file
                          '(level 1)
                          :action #'org-get-heading)))


(defun rolodex/create-and-insert-link (headline id)
  (insert (format "[[%s][%s]]" id headline)))

(defun rolodex/insert-discussion-link-for (person)
  (let* ((heading (org-get-heading t t t t))
         (visiting (current-buffer))
         (discussion-marker (org-find-exact-headline-in-buffer heading visiting)))
    (progn
      (let* ((rolodex-buffer (org-find-base-buffer-visiting rolodex--main-file))
             (person-marker (org-find-exact-headline-in-buffer person rolodex-buffer t)))
        (with-current-buffer
            (get-buffer-create rolodex-buffer)
          (progn
            (revert-buffer-quick)
            (goto-char person-marker)
            (org-end-of-meta-data t)
            (org-toggle-narrow-to-subtree)
            (if (not (search-forward heading nil t))
                (insert (format "- [[id:%s][%s]] (%s)\n"
                                (format "%s" (org-entry-get discussion-marker "ID"))
                                (format "%s" heading)
                                (format "%s" (org-entry-get discussion-marker "DATE_CREATED"))))
              nil)
            (org-toggle-narrow-to-subtree)
            (save-buffer)))))))

;;;###autoload
(defun rolodex/create-contact ()
  "Create a contact in the main rolodex file. User will be prompted to input the Full Name, Alias, and email."
  (interactive)
  (with-current-buffer
      (get-buffer-create (org-find-base-buffer-visiting rolodex--main-file)
                         (progn
                           (goto-char (point-max))
                           (setq name (read-from-minibuffer "Full Name: "))
                           (insert (format "\n* %s\n" (capitalize name)))
                           (setq new_id (org-id-get-create))
                           (setq alias (read-from-minibuffer "Alias: "))
                           (setq email (read-from-minibuffer "Email: "))
                           (org-set-property "alias" alias)
                           (org-set-property "email" email)
                           (setq new-person (org-get-heading t t t t))
                           (save-buffer)))))

;;;###autoload
(defun rolodex/org-insert-person ()
  "Once this is called, or bound to a key, this defun prompts the user to search for a
contact in the pre-defined rolodex file. Once found a @{contact} link is inserted
into the called-from location. This link can be followed to the contact's heading
in the rolodex file. The heading will also have a reference (link) to the called-from
location inserted into it."
  (interactive)
  (setq person (rolodex/find-contact-interactive))
  (print person)
  (progn
    (let ((file_with_name_link
           (format "%s::%s" rolodex--main-file person))
          (heading person)
          (person_marker
           (rolodex/org-find-heading-in-file person org-contacts)))
      (print person_marker)
      (if person_marker
          (progn
            (rolodex/insert-discussion-link-for person)
            (org-insert-link (format "id:%s" (org-entry-get person_marker "ID"))
                             (format "id:%s" (org-entry-get person_marker "ID"))
                             (format "@%s" (org-entry-get person_marker "alias"))))
        (with-current-buffer
            (get-buffer-create (org-find-base-buffer-visiting rolodex--main-file)
                               (progn
                                 (goto-char (point-max))
                                 (setq name (read-from-minibuffer "Full Name: "))
                                 (insert (format "\n* %s\n" (capitalize name)))
                                 (setq new_id (org-id-get-create))
                                 (setq alias (read-from-minibuffer "Alias: "))
                                 (org-set-property "alias" alias)
                                 (setq new-person (org-get-heading t t t t))
                                 (save-buffer)))
          (progn
            (format "%s" (org-insert-link
                          (format "id:%s" new_id)
                          (format "id:%s" new_id)
                          (format "@%s" alias)))
            (rolodex/insert-discussion-link-for new-person)
            (insert " ")))))))

(provide 'rolodex)
;;; rolodex.el ends here
