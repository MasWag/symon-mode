;;; symon-mode.el --- Major mode for SyMon using Tree-sitter -*- lexical-binding: t; -*-

;; Author: Masaki Waga <masakiwaga@gmail.com>
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/maswag/symon-mode

;;; Commentary:

;; This file defines `symon-mode', a major mode for the specification
;; language of SyMon using Emacs's built-in Tree-sitter integration.
;; It automatically downloads and builds the grammar from GitHub if
;; not yet installed, and uses `queries/highlights.scm` from the
;; grammar repo for font-locking.

;;; Code:

(require 'treesit)
(require 'prog-mode)

(defcustom symon-mode-indent-offset 4
  "Number of spaces for each indentation step in `symon-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'symon)

(defvar symon-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `symon-mode'.")

(defun symon-mode-install-grammar ()
  "Install language grammar for SyMon."
  (interactive)
  (add-to-list 'treesit-language-source-alist
               '(symon "https://github.com/maswag/tree-sitter-symon"))
  (treesit-install-language-grammar 'symon)
  (let* ((lang 'symon)
         (url (car (cdr (assoc lang treesit-language-source-alist))))
         (revision nil)
         (target-dir (expand-file-name
                      "symon/"
                      (locate-user-emacs-file "tree-sitter")))
         (workdir (make-temp-file "symon-mode--tree-sitter-symon" t)))
    ;; Clone the git repository to a temporary dictionary
    (treesit--git-clone-repo url revision workdir)
    ;; Create the target directory
    (make-directory target-dir t)
    ;; Copy the queries
    (dolist (query-file (file-expand-wildcards
                         (concat workdir "/queries/*.scm")))
      (message "Copy %s to %s" query-file target-dir)
      (copy-file query-file target-dir t))))

(defvar symon-mode--indent-rules
  `((symon
     ((parent-is "source_file") column-0 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((match ")" "half_guard") parent-bol 0)
     ((match ")" "intervals") parent-bol 0)
     ((parent-is "line_comment") prev-adaptive-prefix 0)
     ((parent-is "paren_expr") parent-bol symon-mode-indent-offset)
     ((parent-is "concat") parent-bol 0)
     ((parent-is "conjunction") parent-bol 0)
     ((parent-is "disjunction") parent-bol 0)
     ((parent-is "time_restriction") parent-bol 0)
     ((parent-is "atomic") parent-bol symon-mode-indent-offset)
     ((parent-is "arg_list") parent-bol 0)
     ((parent-is "guard_block") parent-bol 0)
     ((parent-is "constraint_list") parent-bol 0)
     ((parent-is "assignment_list") parent-bol 0)
     ((parent-is "zero_or_more") parent-bol symon-mode-indent-offset)
     ((parent-is "one_or_more") parent-bol symon-mode-indent-offset)
     ((parent-is "optional") parent-bol symon-mode-indent-offset)
     ((parent-is "one_of") parent-bol symon-mode-indent-offset)
     ((parent-is "any_of") parent-bol symon-mode-indent-offset)
     ((parent-is "within") parent-bol symon-mode-indent-offset)
     ((parent-is "intervals") parent-bol symon-mode-indent-offset)
     ((parent-is "half_guard") parent-bol symon-mode-indent-offset)
     ((parent-is "initial_constraints") parent-bol symon-mode-indent-offset)
     ((parent-is "ignore") parent-bol symon-mode-indent-offset)
     ((parent-is "def_expr") parent-bol symon-mode-indent-offset)
     ((parent-is "signature") parent-bol symon-mode-indent-offset)
     ((parent-is "variables") parent-bol symon-mode-indent-offset)
     ))
  "Tree-sitter indent rules for `symon-mode'.")

;;;###autoload
(define-derived-mode symon-mode prog-mode "SyMon"
  "Major mode for SyMon language using Tree-sitter."
  (if (treesit-ready-p 'symon)
      (progn
        (setq treesit-primary-parser (treesit-parser-create 'symon))

        ;; Comments.
        (setq-local comment-start "# ")

        ;; Font-lock.
        ;; This part is derived from <https://mail.gnu.org/archive/html/emacs-devel/2025-03/msg01177.html>.
        (let* ((file (expand-file-name
                      "symon/highlights.scm"
                      (locate-user-emacs-file "tree-sitter")))
               (query (when (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-substring-no-properties (point-min)
                                                          (point-max))))))

          (when query
            (pcase-dolist (`(,from . ,to)
                           '(
                             ("@comment"               . "@font-lock-comment-face")
                             ("@string"                . "@font-lock-string-face")
                             ("@type"                  . "@font-lock-type-face")
                             ("@constant"              . "@font-lock-constant-face")
                             ("@keyword"               . "@font-lock-keyword-face")
                             ("@operator"              . "@font-lock-operator-face")
                             ("@punctuation.bracket"   . "@font-lock-bracket-face")
                             ("@punctuation.delimiter" . "@font-lock-delimiter-face")
                             ("@variable.parameter"    . "@font-lock-variable-name-face")))
              (setq query (replace-regexp-in-string from to query nil t)))

            (setq-local treesit-font-lock-settings
                        (treesit-font-lock-rules
                         :language 'symon
                         :feature 'highlights
                         query))))

        (setq-local treesit-font-lock-feature-list '((highlights)))

        ;; Indent.
        (setq-local indent-tabs-mode nil
                    treesit-simple-indent-rules symon-mode--indent-rules)

        ;; Navigation.
        (setq-local treesit-defun-type-regexp "variables\\|signature\\|initial_constraints\\|def_expr")

        ;; Defun name.
        (setq-local treesit-defun-name-function
                    (lambda (node)
                      (pcase (treesit-node-type node)
                        ((or "def_expr" "signature")
                         (let* ((name-node (treesit-node-child node 1))
                                (name (treesit-node-text name-node t)))
                           name))
                        (_ nil))))

        ;; Imenu.
        (setq-local treesit-simple-imenu-settings
                    '(("Signature" "signature" nil nil)
                      ("Expression" "def_expr" nil nil)))

        ;; Electric.
        (setq-local electric-indent-chars
                    (append "{};" electric-indent-chars))

        (treesit-major-mode-setup))
    (message "symon-language-grammar is not installed. To install, run \"M-x symon-mode-install-grammar\".")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.symon\\'" . symon-mode))

(provide 'symon-mode)
;;; symon-mode.el ends here
