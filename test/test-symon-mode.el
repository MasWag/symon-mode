;;; test/test-symon-mode.el --- ERT tests for symon-mode -*- lexical-binding: t; -*-

;; Author: Masaki Waga <masakiwaga@gmail.com>
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/maswag/symon-mode

;;; Commentary:

;; This file contains automated tests for `symon-mode' using Emacs's ERT framework.
;; Place this file under the `test/' directory of your project.
;; To run these tests, use:
;;   emacs --batch -Q -L . -l ert -l test/test-symon-mode.el \
;;         --eval "(ert-run-tests-batch-and-exit)"

;;; Code:

;; Ensure `symon-mode.el' is on the load path.
(setq load-path (cons (file-name-directory
                       (file-name-directory load-file-name))
                      load-path))

;; Load the mode and ERT.
(require 'symon-mode)
(symon-mode-install-grammar)
(require 'ert)

;; -----------------------------------------------------------------------------
;; Indentation Test
;; -----------------------------------------------------------------------------

(ert-deftest symon-mode-indent-simple ()
  "Test that a SyMon snippet is indented according to `symon-mode` rules."
  (with-temp-buffer
    ;; Insert an unindented SyMon example.
    (insert
     "var {\n"
     "savedValue: number;\n"
     "}\n"
     "\n"
     "signature update {\n"
     "id: string;\n"
     "value: number;\n"
     "}\n"
     "\n"
     "signature reload {\n"
     "id: string;\n"
     "}\n"
     "\n"
     "signature revoke {\n"
     "id: string;\n"
     "}\n"
     "\n"
     "expr ignoreIrrelevant { (update(id, value))* }\n"
     "expr saveY { update( id, value | id == \"y\" | savedValue := value) }\n"
     "expr notX { update(id, value | id != \"x\" ) }\n"
     "expr consistent { update(id, value | id == \"x\" && savedValue - value = 0) }\n"
     "expr inConsistent { update(id, value | id == \"x\" && savedValue - value > 0 && savedValue - value < 0) }\n"
     "\n"
     "expr getConsistent {\n"
     "within (< 3) {\n"
     "ignoreIrrelevant;\n"
     "consistent\n"
     "}\n"
     "}\n"
     "expr keepConsistent {\n"
     "within (< 5) {\n"
     "getConsistent;\n"
     "(notX || consistent)*\n"
     "}\n"
     "}\n"
     "\n"
     "expr main {\n"
     "ignore reload, revoke {\n"
     "keepConsistent;\n"
     "update(id, value)\n"
     "}\n"
     "}\n"
     "\n"
     "ignoreIrrelevant; saveY ; (main)%(> 5)")
    ;; Activate symon-mode and perform indentation.
    (symon-mode)
    (indent-region (point-min) (point-max))
    ;; Define expected indentation.
    (let ((expected
           (concat
            "var {\n"
            "    savedValue: number;\n"
            "}\n"
            "\n"
            "signature update {\n"
            "    id: string;\n"
            "    value: number;\n"
            "}\n"
            "\n"
            "signature reload {\n"
            "    id: string;\n"
            "}\n"
            "\n"
            "signature revoke {\n"
            "    id: string;\n"
            "}\n"
            "\n"
            "expr ignoreIrrelevant { (update(id, value))* }\n"
            "expr saveY { update( id, value | id == \"y\" | savedValue := value) }\n"
            "expr notX { update(id, value | id != \"x\" ) }\n"
            "expr consistent { update(id, value | id == \"x\" && savedValue - value = 0) }\n"
            "expr inConsistent { update(id, value | id == \"x\" && savedValue - value > 0 && savedValue - value < 0) }\n"
            "\n"
            "expr getConsistent {\n"
            "    within (< 3) {\n"
            "        ignoreIrrelevant;\n"
            "        consistent\n"
            "    }\n"
            "}\n"
            "expr keepConsistent {\n"
            "    within (< 5) {\n"
            "        getConsistent;\n"
            "        (notX || consistent)*\n"
            "    }\n"
            "}\n"
            "\n"
            "expr main {\n"
            "    ignore reload, revoke {\n"
            "        keepConsistent;\n"
            "        update(id, value)\n"
            "    }\n"
            "}\n"
            "\n"
            "ignoreIrrelevant; saveY ; (main)%(> 5)")))
      (should (equal (buffer-string) expected)))))

(provide 'test-symon-mode)
;;; test-symon-mode.el ends here
