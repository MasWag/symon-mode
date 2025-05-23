symon-mode
==========

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)

This Emacs package provides a major mode for the specification language of [SyMon](https://github.com/MasWag/SyMon). This package uses Emacs's built-in Tree-sitter integration and [tree-sitter-symon](https://github.com/MasWag/tree-sitter-symon), the tree-sitter grammar for SyMon.

Features
--------

- Highlights
- Indentation
- Navigation: We provide support for [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html). So, you can jump to the definition of a signature or an expression by `M-g i`, `M-x imenu`, `M-x consult-imenu`, etc.

Requirements
------------

- Emacs: Version 29.1 or higher.

Installation and Setup
----------------------

After installing `symon-mode`, you have to run `M-x symon-mode-install-grammar` to install `tree-sitter-symon` under `.emacs.d/tree-sitter`.

### Using `leaf.el`

The configuration of `symon-mode` with [`leaf.el`](https://github.com/conao3/leaf.el) is as follows:

```elisp
(leaf symon-mode
  :vc (:url "https://github.com/MasWag/symon-mode")
  :require t)
```

### Using `use-package`

The configuration of `symon-mode` with `use-package` is as follows:

```elisp
(use-package symon-mode
  :ensure nil  ; The mode is not on MELPA
  :load-path "/path/to/symon-mode")  ; Specify the path to symon-mode.el
```

Running unit tests
------------------

To run the unit tests for `symon-mode`, you can use the following command in your terminal:

```bash
emacs  --batch   -Q   -L .   -l ert   -l test/test-symon-mode.el  --eval "(ert-run-tests-batch-and-exit)"
```
