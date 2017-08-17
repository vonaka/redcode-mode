;;; redcode-mode.el --- major mode for editing Redcode files

;; Copyright (C) 2017 Fedor Ryabinin

;; Author: Fedor Ryabinin

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst redcode-instructions
  '("dat" "mov" "add" "sub" "mul" "div" "mod" "jmp" "jmz" "jmn"
    "djn" "spl" "cmp" "seq" "sne" "slt" "ldp" "stp" "nop")
  "List of Redcode instructions.")

(defconst redcode-pseudo-opcodes
  '("org" "end" "for" "rof" "equ" "pin")
  "List of MARS pseudo-OpCodes.")

(defconst redcode-instruction-modifiers
  '("\\.a" "\\.b" "\\.ab" "\\.ba" "\\.f" "\\.x" "\\.i")
  "List of Redcode instruction modifiers.")

(defconst redcode-addressing-modes
  '("#" "\\$" "\\*" "@" "{" "}" "<" ">")
  "List of Redcode addressing modes.")

(defconst redcode-constants
  '("CORESIZE" "CURLINE" "MAXCYCLES" "MAXLENGTH" "MAXPROCESSES"
    "MINDISTANCE" "PSPACESIZE" "VERSION" "WARRIORS")
  "List of Redcode constants.")

(defvar redcode-mode-hook nil)

(defvar redcode-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c;" 'comment-or-uncomment-region)
    (define-key map "\C-j"  'newline-and-indent)
    map)
  "Keymap for Redcode mode.")

(defun redcode-font-lock-expr (xs)
  (cl-loop for e in xs
           collect (concat "\\<\\(?:" e "\\|" (upcase e) "\\)\\>")))

(defconst redcode-font-lock-instructions
  (let (instrs)
    (dolist (i redcode-instructions instrs)
      (dolist (m redcode-instruction-modifiers instrs)
        (setq instrs
              (cons (concat "\\<\\(?1:" i "\\|" (upcase i)
                            "\\)\\(?2:" m "\\|" (upcase m) "\\)\\>")
                    instrs))))))

(defconst redcode-font-lock-addr-modes
  (let (modes)
    (dolist (i  (append (redcode-font-lock-expr redcode-instructions)
                        redcode-font-lock-instructions
                        redcode-pseudo-opcodes)
                modes)
      (dolist (m redcode-addressing-modes modes)
        (setq modes (cons (concat ",[[:space:]]*\\(?4:" m "\\)")
                          modes))
        (setq modes (cons (concat i "[[:space:]]*\\(?4:" m "\\)")
                          modes))))))

(defconst redcode-font-lock-keywords
  (append
   (cl-loop for e in (redcode-font-lock-expr redcode-instructions)
            collect (append (list e) font-lock-keyword-face))
   (cl-loop for e in redcode-font-lock-instructions
            collect
            (append (list e) (list '(1 font-lock-keyword-face))
                    (list '(2 font-lock-type-face))))
   (cl-loop for e in (redcode-font-lock-expr redcode-pseudo-opcodes)
            collect (append (list e) font-lock-function-name-face))
   (cl-loop for e in redcode-constants
            collect (append (list (concat "\\<" e "\\>"))
                            font-lock-constant-face))
   (cl-loop for e in redcode-font-lock-addr-modes
            collect
            (append (list e) (list '(4 font-lock-variable-name-face)))))
  "Additional expressions to highlight in Redcode mode.")

(defun redcode-indent-line ()
  (interactive)
  (let* ((localp (point))
         (dist (condition-case nil
                   (save-excursion
                     (forward-line 0)
                     (skip-chars-forward "[:space:]")
                     (if (>= (point) localp) (setq localp nil))
                     (max (redcode-indentation) 0))
                 (error 0))))
    (if localp (save-excursion (indent-line-to dist))
      (indent-line-to dist))))

(defconst redcode-instructions-exp
  (let (r)
    (dolist (e (redcode-font-lock-expr redcode-instructions) r)
      (if r (setq r (concat "\\(" e "\\)\\|" r))
        (setq r (concat "\\(" e "\\)"))))
    (dolist (e (redcode-font-lock-expr redcode-pseudo-opcodes) r)
      (setq r (concat "\\(" e "\\)\\|" r)))))

(defun redcode-indentation ()
  (if (looking-at redcode-instructions-exp)
      (progn
        (forward-line -1)
        (if (looking-at "\\(\\sw\\|\\s_\\)*")
            (progn
              (skip-syntax-forward "\\sw\\s_")
              (skip-chars-forward "[:space:]")
              (if (looking-at redcode-instructions-exp)
                  (current-column) default-tab-width))
          default-tab-width))
    (if (looking-at ";")
        (progn
          (forward-line -1)
          (if (looking-at "\\(\\S>\\)*\\s<")
              (progn (skip-syntax-forward "^\\s<") (current-column)) 0))
      0)))

(defun redcode-syntax-propertize (start end)
  (funcall (syntax-propertize-rules
            ("\\<\\(f\\)or[[:space:]]+0\\>" (1 "< b"))
            ("\\<ro\\(f\\)\\>" (1 "> b")))
           start end))

(defvar redcode-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">;" st)
    st)
  "Redcode syntax table.")

(define-derived-mode redcode-mode prog-mode "Redcode"
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(redcode-font-lock-keywords))
  (setq-local indent-line-function #'redcode-indent-line)
  (setq-local tab-always-indent t)
  (setq-local syntax-propertize-function #'redcode-syntax-propertize)
  "Core War's Redcode major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.red\\'" . redcode-mode))

(provide 'redcode-mode)

;;; redcode-mode.el ends here
