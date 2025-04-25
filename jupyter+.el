;;; jupyter+.el --- More jupyter org extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  fakeGenius

;; Author: fakeGenius
;; created: [2024-03-29 Fri]

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

;; A rewrite for `jupyter-org-hydra/body' in jupyter-org-extensions.el, extend
;; by transient.

;;; Code:

(require 'transient)
(require 'ob-jupyter)
(require 'consult)

(declare-function org-next-block "org" (arg &optional backward block-regexp))
(declare-function evil-insert "evil-commands" (count &optional vcount skip-empty-lines))
(declare-function org-in-src-block-p "org" (&optional INSIDE ELEMENT))

(transient-define-prefix jupyter-org-transient ()
  "Prefix with descriptions specified with slots."
  :transient-suffix t
  ["Transient for jupyter org src blocks\n"
   ["Execution"
    ("I" "interrupt" jupyter-org-interrupt-kernel)
    ("S" "shutdown" jupyter-kill-repl-buffer)
    ("<return>" "current" org-ctrl-c-ctrl-c)
    ("C-<return>" "current to next" jupyter-org-execute-and-next-block)
    ("M-<return>" "subtree to point" jupyter-org-execute-subtree)
    ("S-<return>" "restart/block" jupyter-org-restart-kernel-execute-block)]

   ["Navigate"
    ;; ("j" "next" org-babel-next-src-block)
    ("j" "next" jupyter-org-next-executable)
    ;; ("J" "next busy" jupyte-org-next-busy-src-block)
    ;; ("k" "previous" org-babel-previous-src-block)
    ("k" "previous" jupyter-org-previous-executable)
    ;; ("K" "previous busy" jupyter-org-previous-busy-src-block)
    ("g" "consult" jupyter-org-consult-block)
    ("J" "next heading" org-next-visible-heading)
    ("K" "previous heading" org-previous-visible-heading)
    ("G" "consult heading" consult-org-heading)
    ("TAB" "cycle" org-cycle)]

   ["Edit"
    ("d" "kill" jupyter-org-kill-block-and-results)
    ("y" "copy" jupyter-org-copy-block-and-results)
    ("p" "paste" jupyter-org-yank-src-block-below)
    ("P" "paste above" jupyter-org-yank-src-block-above)
    ("m" "merge" jupyter-org-merge-blocks)
    ;; since we always have point in the head of src block
    ;; ("s" "split" jupyter-org-split-src-block)
    ("u" "undo" undo)
    ("o" "add below" jupyter-org-add-src-block-below
     :transient nil)
    ("O" "add above" jupyter-org-add-src-block-above
     :transient nil)]

   ["Misc"
    ("c" "clear" org-babel-remove-result)
    ("h" "edit header" jupyter-org-edit-header)
    ("t" "toggle results" org-babel-hide-result-toggle)]])

(defun jupyter-repl-buffer-rx (sn)
  "Regular expression of a jupyter repl buffer.
SN is the value of header-args `:session'."
  (rx line-start "*jupyter-repl" (* anychar) (literal sn) "*" line-end))

(defun jupyter-kill-repl-buffer ()
  "Kill repl buffer of current jupyter src block.
`jupyter-repl-shutdown-kernel' only shutdown zmq sockets."
  (interactive)
  (let* ((sn (alist-get :session (nth 2 (org-babel-get-src-block-info))))
         (mbs (match-buffers (jupyter-repl-buffer-rx sn))))
    (cond
     ((length= mbs 0) (message "No active REPL buffer matched!"))
     ((length= mbs 1) (kill-buffer (cl-first mbs)))
     ((length> mbs 1)
      (kill-buffer
       (completing-read
        "REPL buffer:"
        (mapcar (lambda (buf) (format "%s" buf)) mbs)))))))

(advice-add 'jupyter-org-kill-block-and-results
            :after (lambda ()
                     (delete-blank-lines)
                     (forward-line)))

(defun jupyter-org-goto-src-block-end ()
  "Go to the end of current src block."
  (interactive)
  (let ((region (jupyter-org-src-block-bounds)))
    (goto-char (cdr region))))

(defun jupyter-org-goto-src-block-begin ()
  "Go to the end of current src block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (if head (goto-char head)
      (org-babel-previous-src-block))))

(defun jupyter-org-yank-src-block-below ()
  "Yank below current src block."
  (interactive)
  (jupyter-org-goto-src-block-end)
  (newline)
  (yank))

(defun jupyter-org-yank-src-block-above ()
  "Yank below current src block."
  (interactive)
  (jupyter-org-goto-src-block-begin)
  (yank)
  (newline))

;; CALLER: A syntax that call a named src block
(defconst jupyter-org--block-caller-regexp
  (rx line-start "#+CALL:" (0+ space)
      (1+ (any alnum "-"))
      "(" (? (any alnum "=")) ")")
  "Regular expression for a CALL to src block.")

;; EXECUTABLE: caller and ob-src-block
(defun jupyter-org--executable-block-regexp ()
  "Regexp for a general executable block."
  (rx (or (regexp org-babel-src-block-regexp)
          (regexp jupyter-org--block-caller-regexp))))

;;;###autoload
(defun jupyter-org-next-executable (&optional arg)
  "Jump to next org executable.
Include `#+CALL:' besides src block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "p")
  (org-next-block arg nil (jupyter-org--executable-block-regexp)))

;;;###autoload
(defun jupyter-org-previous-executable (&optional arg)
  "Jump to previous org executable.
Include `#+CALL:' besides src block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "p")
  (org-next-block arg t (jupyter-org--executable-block-regexp)))

(defun jupyter-org-add-src-block-below ()
  "Add an empty src block below closest src block."
  (interactive)
  (unless (org-in-src-block-p)
    (jupyter-org-previous-executable))
  (jupyter-org-insert-src-block t)
  (when (featurep 'evil)
    (evil-insert 1)))

(defun jupyter-org-add-src-block-above ()
  "Add an empty src block above closest src block."
  (interactive)
  (unless (org-in-src-block-p)
    (jupyter-org-next-executable))
  (jupyter-org-insert-src-block nil)
  (when (featurep 'evil)
    (evil-insert 1)))

(defun jupyter-org--annotate (s)
  "Annotate a preview of current EXECUTABLE.
S is the whole EXECUTABLE string,
PM is the point marker of EXECUTABLE."
  (save-match-data
    (let* ((parts (split-string s "\n"))
           (preview (take jupyter-org-jump-to-block-context-lines parts)))
      (replace-regexp-in-string "[ \t\n]+" " " (mapconcat #'identity preview "\n")))))

;;;###autoload
(defun jupyter-org-consult-block ()
  "Consult jump to a callable src block in the buffer."
  (interactive)
  (let ((blocks '())
        (from-pt (point)))
    (save-excursion
      (while (re-search-forward (jupyter-org--executable-block-regexp) nil t)
        (push (list (jupyter-org--annotate (match-string 0))
                    (match-beginning 0)) blocks))
      (goto-char (point-min))
      (while (re-search-forward (jupyter-org--executable-block-regexp) from-pt t)
        (push (list (jupyter-org--annotate (match-string 0))
                    (match-beginning 0)) blocks)))
    (consult--read (nreverse blocks)
                   :prompt "block: "
                   :require-match t
                   :sort nil
                   :lookup (lambda (cand all &rest _) (cdr (assoc cand all)))
                   :state (consult--jump-state))))

;; From https://emacs.stackexchange.com/a/63562
;;;###autoload
(defun jupyter-org--ansi-results ()
  "Render ansi src block results."
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

;;;###autoload
(add-hook 'org-babel-after-execute-hook 'jupyter-org--ansi-results)

(provide 'jupyter+)

;;; jupyter+.el ends here
