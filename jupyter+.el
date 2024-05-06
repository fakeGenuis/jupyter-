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

(declare-function evil-insert "evil-commands" (count &optional vcount skip-empty-lines))
(transient-define-prefix jupyter-org-transient ()
  "Prefix with descriptions specified with slots."
  :transient-suffix t
  ["Transient for jupyter org src blocks\n"
   ["Execution"
    ("S" "shutdown" jupyter-kill-repl-buffer)
    ("<return>" "current" org-ctrl-c-ctrl-c)
    ("C-<return>" "current to next" jupyter-org-execute-and-next-block)
    ("M-<return>" "subtree to point" jupyter-org-execute-subtree)
    ("S-<return>" "restart/block" jupyter-org-restart-kernel-execute-block)]

   ["Navigate"
    ("j" "next" org-babel-next-src-block)
    ;; ("J" "next busy" jupyte-org-next-busy-src-block)
    ("k" "previous" org-babel-previous-src-block)
    ;; ("K" "previous busy" jupyter-org-previous-busy-src-block)
    ("g" "consult" jupyter-org-consult-block)
    ("J" "next heading" org-next-visible-heading)
    ("K" "previous heading" org-previous-visible-heading)
    ("G" "consult heading" consult-org-heading)]

   ["Edit"
    ("d" "kill" jupyter-org-kill-block-and-results)
    ("y" "copy" jupyter-org-copy-block-and-results)
    ("p" "paste" jupyter-org-yank-src-block-below)
    ("P" "paste above" jupyter-org-yank-src-block-above)
    ("m" "merge" jupyter-org-merge-blocks)
    ;; since we always have point in the head of src block
    ;; ("s" "split" jupyter-org-split-src-block)
    ("u" "undo" undo)
    ("o" "add below"
     (lambda () (interactive)
       (jupyter-org-insert-src-block t current-prefix-arg)
       (when (featurep 'evil)
         (evil-insert 1)))
     :transient nil)
    ("O" "add above"
     (lambda () (interactive)
       (jupyter-org-insert-src-block nil current-prefix-arg)
       (when (featurep 'evil)
         (evil-insert 1)))
     :transient nil)]

   ["Misc"
    ("c" "clear" org-babel-remove-result)
    ("h" "edit header" jupyter-org-edit-header)
    ("t" "toggle results" org-babel-hide-result-toggle)
    ("i" "interrupt" jupyter-org-interrupt-kernel)]])

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

(provide 'jupyter+)

;;; jupyter+.el ends here
