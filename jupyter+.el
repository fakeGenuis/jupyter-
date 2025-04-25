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

(defun jupyter+-completion-at-point ()
  "Function to add to `completion-at-point-functions'."
  (let ((prefix (jupyter-completion-prefix)))
    (when (and
           prefix jupyter-current-client
           ;; Don't try to send completion requests when the kernel is busy
           ;; since it doesn't appear that kernels respond to these requests
           ;; when the kernel is busy, at least the Julia kernel doesn't.
           ;;
           ;; FIXME: Maybe this is kernel dependent
           (or (not (jupyter-kernel-busy-p jupyter-current-client))
               (plist-get (jupyter-kernel-info jupyter-current-client) :busy_completion)))
      ;; NOTE: removed company idle
      (when (consp prefix)
        (setq prefix (car prefix)))
      (when (jupyter-completion-prefetch-p prefix)
        (setq jupyter-completion-cache nil)
        (jupyter-completion-prefetch
         (lambda (msg) (setq jupyter-completion-cache
                             (list 'fetched prefix msg)))))
      (list
       (- (point) (length prefix)) (point)
       (completion-table-dynamic
        (lambda (_)
          (when (null jupyter-completion-cache)
            (sit-for 0.1))
          (when (eq (car jupyter-completion-cache) 'fetched)
            (jupyter-with-message-content (nth 2 jupyter-completion-cache)
                (status matches metadata)
              (setq jupyter-completion-cache
                    (cons (nth 1 jupyter-completion-cache)
                          (when (equal status "ok")
                            (jupyter+-completion-construct-candidates
                             matches metadata))))))
          (cdr jupyter-completion-cache)))
       :exit-function
       #'jupyter-completion--post-completion
       :company-kind
       (lambda (arg)
         (intern (get-text-property 0 'type arg)))
       :company-location
       (lambda (arg) (get-text-property 0 'location arg))
       ;; :annotation-function
       ;; (lambda (arg) (get-text-property 0 'annot arg))
       :company-docsig
       (lambda (arg) (get-text-property 0 'docsig arg))
       :company-doc-buffer
       #'jupyter+-company-doc-buffer))))

(defun jupyter+-completion-construct-candidates (matches metadata)
  "Construct candidates for completion.
MATCHES are the completion matches returned by the kernel,
METADATA is any extra data associated with MATCHES that was
supplied by the kernel."
  (let (buf)
    (with-temp-buffer
      (cl-loop
       for i from 0 below (length matches)
       for match = (aref matches i)
       do
       ;; NOTE not add function itself, respect backend's 'docsig
       ;; (put-text-property 0 1 'docsig match match)
       (cond
        ;; TODO maybe a variable (list of function) here to support language
        ;; specific handle
        ((string-match jupyter-completion-argument-regexp match)
         (let* ((str match)
                (args-str (match-string 1 str))
                (end (match-end 1))
                (path (match-string 2 str))
                (line (string-to-number (match-string 3 str)))
                (snippet (progn
                           (erase-buffer)
                           (insert args-str)
                           (goto-char (point-min))
                           (jupyter-completion--make-arg-snippet
                            (jupyter-completion--arg-extract)))))
           (setq match (aset matches i (substring match 0 end)))
           (put-text-property 0 1 'snippet snippet match)
           (put-text-property 0 1 'location (cons path line) match)))
        ;; TODO: This is specific to the results that
        ;; the python kernel returns, make a support
        ;; function?
        ((string-match-p "\\." match)
         (aset matches i (car (last (split-string match "\\."))))))))
    ;; When a type is supplied add it as an annotation
    (when-let* ((types (plist-get metadata :_jupyter_types_experimental))
                (lengths (mapcar #'length matches)))
      (cl-loop
       for i from 0 below
       ;; For safety, ensure the lengths are the same which is
       ;; usually the case, but sometimes a kernel may not return
       ;; lists of the same length.  Seen for example in IJulia.
       (min (length matches) (length types))
       ;; These are typically in the same order.
       for match = (aref matches i)
       for meta = (aref types i)
       do (let* ((type (plist-get meta :type))
                 (sig (plist-get meta :signature)))
            (put-text-property 0 1 'docsig sig match)
            (put-text-property 0 1 'type type match))))
    ;; FIXME To get rid of this conversion use `json-array-type', be
    ;; sure to change places where it is assumed that there are
    ;; vectors.
    (append matches nil)))

(defun jupyter+-company-doc-buffer (code)
  "Inspect CODE string and return doc buffer."
  (let ((buf (jupyter+-make-doc-buffer)))
    (jupyter-inspect code 1 buf)
    (with-current-buffer buf
      (when (> (point-max) (point-min))
        (let ((inhibit-read-only t))
          ;; TODO use text properties to highlight?
          (remove-text-properties
           (point-min) (point-max) '(read-only))
          (font-lock-mode 1)
          (goto-char (point-min))
          (current-buffer))))))

(defun jupyter+-make-doc-buffer ()
  "Generate / reuse a buffer for doc display."
  (with-current-buffer (get-buffer-create "*jupyter-doc*")
    (erase-buffer)
    (fundamental-mode)
    (current-buffer)))

(advice-add 'jupyter-completion-at-point :override #'jupyter+-completion-at-point)

(provide 'jupyter+)

;;; jupyter+.el ends here
