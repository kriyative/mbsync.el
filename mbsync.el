;;; mbsync.el --- Manage mailbox synchronization using mbsync

;; Copyright (C) 2020 Ram Krishnan

;; Author: Ram Krishnan <kriyative@gmail.com>
;; Version: 1.0
;; Keywords: mail, mbsync
;; URL: https://github.com/kriyative/mbsync.el

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; mbsync.el --- This is the main source code of mbsync.el

;;; Commentary:

;; This package provides a mechanism to trigger synchronization of
;; mailboxes using the `mbsync' utility. To activate, set the
;; *mbsync-accounts* variable to a list of the mbsync accounts,
;; optionally with an update interval in seconds. Add a
;; `(mbsync-sync-accounts)` to start the sync timers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar *mbsync--verbose* nil
 "Enable verbose logging of mbsync status.")

(defmacro mbsync--log (&rest args)
  "Emit a echo-area message using ARGS, if *mbsync--verbose* is
non-nil."
  `(when *mbsync--verbose*
     (message ,@args)))

(cl-defstruct mbsync-state
  mbox last-checked-time total recent)

(defvar *mbsync--state* (make-hash-table :test 'equal)
  "Internal var to keep track of mbsync state.")

(defun mbsync-status ()
  "Display a summary buffer of the sync status of all configured
Accounts and Mailboxes."
  (interactive)
  (let ((buf "*mbsync-status*"))
    (with-current-buffer (get-buffer-create buf)
      (read-only-mode -1)
      (erase-buffer)
      (insert "[g] - refresh, [q] - quit\n")
      (maphash (lambda (account state)
                 (insert (format "%-20s %s\n"
                                 account
                                 (current-time-string
                                  (mbsync-state-last-checked-time
                                   (cdr (car state))))))
                 (dolist (mbox-state (reverse state))
                   (let ((s (cdr mbox-state)))
                     (insert (format " %-19s %d/%d\n"
                                     (car mbox-state)
                                     (mbsync-state-recent s)
                                     (mbsync-state-total s))))))
               *mbsync--state*)
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key "g" 'mbsync-status)
      (local-set-key "q" 'bury-buffer))
    (pop-to-buffer-same-window  buf)))

(defun mbsync--parse (account buf)
  "Parse the mbsync output of ACCOUNT in buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (let (state)
      (while (re-search-forward "Opening master box \\([^.]+\\)..." nil t)
        (let ((mbox (match-string-no-properties 1)))
          (when (re-search-forward "master: \\([0-9]+\\) messages, \\([0-9]+\\) recent"
                                   nil
                                   t)
            (push (cons mbox
                        (make-mbsync-state 
                         :mbox mbox
                         :last-checked-time (current-time)
                         :total (string-to-number (match-string 1))
                         :recent (string-to-number (match-string 2))))
                  state))))
      state)))

(defun mbsync--state-get (state mbox)
  "Lookup the MBOX information from STATE."
  (cdr (assoc mbox state 'equal)))

(defun mbsync--sentinel (proc type)
  "Process sentinel function for the mbsync sub-process."
  (let ((account (process-get proc :mbsync-account))
        (proc-buf (process-buffer proc)))
    (if (string-equal "finished\n" type)
        (let* ((state (mbsync--parse account proc-buf))
               (inbox-state (mbsync--state-get state "INBOX")))
          (puthash account state *mbsync--state*)
          (with-current-buffer proc-buf
            (erase-buffer))
          (mbsync--log "mbsync synced (%s - %d/%d)"
                       account
                       (mbsync-state-recent inbox-state)
                       (mbsync-state-total inbox-state)))
      (message "mbsync (%s) %s"
               account
               (string-trim type)))))

(defvar mbsync--command "mbsync")
(defvar mbsync--args '("-V"))

(defun mbsync--sync (account)
  "Sync the mailbox information for ACCOUNT using mbsync."
  (mbsync--log "mbsync syncing (%s)" account)
  (let* ((name (format " *mbsync:%s*" account))
         (proc (make-process :name name
                             :buffer name
                             :command `(,mbsync--command ,@mbsync--args ,account)
                             :sentinel 'mbsync--sentinel)))
    (process-put proc :mbsync-account account)
    proc))

(defvar *mbsync--timers* nil
  "Internal var to keep track of mbsync sync timers.")

(defun mbsync--sync-start (account &optional interval)
  "Start a repeating timer for synchronizing mailbox information
for ACCOUNT, optionally updating repeatedly at INTERVAL seconds."
  (let ((existing (assoc account *mbsync--timers* 'equal)))
    (when existing
      (cancel-timer (cdr existing))
      (setq *mbsync--timers* (remove existing *mbsync--timers*)))
    (push (cons account
                (let ((interval (or interval 300)))
                  (run-at-time 0 interval 'mbsync--sync account)))
          *mbsync--timers*)))

;;;###autoload
(defvar *mbsync-accounts* nil
  "List of mbsync accounts to update, with optional update-interval, e.g.
;; sync the \"gmail\" account every 2 mins, \"outlook\" account
;; every 5 mins
(setq *mbsync-accounts* '(\"gmail\" (\"outlook\" 300)))")

;;;###autoload
(defun mbsync-sync-accounts ()
  "Start synchronization timers for all account specified in the
*MBSYNC-ACCOUNTS* var."
  (dolist (spec *mbsync-accounts*)
    (apply 'mbsync--sync-start (if (listp spec) spec (list spec)))))

(provide 'mbsync)

;;; mbsync.el ends here
