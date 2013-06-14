;; * org-rpkg.el --- writing R packages with Org-mode
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2013
;;   :version:  0.9
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :keywords: emacs org-mode R ESS CRAN outshine outorg
;;   :END:

;; ** Commentary

;; This file implements functionality for 'exporting' an Org-mode file to an R
;; package. The implementation is based on two goals:

;; 1. Minimize redundancy and repetition
;; 2. Use convention instead of configuration/programming whenever possible

;; Therefore, no attempts are made to cover all kinds of corner cases and to
;; deal with all kinds of possible user preferences. This file assumes that
;; the user sticks strictly to the conventions. If he doesn't, things won't
;; work.

;; ** Usage
;; *** Focus on source-code (no tangling)

;; This is the principal use case of this library. It assumes that writing an R
;; package is primarily a programming task, but nevertheless includes a
;; challenging and sophisticated documentation part. 

;; In this case, the whole R package is written in a single =.R= file that is
;; given the 'look&feel' of an Org-mode file by activating /outline-minor-mode/
;; with /outshine.el/ and /outorg.el/ extensions (optionally, /navi-mode.el/ can
;; be used too as a productivity booster). 

;; This =.R= file is the sole source-file of the project, all R code is contained
;; in this file. It is _not_ splitted into several smaller files later on. It
;; will usuably be used in /R-mode/ (ESS[S]), i.e. the R-dialect of /ess-mode/,
;; in combination with an \*R\* process-buffer in iESS mode. No tangling is
;; necessary. This is the (only?) file that should be under version-control. 

;; At the same time, this =.R= file contains all the documentation and meta-data
;; necessary to produce a R package - as comments. These comment-sections are
;; converted to Org-mode and offered in temporary Org-mode edit buffers for any
;; non-trivial text editing to be done. Thus, the documentation is stored in this
;; =.R= file as comments (making the file machine-executable), but all the actual
;; editing of the documentation is done in Org-mode after uncommenting the
;; comment-text parts. The temporary Org-mode edit buffers are then used to
;; 'export' the file to an R package. In contrast to the source-code, the
;; documentation is splitted into several (obligatoric) files during this
;; process, as demanded by the standards for R packages.

;; *** Focus on text (with tangling)

;; Some users will prefer the classic way of literate programming even when
;; the programming part seems to be more important than the document authoring
;; part. They will want to write their R package in one =.org= file, do the
;; source-code editing in temporary ESS edit buffers, and frequently extract
;; machine-executable source-files by tangling. They probably want their
;; source-code to be splitted into several small =.R= files in the R package
;; generation process, thereby adhering to the conventions in the R community.

;; ** ChangeLog

;; | version | author          | date            |
;; |---------+-----------------+-----------------|
;; |     0.9 | Thorsten Jolitz | <2013-06-14 Fr> |

;; * Requires
;; * Mode Definitions
;; * Variables
;; ** Consts
;; ** Vars
;; ** Hooks
;; ** Customs
;; * Functions
;; ** Non-interactive Functions

(defun org-rpkg-insert-org-template ()
  "Insert file skeleton for `org-mode' file.")

(defun org-rpkg-insert-ess-template ()
  "Insert file skeleton for `ess-mode' file.")

;; ** Commands

(defun org-rpkg-insert-template (&optional ORG)
  "Insert file skeleton for writing an R package with Org-mode. 
If current-buffer is in `org-mode' or ORG is non-nil, an Org-mode template
  will be inserted, otherwise an `ess-mode' template."
  (interactive))

(defun org-rpkg-update-package (&optional filename pkgdir CONFIRM-OVERWRITE-P)
  "Update an existing R package or create a new one.

If FILENAME is non-nil and has extension '.org' or '.R', visit
the file and operate in that that buffer. Otherwise, use the
current buffer if it is in `org-mode' or `outline-minor-mode'
with `outshine' extensions. If PKGDIR is non-nil, it is assumed
to be the name of the R package to be updated. Otherwise, the R
package to be updated or created is searched in FILENAME's
directory (if FILENAME is given) or in the current directory. 

If CONFIRM-OVERWRITE-P is non-nil, ask the user before overwriting existing R
package files."
  (interactive))

;; * Menus and Keys
;; ** Menus
;; ** Keys
;; * Run hooks and provide
