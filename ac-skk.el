(defvar ac-source-skk
  '((prefix . ac-skk-prefix)
    (candidates . ac-skk-candidates)
    (match . (lambda (prefix cands) cands))
    (requires . 1)
    (symbol . "SKK")))

(defun ac-skk-prefix ()
  (when (and skk-mode
             (eq skk-henkan-mode 'on))
    skk-henkan-start-point))

(defun ac-skk-make-cand (cand action midasi count)
  (propertize cand 'action action 'henkan-key midasi 'skk-count count))

(defun ac-skk-make-cand-list (midasi prog-list)
  (let* ((henkan-list (skk-search-progs midasi prog-list 'remove-note))
         (candidates (loop for cand in henkan-list
                           for i from 0
                           collect (ac-skk-make-cand cand 'ac-skk-kakutei midasi i)))
         (forward-cand (ac-skk-make-cand midasi 'ac-skk-henkan-forward midasi (length candidates))))
    candidates))

(defun ac-skk-candidates ()
  (when (eq skk-henkan-mode 'on)
    (let ((henkan-prog-list (append
                             (subseq skk-search-prog-list
                                     0 (position '(skk-okuri-search) skk-search-prog-list :test #'equal))
                             (when skk-auto-okuri-process
                               (list '(skk-okuri-search-1)))))
          (midasi-prog-list '((skk-comp-from-jisyo skk-jisyo))))
      (loop for midasi in (cons ac-prefix (skk-comp-get-all-candidates ac-prefix nil midasi-prog-list))
            nconc (ac-skk-make-cand-list midasi henkan-prog-list)))))
;; 順番が(Recent headに)変わらない時があるのはskk-studyの影響

(defun ac-skk-kakutei ()
  (when skk-katakana
    (error "No Support skk-katakana mode."))
  (delete-region skk-henkan-start-point (point))
  (insert (get-text-property 0 'henkan-key candidate)) ;only support execute in ac-complete function
  (ac-skk-start-henkan (1+ (get-text-property 0 'skk-count candidate)))
  (skk-kakutei))

(defun ac-skk-henkan-forward ()
  (ac-skk-start-henkan (get-text-property 0 'skk-count candidate))
  (skk-start-henkan 1))

(defun ac-skk-start-henkan (count)
  (let ((skk-show-annotation nil))         ;darty hack??
    (dotimes (i count)
      (skk-start-henkan 1))))

;;;; hiracomp
(defvar ac-source-skk-hiracomp
  '((prefix . ac-skk-prefix-hiracomp)
    (candidates . ac-skk-hiracomp-candidates)
    (match . (lambda (prefix cands) cands))
    (requires . 2)
    (symbol . "SKKH")))

(defun ac-skk-prefix-hiracomp ()
  (when (and skk-mode
             skk-j-mode
             (not skk-henkan-mode)
             (fboundp 'ts:segment))
    (save-match-data
      (when (looking-back "\\(?:\\cH\\|\\cK\\|\\cC\\)\\{1,10\\}" (max (- (point) 10) 0) t)
        (let* ((segs (ts:segment (substring-no-properties (match-string 0) 0)))
               (lst (last segs 2)))
          (- (point)
             (if (null (cdr lst))
                 (length (car lst))
               (if (= (length (cadr lst)) 1)
                   (loop for c in lst sum (length c))
                 (length (cadr lst))))))))))

(defun ac-skk-hiracomp-candidates ()
  (let ((prog-list '((skk-search-jisyo-file skk-jisyo 0 t)
                     (skk-okuri-search-1)
                     ;(skk-search-server skk-aux-large-jisyo 10000)
                     (skk-search-katakana))))
    (append
     (skk-search-progs ac-prefix prog-list 'remove-note)
     (loop for i from 0 below (length ac-prefix)
           collect (propertize (concat (substring ac-prefix 0 i) "▽" (substring ac-prefix i))
                               'action 'ac-skk-hiracomp-mes)))))

(defun ac-skk-hiracomp-mes ()
  (let ((midasi ""))
    (save-match-data
      (when (looking-back "▽\\(\\cH+\\)" nil t)
        (setq midasi (match-string 1))
        (delete-backward-char (length (match-string 0)))))
    (skk-set-henkan-point-subr)
    (insert midasi)
    (ac-start :force-init t)))          ;これでいいのか？


;;;; Enable/Disable mode functions
(defvar ac-skk-enable nil)
(defvar ac-skk-ac-sources-orig nil)
(defvar ac-skk-special-sources '(ac-source-skk))
(defvar ac-skk-save-variable '(ac-trigger-commands ac-use-comphist skk-dcomp-activate skk-dcomp-multiple-activate))

(defun ac-skk-enable ()
  (interactive)
  (setq ac-skk-enable t))

(defun ac-skk-disable ()
  (interactive)
  (setq ac-skk-enable nil))

(defun ac-skk-toggle ()
  (interactive)
  (setq ac-skk-enable (not ac-skk-enable)))

(defun ac-skk-setup ()
  (when ac-skk-enable
    (set (make-local-variable 'ac-skk-ac-sources-orig) ac-sources)
    (setq ac-sources ac-skk-special-sources)
    (dolist (sym ac-skk-save-variable)
      (let ((store-sym (intern (format "ac-skk-%s-orig" sym))))
        (set (make-local-variable store-sym) (symbol-value sym))
        (set (make-local-variable sym) nil)))
    (setq ac-trigger-commands (append '(skk-insert skk-previous-candidate) ac-skk-ac-trigger-commands-orig))))

(defun ac-skk-cleanup ()
  (when (local-variable-p 'ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-ac-sources-orig)
    (kill-local-variable 'ac-skk-ac-sources-orig)
    (dolist (sym ac-skk-save-variable)
      (let ((store-sym (intern (format "ac-skk-%s-orig" sym))))
        (kill-local-variable sym)
        (kill-local-variable store-sym)))))

(add-hook 'skk-mode-hook 'ac-skk-setup)
(defadvice skk-mode-exit (after ac-skk activate)
  (ac-skk-cleanup))
(defadvice skk-j-mode-on (after ac-skk activate)
  (when (and ac-skk-enable ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-special-sources)
    (dolist (com '(skk-insert skk-previous-candidate))
      (add-to-list 'ac-trigger-commands com))))
(defadvice skk-latin-mode (after ac-skk activate)
  (when (and ac-skk-enable ac-skk-ac-sources-orig)
    (setq ac-sources ac-skk-ac-sources-orig
          ac-trigger-commands ac-skk-ac-trigger-commands-orig)))

(defadvice ac-trigger-command-p (after ac-trigger-command-p-for-viper activate)
  "Return non-nil if `this-command' is a trigger command for viper-mode."
  (setq ad-return-value
        (if (or ;(and skk-j-mode (not skk-henkan-mode))
                (and skk-henkan-mode
                     (not (memq 'skk-insert ac-trigger-commands))))
            nil
          ad-return-value)))

;; On Debug
(when (memq this-command '(expectations-eval-defun eval-defun))
  ;; (remove-hook 'skk-mode-hook 'ac-skk-setup)
  ;; (setq ac-use-comphist nil)
  (progn (setq skk-search-end-function (delq 'skk-study-search skk-search-end-function))
         (setq skk-update-end-function (delq 'skk-study-update skk-update-end-function)))
  (message "AC-SKK DEBUG ON"))

(when (memq this-command '(expectations-eval-defun eval-defun))
  (progn
    ;; (setq ac-use-comphist t)
    (add-to-list 'skk-search-end-function 'skk-study-search)
    (add-to-list 'skk-update-end-function 'skk-study-update)
    (message "AC-SKK DEBUG OFF")))
