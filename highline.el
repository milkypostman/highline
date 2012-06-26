;;; newline.el --- Rewrite of Powerline

;;; Commentary:
;;

;;; Code:

(defvar highline-buffer-size-suffix t)

(defface highline-active1 '((t (:background "grey22"))) "Powerline face 1."
  :group 'highline)

(defface highline-active2 '((t (:background "grey40"))) "Powerline face 2."
  :group 'highline)

(defface highline-inactive1 '((t (:background "grey11"))) "Powerline face 1."
  :group 'highline)

(defface highline-inactive2 '((t (:background "grey20"))) "Powerline face 2."
  :group 'highline)


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize FUNC.
If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of FUNC."
  (let ((table-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))


(defun hl/arrow-row-right (dots width)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (concat "\"" (make-string dots ?.) (make-string (- width dots) ? ) "\","))

(defun hl/arrow-row-left (dots width)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (concat "\"" (make-string (- width dots) ?.) (make-string dots ? ) "\","))

(defmacro hl/arrow-xpm (dir)
  "Generate an arrow xpm function for DIR."
  (let ((rowfunc (intern (format "hl/arrow-row-%s" (symbol-name dir)))))
    `(defun ,(intern (format "hl/arrow-xpm-%s" (symbol-name dir))) (height color1 color2)
       (let* ((dots (/ height 2))
              (odd (= (mod height 2) 1))
              (width (+ (if odd 1 0) dots 3)))
         (create-image
          (concat
           (format "/* XPM */
static char * arrow_%s[] = {
\"%s %s 2 1\",
\". c %s\",
\"  c %s\",
" (symbol-name ',dir) width height (or color1 "None") (or color2 "None"))
           (mapconcat (lambda (d) (,rowfunc d width)) (number-sequence 1 dots) "\n")
           (and odd "\n")
           (and odd (,rowfunc (+ dots 1) width))
           "\n"
           (mapconcat (lambda (d) (,rowfunc d width)) (number-sequence dots 1 -1) "\n")
           "};")
          'xpm t :ascent 'center)))))

(memoize (hl/arrow-xpm left))
(memoize (hl/arrow-xpm right))


(defun hl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 for enabled and COLOR2 for disabled bits specified in DATA."
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (or color1 "None")
            (or color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar '(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar '(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun hl/percent-xpm
  (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm."
  (let* ((height- (1- height))
         (fillstart (round (* height- (/ (float winstart) (float pmax)))))
         (fillend (round (* height- (/ (float winend) (float pmax)))))
         (data nil)
         (i 0))
    (while (< i height)
      (setq data (cons
                  (if (and (<= fillstart i)
                           (<= i fillend))
                      (append (list 0) (make-list width 1) (list 0))
                    (append (list 0) (make-list width 0) (list 0)))
                  data))
      (setq i (+ i 1)))
    (hl/make-xpm "percent" color1 color2 (reverse data))))

(memoize 'hl/percent-xpm)

(defun highline-hud (face1 face2)
  (let ((color1 (if face1 (face-attribute face1 :background) "None"))
        (color2 (if face2 (face-attribute face2 :background) "None"))
        pmax
        pmin
        (ws (window-start))
        (we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (propertize "  "
                'display
                (hl/percent-xpm (frame-char-height) pmax pmin we ws 15 color1 color2))))


(defun highline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))


(defun highline-concat (&rest strings)
  "Concatonate STRINGS and pad sides by spaces."
  (concat
   " "
   (mapconcat 'identity (delq nil strings) " ")
   " "))

(defmacro defhltext (name body)
  `(defun ,name
     (&optional face)
     (let ((str ,body))
       (propertize (or str "") 'face face))))

(defun highline-raw (str &optional face)
     (propertize (or str "") 'face face))


(defun highline-fill (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (eq 'right (get-scroll-bar-mode))
    (setq reserve (+ reserve 3)))
  (propertize " " 'display `((space :align-to (- right-fringe ,reserve))) 'face face))


(defhltext highline-major-mode
  (propertize mode-name
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defhltext highline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line down-mouse-1]   (highline-mouse 'minor 'menu mm))
                                        (define-key map [mode-line mouse-2]        (highline-mouse 'minor 'help mm))
                                        (define-key map [mode-line down-mouse-3]   (highline-mouse 'minor 'menu mm))
                                        (define-key map [header-line down-mouse-3] (highline-mouse 'minor 'menu mm))

                                        map)))
             (split-string (format-mode-line minor-mode-alist)) " "))


(defhltext highline-narrow
  (let (real-point-min real-point-max)
    (save-excursion
      (save-restriction
        (widen)
        (setq real-point-min (point-min) real-point-max (point-max))))
    (when (or (/= real-point-min (point-min))
              (/= real-point-max (point-max)))
      (propertize "Narrow"
                  'help-echo "mouse-1: Remove narrowing from the current buffer"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 'mode-line-widen)))))
(defhltext highline-vc
  (when (and (buffer-file-name (current-buffer))
             vc-mode)
    (symbol-name (vc-mode-line (buffer-file-name (current-buffer))))))


(defhltext highline-buffer-size
  (propertize
   (if highline-buffer-size-suffix
       "%I"
     "%i")
   'local-map (make-mode-line-mouse-map
               'mouse-1 (lambda () (interactive)
                          (setq powerline-buffer-size-suffix
                                (not powerline-buffer-size-suffix))
                          (redraw-modeline)))))


(defmacro defhlsep (name docstring &optional func)
  "Create a function NAME with optional DOCSTRING that takes arguments FACE1, FACE2 and call FUNC with the background colors for those faces or \"None\"."
  (unless func
    (setq func docstring)
    (setq docstring nil))
  `(defun ,name
     (&optional face1 face2)
     ,docstring
     (let ((color1 (if face1 (face-attribute face1 :background) "None"))
           (color2 (if face2 (face-attribute face2 :background) "None")))
           (propertize " " 'display
                       (,func (frame-char-height) color1 color2)))))


(defhlsep highline-arrow-left hl/arrow-xpm-left)
(defhlsep highline-arrow-right hl/arrow-xpm-right)


(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)))
                        (face1 (if active 'highline-active1 'highline-inactive1))
                        (face2 (if active 'highline-active2 'highline-inactive2))
                        (lhs (concat
                              (highline-raw "%*")
                              (highline-buffer-size)
                              (highline-raw "%12b")

                              (highline-arrow-right nil face1)

                              (highline-major-mode face1)
                              (highline-minor-modes face1)
                              (highline-raw mode-line-process face2)

                              (highline-arrow-right face1 face2)

                              (highline-narrow 'highline-active1)
                              (highline-vc face2)
                              ))
                        (rhs (concat
                              (highline-raw global-mode-string face2)
                              (highline-arrow-left face2 face1)

                              (highline-raw "%4l" face1)
                              (highline-raw ":" face1)
                              (highline-raw "%3c" face1)

                              (highline-arrow-left face1 nil)

                              (highline-raw "%6p")

                              (highline-hud face2 nil)
                              ))
                        )
                   (concat lhs (highline-fill face2 20) rhs)))))


(provide 'highline)


;;; newline.el ends here
