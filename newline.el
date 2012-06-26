
(setq-default mode-line-format
              '("%e"
                (:eval
                 (if
                     (display-graphic-p)
                     #(" " 0 1
                       (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
                   #("-" 0 1
                     (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
                mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
                #("   " 0 3
                  (help-echo "mouse-1: Select (dra
g to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
                mode-line-position
                (vc-mode vc-mode)
                #("  " 0 2
                  (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
                mode-line-modes
                (which-func-mode
                 ("" which-func-format
                  #(" " 0 1
                    (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
                (global-mode-string
                 ("" global-mode-string
                  #(" " 0 1
                    (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
                (:eval
                 (unless
                     (display-graphic-p)
                   #("-%-" 0 3
                     (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
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


(defun nl/arrow-row-left (dots width)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (concat "\"" (make-string dots ?.) (make-string (- width dots) ? ) "\","))

(defun nl/arrow-row-right (dots width)
  "Generate a string with DOTS dots and spaces to fill out WIDTH."
  (concat "\"" (make-string (- width dots) ? ) (make-string dots ?.) "\","))

(defmacro nl/arrow-xpm (dir)
  "Generate an arrow xpm function for DIR."
  (let ((rowfunc (intern (format "nl/arrow-row-%s" (symbol-name dir)))))
    `(defun ,(intern (format "nl/arrow-xpm-%s" (symbol-name dir))) (height color1 color2)
       (let* ((dots (/ height 2))
              (odd (= (mod height 2) 1))
              (width (+ (if odd 1 0) dots 3)))
         (create-image
          (concat
           (format "/* XPM */
static char * arrow_left[] = {
\"%s %s 2 1\",
\". c %s\",
\"  c %s\",
" width height (or color1 "None") (or color2 "None"))
           (mapconcat (lambda (d) (,rowfunc d width)) (number-sequence 1 dots) "\n")
           (and odd "\n")
           (and odd (,rowfunc (+ dots 1) width))
           "\n"
           (mapconcat (lambda (d) (,rowfunc d width)) (number-sequence dots 1 -1) "\n")
           "};")
          'xpm t :ascent 'center)))))

(defun nl/make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
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

(defun nl/percent-xpm
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
    (nl/make-xpm "percent" color1 color2 (reverse data))))

(memoize (nl/arrow-xpm left))
(memoize (nl/arrow-xpm right))
(memoize 'nl/percent-xpm)

(defface newline-active1 '((t (:background "grey22"))) "Powerline face 1."
  :group 'newline)

(defface newline-active2 '((t (:background "grey40"))) "Powerline face 2."
  :group 'newline)

(defface newline-inactive1 '((t (:background "grey11"))) "Powerline face 1."
  :group 'newline)

(defface newline-inactive2 '((t (:background "grey20"))) "Powerline face 2."
  :group 'newline)

(defun newline-percent-xpm (color1 color2)
  (propertize "  "
              'display
              (let (pmax
                    pmin
                    (ws (window-start))
                    (we (window-end)))
                (save-restriction
                  (widen)
                  (setq pmax (point-max))
                  (setq pmin (point-min)))
                (nl/percent-xpm (frame-char-height) pmax pmin we ws 15 color1 color2))))


(defun newline-mouse (click-group click-type string)
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


(defun newline-minor-modes ()
  (mapconcat (lambda (mm)
               (propertize mm
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line down-mouse-1]   (newline-mouse 'minor 'menu mm))
                                        (define-key map [mode-line mouse-2]        (newline-mouse 'minor 'help mm))
                                        (define-key map [mode-line down-mouse-3]   (newline-mouse 'minor 'menu mm))
                                        (define-key map [header-line down-mouse-3] (newline-mouse 'minor 'menu mm))

                                        map)))
             (split-string (format-mode-line minor-mode-alist)) " "))

(defun newline-major-mode ()
  (propertize mode-name
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defun newline-narrow ()
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

(defun newline-vc ()
  (when (and (buffer-file-name (current-buffer))
             vc-mode)
    (symbol-name (vc-mode-line (buffer-file-name (current-buffer))))))

(defun newline-concat (&rest strings)
  "Concatonate STRINGS and pad sides by spaces."
  (concat
   " "
   (mapconcat 'identity (delq nil strings) " ")
   " "))

(defun newline-fill ()
  "Justify right by filling with spaces to right fringe, 20 should be calculated."
  (if (eq 'right (get-scroll-bar-mode))
      (propertize " " 'display '((space :align-to (- right-fringe 21))))
    (propertize " " 'display '((space :align-to (- right-fringe 24))))))

(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)))
                        (face1 (if active 'newline-active1 'newline-inactive1))
                        (face2 (if active 'newline-active2 'newline-inactive2)))
                  (concat
                   (propertize (newline-concat
                                "%*"
                                "%I"
                                (format-mode-line mode-line-buffer-identification)
                                ))
                   (propertize " " 'display
                               (nl/arrow-xpm-left
                                (frame-char-height)
                                nil
                                (face-attribute face1 :background)))
                   (propertize
                    (newline-concat
                     (newline-major-mode)
                     mode-line-process
                     (newline-minor-modes))
                    'face face1)
                   (propertize " " 'display
                               (nl/arrow-xpm-left
                                (frame-char-height)
                                (face-attribute face1 :background)
                                (face-attribute face2 :background)
                                ))
                   (propertize
                    (concat
                     (newline-narrow)
                     global-mode-string
                     (newline-vc)
                     (newline-fill)
                     )
                    'face face2)
                   (propertize " " 'display
                               (nl/arrow-xpm-right
                                (frame-char-height)
                                (face-attribute face1 :background)
                                (face-attribute face2 :background)))
                   (propertize
                    (newline-concat
                     "%4l :%3c"
                     )
                    'face face1)
                   (propertize " " 'display
                               (nl/arrow-xpm-right
                                (frame-char-height)
                                nil
                               (face-attribute face1 :background)))
                   (propertize
                    (newline-concat "%6p"))

                   (newline-percent-xpm (face-attribute face2 :background)
                                        nil)
                   )))))

