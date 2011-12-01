(deftheme dark-forest
  "An amber on black theme. A modification of the default light
text on dark background font-lock colors, slightly modified, and
then applied to all of emacs faces. Inspired by Bob Hauser's Black
Forest theme.

The value component of each color has been normalized to .10/1
depending on darkness/lightness. Valid saturation values are
.35/.65. Hues attempt to be multiples of 25.")

;; Original colors in HSV:
;; FG:            51/45/93
;; string/red:    17/52/98
;; builtin/blue:  215/21/86
;; type/green:    120/39/98  (#98fb98)
;; keyword/cyan:  180/100/100

;; Stolen from hexrgb.el until I've settled on the colors. Then I'll remove
;; these and hard code the constants.
(defun hexrgb-hsv-to-rgb (hue saturation value)
  "Convert HUE, SATURATION, VALUE components to RGB (red, green, blue).
Each input component is 0.0 to 1.0, inclusive.
Returns a list of RGB components of value 0.0 to 1.0, inclusive."
  (let (red green blue int-hue fract pp qq tt ww)
    (if (hexrgb-approx-equal 0.0 saturation)
        (setq red    value
              green  value
              blue   value)             ; Gray
      (setq hue      (* hue 6.0)        ; Sectors: 0 to 5
            int-hue  (floor hue)
            fract    (- hue int-hue)
            pp       (* value (- 1 saturation))
            qq       (* value (- 1 (* saturation fract)))
            ww       (* value (- 1 (* saturation (- 1 (- hue int-hue))))))
      (case int-hue
        ((0 6) (setq red    value
                     green  ww
                     blue   pp))
        (1 (setq red    qq
                 green  value
                 blue   pp))
        (2 (setq red    pp
                 green  value
                 blue   ww))
        (3 (setq red    pp
                 green  qq
                 blue   value))
        (4 (setq red    ww
                 green  pp
                 blue   value))
        (otherwise (setq red    value
                         green  pp
                         blue   qq))))
    (list red green blue)))

;; Color "components" would be better in the name than color "value"
;; but this name follows the Emacs tradition (e.g. `x-color-values',
;; 'ps-color-values', `ps-e-x-color-values').
(defun hexrgb-color-values-to-hex (components)
  "Convert list of rgb color COMPONENTS to a hex string, #XXXXXXXXXXXX.
Each X in the string is a hexadecimal digit.
Input COMPONENTS is as for the output of `x-color-values'."
;; Just hard-code 4 as the number of hex digits, since `x-color-values'
;; seems to produce appropriate integer values for `4'.
  (concat "#" (hexrgb-int-to-hex (nth 0 components) 4) ; red
          (hexrgb-int-to-hex (nth 1 components) 4) ; green
          (hexrgb-int-to-hex (nth 2 components) 4))) ; blue

;; Inspired by Elisp Info manual, node "Comparison of Numbers".
(defun hexrgb-approx-equal (x y &optional rfuzz afuzz)
  "Return non-nil if numbers X and Y are approximately equal.
RFUZZ is a relative fuzz factor.  AFUZZ is an absolute fuzz factor.
RFUZZ defaults to 1.0e-8.  AFUZZ defaults to (/ RFUZZ 10).
RFUZZ and AFUZZ are converted to their absolute values.
The algorithm is:
 (< (abs (- X Y)) (+ AFUZZ (* RFUZZ (+ (abs X) (abs Y)))))."
  (setq rfuzz  (or rfuzz 1.0e-8)
        rfuzz  (abs rfuzz)
        afuzz  (or afuzz (/ rfuzz 10))
        afuzz  (abs afuzz))
  (< (abs (- x y)) (+ afuzz (* rfuzz (+ (abs x) (abs y))))))

;; Originally, I used the code from `int-to-hex-string' in `float.el'.
;; This version is thanks to Juri Linkov <juri@jurta.org>.
;;
(defun hexrgb-int-to-hex (int &optional nb-digits)
  "Convert integer argument INT to a #XXXXXXXXXXXX format hex string.
Each X in the output string is a hexadecimal digit.
NB-DIGITS is the number of hex digits.  If INT is too large to be
represented with NB-DIGITS, then the result is truncated from the
left.  So, for example, INT=256 and NB-DIGITS=2 returns \"00\", since
the hex equivalent of 256 decimal is 100, which is more than 2 digits."
  (setq nb-digits  (or nb-digits 4))
  (substring (format (concat "%0" (int-to-string nb-digits) "X") int) (- nb-digits)))


(defun hexrgb-hsv-to-hex (hue saturation value)
  "Return the hex RBG color string for inputs HUE, SATURATION, VALUE.
The inputs are each in the range 0 to 1.
The output string is of the form \"#RRRRGGGGBBBB\"."
  (hexrgb-color-values-to-hex
   (mapcar (lambda (x) (floor (* x 65535.0))) (hexrgb-hsv-to-rgb hue saturation value))))

(defun dfb-hsv (base saturation-offset value)
  "Given the base (HUE . SATURATION), applies saturation-offset
and value and returns a color hex string."
  (hexrgb-hsv-to-hex (/ (nth 0 base) 360.0)
                     (/ (+ (nth 1 base) saturation-offset) 100.0)
                     (/ value 100.0)))

(let* (;; First are all the dark-forest-base colors. These are Hue/Saturation
      ;; pairs and are mostly based on the default font-lock. Integer in terms
      ;; of 360 degrees for hue / 100 percent for saturation.
      (dfb-red        '(  0   45))
      (dfb-orange     '( 17   52))
      (dfb-yellow     '( 51   45))
      (dfb-green      '(120   39))
      (dfb-cyan       '(180  100))  ;; TODO: Make this more faded?
      (dfb-blue       '(203   92))
      (dfb-violet     '(280   45))



      (dark-forest-fg    ;  (dfb-hsv dfb-yellow 0 95))
"#E6D67E")           ; 50/50/95

      ;; The superlight colors have saturation 20 and value of 100.
      (dark-forest-sl-blue  "#C7DFFF")          ; 214/20/100
      (dark-forest-sl-orange "#FFDACC")         ; 17/20/100

      ;; The light set of colors have saturation 40 and value of 100.
      (dark-forest-l-red    "#FF9999")          ; 0/40/100
      (dark-forest-l-orange "#FFC499")          ; 17/40/100
      (dark-forest-l-yellow "#FFF099")          ; 51/40/100
      (dark-forest-l-green  "#AAFF99")          ; 110/40/100
      (dark-forest-l-cyan   "#99FFFF")          ; 180/40/100
      (dark-forest-l-blue   "#99C5FF")          ; 214/40/100

      ;; "Medium" colors, which have saturation 45 and value of 90.
      (dark-forest-m-red    "#E67E7E")          ; 0/45/90
      (dark-forest-m-orange "#E6A97E")          ; 25/45/90
      (dark-forest-m-yellow "#E6D67E")          ; 51/45/90
      (dark-forest-m-green  "#8FE67E")          ; 110/45/90
      (dark-forest-m-cyan   "#7EE6E6")          ; 180/45/90
      (dark-forest-m-blue   "#7EABE6")          ; 214/45/90
      (dark-forest-m-violet "#C37EE6")          ; 280/45/90


      ;; "Bold" colors, which have saturation of 50 and value of 100.
      (dark-forest-b-orange "#FF9F80")          ; 17/50/100

      ;; "Dark" colors, which have saturation 55 and value of 80. Mainly used
      ;; for flyspell checking. Significantly darker than any of the above.
      (dark-forest-d-red    "#CC2929")          ; 0/80/80
      (dark-forest-d-yellow "#CCB429")          ; 51/80/80

      ;; "Black" background colors. All are variants of a theme.
      (dark-forest-bg      "#1a1a1a")           ; 0/0/10
      (dark-forest-bg-2    "#333333")           ; 0/0/20
      (dark-forest-bg-3    "#4D4D4D")           ; 0/0/30

      (dark-forest-bg-red  "#660000")           ; 0/100/40
      (dark-forest-bg-blue "#3D4F66")           ; 214/40/40

      )
  (custom-theme-set-faces
   'dark-forest

   ;; faces.el
   `(default ((t (:background ,dark-forest-bg :foreground ,dark-forest-fg
                              :weight normal :width normal))))
   '(cursor ((t (:background "yellow" :foreground "black"))))
   `(escape-glyph ((t (:foreground ,dark-forest-l-cyan))))
   `(minibuffer-prompt ((t (:foreground ,dark-forest-l-cyan))))
   '(highlight ((t (:weight bold :underline t))))
   `(region ((t (:background ,dark-forest-bg-3))))
   `(secondary-selection ((t (:background ,dark-forest-bg-2))))
   `(trailing-whitespace ((t (:background ,dark-forest-m-red))))

   '(button ((t (:inherit link))))
   `(link ((t (:foreground ,dark-forest-m-cyan :underline t))))
   `(link-visited ((t (:inherit link :foreground ,dark-forest-m-violet))))
   '(fringe ((t (:background "grey10"))))
   '(header-line ((t (:inherit mode-line :background "gray20" :foreground "gray90" :box nil))))
   '(mode-line ((t (:background "gray50" :foreground "black" :box (:line-width -1 :style released-button)))))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
   '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40") :weight light))))
   '(match ((t (:background "RoyalBlue3"))))
   '(next-error ((t (:inherit region))))
   ;; error and warning.

   ;; ---- After this line only new, validated stuff ----

   ;; Compilation
   `(compilation-info ((t (:foreground ,dark-forest-m-green :weight bold))))
   `(compilation-line-number ((t (:foreground ,dark-forest-sl-blue))))
   `(compilation-column-number ((t (:foreground ,dark-forest-sl-orange))))
   `(compilation-error ((t (:foreground ,dark-forest-m-red))))
   `(compilation-warning ((t (:foreground ,dark-forest-d-yellow))))

   ;; Diff mode
   `(diff-file-header ((t (:foreground ,dark-forest-fg
                           :background ,dark-forest-bg-2))))
   `(diff-header ((t (:foreground ,dark-forest-fg
                      :background ,dark-forest-bg-2))))
   `(diff-added ((t (:foreground ,dark-forest-l-green))))
   `(diff-removed ((t (:foreground ,dark-forest-l-red))))

   ;; Flyspell colors
   `(flyspell-duplicate ((t (:foreground ,dark-forest-d-yellow :weight bold))))
   `(flyspell-incorrect ((t (:foreground ,dark-forest-d-red :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,dark-forest-l-blue))))
   '(font-lock-comment-face ((t (:foreground "dim gray"))))
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   '(font-lock-constant-face ((t (:foreground "gray"))))
   '(font-lock-doc-face ((t (:foreground "dim gray"))))
   `(font-lock-function-name-face ((t (:foreground ,dark-forest-m-blue))))
   `(font-lock-keyword-face ((t (:foreground ,dark-forest-m-cyan))))
   '(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:foreground ,dark-forest-l-blue))))
   '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
   '(font-lock-regexp-grouping-construct ((t (:weight bold))))
   `(font-lock-string-face ((t (:foreground ,dark-forest-b-orange))))
   `(font-lock-type-face ((t (:foreground ,dark-forest-l-green))))
   `(font-lock-variable-name-face ((t (:foreground ,dark-forest-sl-blue))))
   `(font-lock-warning-face ((t (:inherit error :foreground ,dark-forest-l-red
                                          :weight bold))))

   ;; TODO: ibuffer when that gets updated to use defface stuff.

   ;; ido
   `(ido-first-match ((t (:foreground ,dark-forest-m-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,dark-forest-m-orange))))
   `(ido-subdir ((t (:foreground ,dark-forest-l-blue))))

   ;; isearch
   `(isearch ((t (:background ,dark-forest-l-red
                  :foreground ,dark-forest-bg-red))))
   `(isearch-fail ((t (:background ,dark-forest-bg-red))))
   `(lazy-highlight ((t (:background ,dark-forest-bg-blue))))
   '(query-replace ((t (:inherit isearch))))
))

(provide-theme 'dark-forest)

