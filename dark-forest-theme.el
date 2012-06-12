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
(eval-when-compile (require 'cl)) ;; case
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

(defun dfb-clamp (num min-num max-num)
  (min (max num min-num) max-num))

(defun dfb-hsv (base saturation-offset value-offset)
  "Given the base (HUE SATURATION VALUE), applies saturation-offset
and value and returns a color hex string."
  (hexrgb-hsv-to-hex (/ (nth 0 base) 360.0)
                     (/ (dfb-clamp (+ (nth 1 base) saturation-offset) 0 100)
                        100.0)
                     (/ (dfb-clamp (+ (nth 2 base) value-offset) 0 100)
                        100.0)))

(defun dfb-gray (lum)
  "Builds our grays that have just the slightest hint of amber in therm."
  (hexrgb-hsv-to-hex (/ 51.0 360.0)
                     (/ 3.0 100.0)
                     (/ lum 100.0)))

(let* (;; First are all the base colors. These are hue/saturation/value
       ;; triples and are mostly based on the default font-lock. Integer in terms
       ;; of 360 degrees for hue / 100 percent for saturation and value.
       (dfb-red        '(  0   45  90))
       (dfb-orange     '( 17   52 100))
       (dfb-yellow     '( 51   45  93))
       (dfb-green      '(120   39  98))
       (dfb-cyan       '(180   60  90))
       (dfb-blue       '(210   46  95))
       (dfb-violet     '(280   45  90))

      (df-fg        (dfb-hsv dfb-yellow 0 0))
      (df-fg-white  (dfb-gray 93))

      (df-gray-80    (dfb-gray 80))
      (df-gray       (dfb-gray 70))
      (df-gray-60    (dfb-gray 60))
      (df-dim-gray   (dfb-gray 45))
      (df-gray-30    (dfb-gray 30))

      (df-ssl-blue  (dfb-hsv dfb-blue   -25 -5))

      ;; The superlight colors.
      (df-sl-blue    (dfb-hsv dfb-blue   -15 5))
      (df-sl-orange  (dfb-hsv dfb-orange -15 5))

      ;; The light set of colors.
      (df-l-red    (dfb-hsv dfb-red    -5 5))
      (df-l-orange (dfb-hsv dfb-orange -5 5))
      (df-l-yellow (dfb-hsv dfb-yellow -5 5))
      (df-l-green  (dfb-hsv dfb-green  -5 5))
      (df-l-cyan   (dfb-hsv dfb-cyan   -5 5))
      (df-l-blue   (dfb-hsv dfb-blue   -5 5))

      ;; "Medium" colors.
      (df-m-red    (dfb-hsv dfb-red 0 0))
      (df-m-orange (dfb-hsv dfb-orange 0 0))
      (df-m-yellow (dfb-hsv dfb-yellow 0 0))
      (df-m-green  (dfb-hsv dfb-green 0 0))
      (df-m-cyan   (dfb-hsv dfb-cyan 0 0))
      (df-m-blue   (dfb-hsv dfb-blue 0 0))
      (df-m-violet (dfb-hsv dfb-violet 0 0))

      ;; Bolder colors.
      (df-b-yellow (dfb-hsv dfb-yellow 20 10))

      ;; "Dark" colors, which have saturation 55 and value of 80. Mainly used
      ;; for flyspell checking. Significantly darker than any of the above.
      (df-d-red    (dfb-hsv dfb-red 15 -15))
      (df-d-orange (dfb-hsv dfb-orange 15 -15))
      (df-d-yellow (dfb-hsv dfb-yellow 15 -15))
      (df-d-green (dfb-hsv dfb-green 15 -15))
      (df-d-cyan (dfb-hsv dfb-cyan 15 -15))
      (df-d-blue (dfb-hsv dfb-blue 15 -15))
      (df-d-violet (dfb-hsv dfb-violet 15 -15))

      ;; "Black" background colors. Our "gray" has the slightest hint of amber
      ;; tint, but this is pure black/gray.
      (df-bg      "#1a1a1a")           ; 0/0/10
      (df-bg-2    "#333333")           ; 0/0/20
      (df-bg-3    "#4D4D4D")           ; 0/0/30

      (df-bg-red  "#660000")           ; 0/100/40
      (df-bg-blue "#3D4F66")           ; 214/40/40

      ;; Mode specifiers. These are shorthand for the various places we have.
      (truecolor '((class color) (min-colors 4096)))
      (xterm256  '((class color) (min-colors 256)))
      (term16    '((class color) (min-colors 16)))
      (term8     '((class color) (min-colors 8)))

      ;; Final color lists. If at all possible, use these properties instead of
      ;; including raw colors per above.
      (dark-forest-fg-white `((,truecolor (foreground ,df-fg-white))
                              (t (:foreground "white"))))

      (dark-forest-gray `((,truecolor (:foreground ,df-gray))
                          (t (:foreground "gray"))))
      (dark-forest-dim-gray `((,truecolor (:foreground ,df-dim-gray))
                              (t (:foregournd "gray"))))

      (dark-forest-ssl-blue `((,truecolor (:foreground ,df-ssl-blue))
                              (t (:foreground "blue"))))

      (dark-forest-sl-blue `((,truecolor (:foreground ,df-sl-blue))
                             (t (:foreground "blue"))))
      (dark-forest-sl-orange `((,truecolor (:foreground ,df-sl-orange))
                               (t (:foreground "orange"))))

      (dark-forest-l-red `((,truecolor (:foreground ,df-l-red))
                           (t (:foreground "red"))))
      (dark-forest-l-orange `((,truecolor (:foreground ,df-l-orange))
                              (t (:foreground "orange"))))
      (dark-forest-l-yellow `((,truecolor (:foreground ,df-l-yellow))
                              (t (:foreground "yellow"))))
      (dark-forest-l-green `((,truecolor (:foreground ,df-l-green))
                              (t (:foreground "green"))))
      (dark-forest-l-cyan `((,truecolor (:foreground ,df-l-cyan))
                              (t (:foreground "cyan"))))
      (dark-forest-l-blue `((,truecolor (:foreground ,df-l-blue))
                            (t (:foreground "blue"))))

      (dark-forest-m-red `((,truecolor (:foreground ,df-m-red))
                           (t (:foreground "red"))))
      (dark-forest-m-orange `((,truecolor (:foreground ,df-m-orange))
                              (t (:foreground "orange"))))
      (dark-forest-m-yellow `((,truecolor (:foreground ,df-m-yellow))
                              (t (:foreground "yellow"))))
      (dark-forest-m-green `((,truecolor (:foreground ,df-m-green))
                              (t (:foreground "green"))))
      (dark-forest-m-cyan `((,truecolor (:foreground ,df-m-cyan))
                              (t (:foreground "cyan"))))
      (dark-forest-m-blue `((,truecolor (:foreground ,df-m-blue))
                            (t (:foreground "blue"))))
      (dark-forest-m-violet `((,truecolor (:foreground ,df-m-violet))
                            (t (:foreground "violet"))))

      )
  (custom-theme-set-faces
   'dark-forest

   ;; faces.el
   `(default ((t (:background ,df-bg :foreground ,df-fg
                              :weight normal :width normal))))
   '(cursor ((t (:background "yellow" :foreground "black"))))
   `(escape-glyph ,dark-forest-l-cyan)
   `(minibuffer-prompt ,dark-forest-l-cyan)
   '(highlight ((t (:weight bold :underline t))))
   `(region ((t (:background ,df-bg-3))))
   `(secondary-selection ((t (:background ,df-bg-2))))
   `(trailing-whitespace ((t (:background ,df-m-red))))

   '(button ((t (:inherit link))))
   `(link ((t (:foreground ,df-m-cyan :underline t))))
   `(link-visited ((t (:inherit link :foreground ,df-m-violet))))
   '(fringe ((t (:background "grey10"))))
   '(match ((t (:background "RoyalBlue3"))))
   '(next-error ((t (:inherit region))))

   `(shadow ,dark-forest-gray)
   `(error ((t (:inherit error :foreground ,df-l-red :weight bold))))
   `(warning ,dark-forest-l-orange)

   ;; ---- After this line only new, validated stuff ----

   ;; Calendar
   `(holiday ,dark-forest-l-red)
   `(calendar-today ,dark-forest-l-green)
   `(diary ,dark-forest-l-blue)

   ;; Comint
   `(comint-highlight-input ,dark-forest-fg-white)
   `(comint-highlight-prompt ,dark-forest-l-cyan)

   ;; Compilation
   `(compilation-info ((t (:foreground ,df-m-green :weight normal))))
   `(compilation-line-number ,dark-forest-sl-blue)
   `(compilation-column-number ,dark-forest-sl-orange)
   `(compilation-error ,dark-forest-m-red)
   `(compilation-warning ((t (:foreground ,df-d-yellow))))

   ;; Custom mode
   `(custom-invalid ,dark-forest-l-red)
   `(custom-group-tag ,dark-forest-m-blue)
   `(custom-state ,dark-forest-m-green)
   `(custom-variable-tag ,dark-forest-l-blue)
   `(custom-comment-tag ,dark-forest-gray)
   `(custom-button ((t (:background ,df-gray-80))))
   `(custom-button-mouse ((t (:background ,df-fg-white))))
   ;; TODO(erg): Theoretically there are other faces in this group, but I can't
   ;; see them ever used. :-/

   ;; Diff mode
   `(diff-file-header ((t (:weight normal :foreground ,df-b-yellow
                           :background ,df-bg-2))))
   `(diff-header ((t (:foreground ,df-fg
                      :background ,df-bg-2))))
   `(diff-added ,dark-forest-l-green)
   `(diff-removed ,dark-forest-l-red)

   ;; EMMS
   `(emms-stream-name ((t (:weight normal :foreground ,df-b-yellow))))
   `(emms-stream-url-face ,dark-forest-sl-blue)
   `(emms-playlist-selected-face ,dark-forest-l-blue)
   `(emms-playlist-track-face ,dark-forest-m-yellow)

   ;; erc
   `(erc-button ((t (:foreground ,df-m-cyan :weight normal :underline t))))
   `(erc-current-nick-face ,dark-forest-l-red)
   `(erc-error-face ,dark-forest-m-red)
   `(erc-input-face ((t (:weight normal :foreground ,df-fg-white))))
   `(erc-keyword-face ,dark-forest-m-green)
   `(erc-nick-default-face ((t (:weight normal :foreground ,df-sl-blue))))
   `(erc-notice-face ((t (:weight normal :foreground ,df-dim-gray))))
   `(erc-prompt-face ((t (:foreground "black" :background ,df-m-blue))))
   `(erc-timestamp-face ,dark-forest-m-green)
   `(erc-my-nick-face ,dark-forest-l-red)

   ;; TODO: Figure out what to do about teal.
   `(fg:erc-color-face0 ,dark-forest-fg-white)
   `(fg:erc-color-face1 ((t (:foreground "black"))))
   `(fg:erc-color-face2 ((t (:foreground ,df-d-blue))))
   `(fg:erc-color-face3 ((t (:foreground ,df-d-green))))
   `(fg:erc-color-face4 ((t (:foreground ,df-d-red))))
   `(fg:erc-color-face5 ((t (:foreground ,df-d-yellow))))
   `(fg:erc-color-face6 ((t (:foreground ,df-d-violet))))
   `(fg:erc-color-face7 ((t (:foreground ,df-d-orange))))
   `(fg:erc-color-face8 ,dark-forest-m-yellow)
   `(fg:erc-color-face9 ,dark-forest-m-green)
   '(fg:erc-color-face10 ((t (:foreground "teal"))))
   `(fg:erc-color-face11 ,dark-forest-m-cyan)
   `(fg:erc-color-face12 ,dark-forest-m-blue)
   `(fg:erc-color-face13 ,dark-forest-m-violet)
   `(fg:erc-color-face14 ,dark-forest-dim-gray)
   `(fg:erc-color-face15 ,dark-forest-gray)
   `(bg:erc-color-face0 ((t (:background ,df-fg-white))))
   `(bg:erc-color-face1 ((t (:background "black"))))
   `(bg:erc-color-face2 ((t (:background ,df-d-blue))))
   `(bg:erc-color-face3 ((t (:background ,df-d-green))))
   `(bg:erc-color-face4 ((t (:background ,df-d-red))))
   `(bg:erc-color-face5 ((t (:background ,df-d-yellow))))
   `(bg:erc-color-face6 ((t (:background ,df-d-violet))))
   `(bg:erc-color-face7 ((t (:background ,df-d-orange))))
   `(bg:erc-color-face8 ((t (:background ,df-m-yellow))))
   `(bg:erc-color-face9 ((t (:background ,df-m-green))))
   '(bg:erc-color-face10 ((t (:background "teal"))))
   `(bg:erc-color-face11 ((t (:background ,df-m-cyan))))
   `(bg:erc-color-face12 ((t (:background ,df-m-blue))))
   `(bg:erc-color-face13 ((t (:background ,df-m-violet))))
   `(bg:erc-color-face14 ((t (:background ,df-dim-gray))))
   `(bg:erc-color-face15 ((t (:background ,df-gray))))

   ;; Flyspell colors
   `(flyspell-duplicate ((t (:foreground ,df-d-yellow :weight bold))))
   `(flyspell-incorrect ((t (:foreground ,df-d-red :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ,dark-forest-l-blue)
   `(font-lock-comment-face ,dark-forest-dim-gray)
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ,dark-forest-gray)
   `(font-lock-doc-face ,dark-forest-dim-gray)
   `(font-lock-function-name-face ,dark-forest-m-blue)
   `(font-lock-keyword-face ,dark-forest-m-cyan)
   '(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ,dark-forest-ssl-blue)
   '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
   '(font-lock-regexp-grouping-construct ((t (:weight bold))))
   `(font-lock-string-face ,dark-forest-m-orange)
   `(font-lock-type-face ,dark-forest-m-green)
   `(font-lock-variable-name-face ,dark-forest-sl-blue)
   `(font-lock-warning-face ((t (:inherit error :foreground ,df-l-red
                                          :weight bold))))

   ;; help-argument: Just a slight lightening of arguments to make them stand
   ;; out just a bit, but not a :wight bold bit. Consider making this a sl
   ;; variant.
   `(help-argument-name ,dark-forest-l-yellow)

   ;; ido
   `(ido-first-match ((t (:foreground ,df-m-yellow :weight bold))))
   `(ido-only-match ,dark-forest-m-green)
   `(ido-subdir ,dark-forest-l-blue)

   ;; info
   `(info-title-1 ,dark-forest-fg-white)
   `(info-title-2 ,dark-forest-gray)
   `(info-title-3 ,dark-forest-dim-gray)
   `(info-header-node ,dark-forest-fg-white)
   `(info-menu-header ,dark-forest-gray)
   `(info-menu-star ,dark-forest-m-red)

   ;; isearch
   `(isearch ((t (:background ,df-l-red
                  :foreground ,df-bg-red))))
   `(isearch-fail ((t (:background ,df-bg-red))))
   `(lazy-highlight ((t (:background ,df-bg-blue))))
   '(query-replace ((t (:inherit isearch))))

   ;; makefile
   `(makefile-space ((t (:background ,df-m-red))))

   ;; modeline
   `(header-line ((t (:inherit mode-line :background ,df-bg-2
                               :foreground ,df-fg-white :box nil))))
   `(mode-line ((t (:background ,df-gray-60 :foreground "black" :box (:line-width -1 :style released-button)))))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
   `(mode-line-inactive ((t (:inherit mode-line :background ,df-gray-30 :foreground "gray80" :box (:line-width -1 :color "grey40") :weight light))))

   ;; org-mode (WOEFULLY INCOMPLETE ON THE COLORING!)
   `(org-date ((t (:foreground ,df-l-cyan :underline t))))
   `(org-special-keyword ,dark-forest-l-yellow)
   `(org-level-1 ,dark-forest-m-blue)
   `(org-level-2 ,dark-forest-m-cyan)
   `(org-level-3 ,dark-forest-m-green)
   `(org-level-4 ,dark-forest-m-yellow)
   `(org-level-5 ,dark-forest-m-orange)
   `(org-level-6 ,dark-forest-m-red)
   `(org-level-7 ,dark-forest-m-violet)
   `(org-todo ((t (:foreground ,df-l-red :weight bold))))
))

(provide-theme 'dark-forest)
