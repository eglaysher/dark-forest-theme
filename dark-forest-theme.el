(deftheme dark-forest
  "An amber on black theme. A modification of the default light
text on dark background font-lock colors, slightly modified, and
then applied to all of emacs faces. Inspired by Bob Hauser's Black
Forest theme.

We start off with the following base colors as the 'medium' variants:

  red     '(  0   45  90)
  orange  '( 17   52 100)
  yellow  '( 51   45  93)
  green   '(120   39  98)
  cyan    '(180   60  90)
  blue    '(210   46  95)
  violet  '(280   45  90)

And then performed various hsv shifts across this pallet for
light, dark, bold, etc variants. (The hsv calculations used to be
done at build time; it's only recently that I considered the base
pallet to be complete enough that I started hard coding the hex
into this file.)")

(let* (
      (df-fg        "#EE13DE0182F1")
      (df-fg-white  "#EE13ED01E6EF") ; gray-93

      ;; Our default grays have a subtle tint of amber in them. (hue=51, sat=3)
      (df-gray-80    "#CCCCCBE0C6A7")
      (df-gray       "#B332B264ADD2")
      (df-gray-60    "#999998E894FD")
      (df-dim-gray   "#733272AE6FBE")
      (df-gray-30    "#4CCC4C744A7E")
      (df-gray-15    "#2666263A253F")

      ;; The super-superlight colors. (hsv shift -25, -5)
      (df-ssl-blue  "#B603CE34E665")

      ;; The superlight colors. (hsv shift -15, 5)
      (df-sl-blue    "#B0A3D851FFFF")
      (df-sl-orange  "#FFFFBC1DA147")

      ;; The light set of colors. (hsv shift -5, 5)
      (df-l-red    "#F33291EA91EA")
      (df-l-orange "#FFFFA9C487AD")
      (df-l-yellow "#FAE0EBD29686")
      (df-l-green  "#A8F5FFFFA8F5")
      (df-l-cyan   "#6D70F332F332")
      (df-l-blue   "#9709CB84FFFF")

      ;; "Medium" colors. (hsv shift 0, 0)
      (df-m-red    "#E6657EB77EB7")
      (df-m-orange "#FFFFA0987AE0")
      (df-m-yellow "#EE13DE0182F1")
      (df-m-green  "#9908FAE09908")
      (df-m-cyan   "#5C28E665E665")
      (df-m-blue   "#8353BB42F332")
      (df-m-violet "#C3D67EB7E665")

      ;; Bolder colors. (hsv shift 20, 10)
      (df-b-yellow "#FFFFE7095999")

      ;; "Dark" colors, which have saturation 55 and value of 80. Mainly used
      ;; for flyspell checking. Significantly darker than any of the
      ;; above. (hsv shift 15, -15)
      (df-d-red    "#BFFF4CCC4CCC")
      (df-d-orange "#D998711D47CE")
      (df-d-yellow "#C7ADB5B44FDE")
      (df-d-green "#61BDD47A61BD")
      (df-d-cyan "#2FFFBFFFBFFF")
      (df-d-blue "#4FDE8E55CCCC")
      (df-d-violet "#99994CCCBFFF")

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
      (dark-forest-fg-white `((,truecolor (:foreground ,df-fg-white))
                              (,xterm256 (:foreground "#e4e4e4"))
                              (t (:foreground "white"))))

      (dark-forest-gray `((,truecolor (:foreground ,df-gray))
                          (,xterm256 (:foreground "#b2b2b2"))
                          (t (:foreground "gray"))))
      (dark-forest-dim-gray `((,truecolor (:foreground ,df-dim-gray))
                              (,xterm256 (:foreground "#767676"))
                              (t (:foregournd "gray"))))

      (dark-forest-ssl-blue `((,truecolor (:foreground ,df-ssl-blue))
                              (,xterm256 (:foreground "#afd7d7"))
                              (t (:foreground "blue"))))

      (dark-forest-sl-blue `((,truecolor (:foreground ,df-sl-blue))
                             (,xterm256 (:foreground "#afd7ff"))
                             (t (:foreground "blue"))))
      (dark-forest-sl-orange `((,truecolor (:foreground ,df-sl-orange))
                              (,xterm256 (:foreground "#ffd7d7"))
                              (t (:foreground "orange"))))

      (dark-forest-l-red `((,truecolor (:foreground ,df-l-red))
                           (,xterm256 (:foreground "#ff8787"))
                           (t (:foreground "red"))))
      (dark-forest-l-orange `((,truecolor (:foreground ,df-l-orange))
                              (,xterm256 (:foreground "#ffd7af"))
                              (t (:foreground "orange"))))
      (dark-forest-l-yellow `((,truecolor (:foreground ,df-l-yellow))
                              (t (:foreground "yellow"))))
      (dark-forest-l-green `((,truecolor (:foreground ,df-l-green))
                             (,xterm256 (:foreground "#afffaf"))
                             (t (:foreground "green"))))
      (dark-forest-l-cyan `((,truecolor (:foreground ,df-l-cyan))
                            (,xterm256 (:foreground "#5fffff"))
                            (t (:foreground "cyan"))))
      (dark-forest-l-blue `((,truecolor (:foreground ,df-l-blue))
                            (,xterm256 (:foreground "#87afff"))
                            (t (:foreground "blue"))))

      (dark-forest-m-red `((,truecolor (:foreground ,df-m-red))
                           (,xterm256 (:foreground "#ff5f5f"))
                           (t (:foreground "red"))))
      (dark-forest-m-orange `((,truecolor (:foreground ,df-m-orange))
                              (,xterm256 (:foreground "#ffaf87"))
                              (t (:foreground "orange"))))
      (dark-forest-m-yellow `((,truecolor (:foreground ,df-m-yellow))
                              (t (:foreground "yellow"))))
      (dark-forest-m-green `((,truecolor (:foreground ,df-m-green))
                             (,xterm256 (:foreground "#87ff87"))
                             (t (:foreground "green"))))
      (dark-forest-m-cyan `((,truecolor (:foreground ,df-m-cyan))
                            (,xterm256 (:foreground "#5fd7d7"))
                            (t (:foreground "cyan"))))
      (dark-forest-m-blue `((,truecolor (:foreground ,df-m-blue))
                            (,xterm256 (:foreground "#5f87ff"))
                            (t (:foreground "blue"))))
      (dark-forest-m-violet `((,truecolor (:foreground ,df-m-violet))
                            (t (:foreground "violet"))))

      )
  (custom-theme-set-faces
   'dark-forest

   ;; faces.el
   `(default ((t (:background ,df-bg :foreground ,df-fg
                              :weight normal :width normal))))
   '(cursor ((t (:background "#ffff5f" :foreground "black"))))
   `(escape-glyph ,dark-forest-l-cyan)
   `(minibuffer-prompt ,dark-forest-l-cyan)
   '(highlight ((t (:weight bold :underline t))))
   `(region ((t (:background ,df-bg-3))))
   `(secondary-selection ((t (:background ,df-bg-2))))
   `(trailing-whitespace ((t (:background ,df-m-red))))

   '(button ((t (:inherit link))))
   `(link ((t (:foreground ,df-m-cyan :underline t))))
   `(link-visited ((t (:inherit link :foreground ,df-m-violet))))
   `(fringe ((t (:background ,df-gray-15))))
   '(match ((t (:background "RoyalBlue3"))))
   `(menu ((((type tty)) (:background "black" :foreground "white"))))
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
   ;; out just a bit, but not a :weight bold bit. Consider making this a sl
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
   `(header-line ((,truecolor (:inherit mode-line :background ,df-bg-2
                               :foreground ,df-fg-white :box nil))
                  (t (:background "black" :foreground "white"))))
   `(mode-line
     ((,truecolor (:background ,df-gray-60 :foreground "black"
                   :box (:line-width -1 :style released-button)))
      (,xterm256 (:background "white" :foreground "black"))
      (t (:background "white" :foreground "black" :inverse-video nil))))
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
