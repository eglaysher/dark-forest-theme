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

;; This at least works as is.
(defun df-build-fg (color-tuple &rest e)
  "For Dark Forest theme. Takes a tuple and a set of properties
  and creates truecolor, 256 safe and 16 color variants of a
  foreground."
  (let ((truecolor (append `(:foreground ,(elt color-tuple 0)) e))
        (xterm256 (append `(:foreground ,(elt color-tuple 1)) e))
        (simple (append `(:foreground ,(elt color-tuple 2)) e)))
    `((((class color) (min-colors 4096)) ,truecolor)
      (((class color) (min-colors 256)) ,xterm256)
      (t ,simple))))

(defun df-build-bg (color-tuple &rest e)
  "For Dark Forest theme. Like `df-build-fg', but for the background."
  (let ((truecolor (append `(:background ,(elt color-tuple 0)) e))
        (xterm256 (append `(:background ,(elt color-tuple 1)) e))
        (simple (append `(:background ,(elt color-tuple 2)) e)))
    `((((class color) (min-colors 4096)) ,truecolor)
      (((class color) (min-colors 256)) ,xterm256)
      (t ,simple))))

(defun df-build-fgbg (fg-tuple bg-tuple &rest e)
  "For Dark Forest theme. Like `df-build-fg', but takes tuples
  for both the foregorund and background."
  (let ((truecolor (append `(:foreground ,(elt fg-tuple 0)
                             :background ,(elt bg-tuple 0)) e))
        (xterm256 (append `(:foreground ,(elt fg-tuple 1)
                            :background ,(elt bg-tuple 1)) e))
        (simple (append `(:foreground ,(elt fg-tuple 2)
                          :background ,(elt bg-tuple 2)) e)))
    `((((class color) (min-colors 4096)) ,truecolor)
      (((class color) (min-colors 256)) ,xterm256)
      (t ,simple))))


;; You might want to view this in rainbow-mode.
(let* (
      (fg        ["#EE13DE0182F1" "#ffd787" "brightwhite"])
      (fg-white  ["#EE13ED01E6EF" "#eeeeee" "brightwhite"]) ; gray-93

      ;; "Black" background colors. Our "gray" has the slightest hint of amber
      ;; tint, but this is pure black/gray. These colors are slightly adjusted
      ;; on the xterm256 from the hsv values on the right.
      (bg        ["#1a1a1a" "#1c1c1c" "black"])             ; 0/0/10
      (bg-2      ["#333333" "#303030" "black"])             ; 0/0/20
      (bg-3      ["#4D4D4D" "#4e4e4e" "brightblack"])       ; 0/0/30

      ;; Our grays on truecolor have a subtle tint of amber in them. (hue=51,
      ;; sat=3) Otherwise, we fall back to the closest 256 safe gray.
      (gray-80   ["#CCCCCBE0C6A7" "#c6c6c6" "gray"])
      (gray      ["#B332B264ADD2" "#b2b2b2" "gray"])
      (gray-60   ["#999998E894FD" "#9e9e9e" "white"])
      (gray-50   ["#7FFF7F6C7C28" "#7f7f7f" "white"])
      (gray-45   ["#733272AE6FBE" "#767676" "brightblack"])
      (gray-40   ["#666665F06353" "#666666" "brightblack"])
      (gray-30   ["#4CCC4C744A7E" "#4e4e4e" "gray"])
      (gray-25   ["#3FFF3FB63E14" "#3a3a3a" "gray"])
      (gray-20   ["#333332F831A9" "#303030" "gray"])
      (gray-15   ["#2666263A253F" "#262626" "gray"])
      (gray-10   ["#1999197C18D4" "#1c1c1c" "gray"])

      ;; The super-superlight colors. (hsv shift -25, -5)
      (ssl-blue  ["#B603CE34E665" "#afd7d7" "blue"])

      ;; The superlight colors. (hsv shift -15, 5)
      (sl-blue   ["#B0A3D851FFFF" "#afd7ff" "blue"])
      (sl-orange ["#FFFFBC1DA147" "#ffd7d7" "orange"])

      ;; The light set of colors. (hsv shift -5, 5)
      (l-red     ["#F33291EA91EA" "#ffafaf" "brightred"])
      (l-orange  ["#FFFFA9C487AD" "#ffd7af" "brightyellow"])
      (l-yellow  ["#FAE0EBD29686" "#ffffaf" "brightyellow"])
      (l-green   ["#A8F5FFFFA8F5" "#afffaf" "brightgreen"])
      (l-cyan    ["#6D70F332F332" "#5fffff" "brightcyan"])
      (l-blue    ["#9709CB84FFFF" "#87afff" "brightblue"])

      ;; "Medium" colors. (hsv shift 0, 0)
      (m-red     ["#E6657EB77EB7" "#ff8787" "brightred"])
      (m-orange  ["#FFFFA0987AE0" "#ffaf87" "brightyellow"])
      (m-yellow  ["#EE13DE0182F1" "#ffff87" "brightyellow"])
      (m-green   ["#9908FAE09908" "#87ff87" "brightgreen"])
      (m-cyan    ["#5C28E665E665" "#5fd7d7" "brightcyan"])
      (m-blue    ["#8353BB42F332" "#87afff" "brightblue"])
      (m-violet  ["#C3D67EB7E665" "#d787d7" "birghtmagenta"])

      ;; Bolder colors. (hsv shift 20, 10)
      (b-yellow  ["#FFFFE7095999" "#ffff5f" "yellow"])

      ;; "Dark" colors, which have saturation 55 and value of 80. Mainly used
      ;; for flyspell checking. Significantly darker than any of the
      ;; above. (hsv shift 15, -15)
      (d-red     ["#BFFF4CCC4CCC" "#af5f5f" "red"])
      (d-orange  ["#D998711D47CE" "#d7875f" "orange"])
      (d-yellow  ["#C7ADB5B44FDE" "#d7af5f" "yellow"])
      (d-green   ["#61BDD47A61BD" "#5fd75f" "green"])
      (d-cyan    ["#2FFFBFFFBFFF" "#00afaf" "cyan"])
      (d-blue    ["#4FDE8E55CCCC" "#5f87d7" "blue"])
      (d-violet  ["#99994CCCBFFF" "#875faf" "violet"])

      ;; Temporary black tuple until I figure out what's black and what should
      ;; be bg.
      (black     ["#000000" "#000000" "#000000"])

      ;; Highlight backgrounds. For small areas. (20, -50)
      (bg-hl-red    ["#666623D623D6" "black" "black"])
      (bg-hl-orange ["#7FFF3DF323D6" "black" "black"])
      (bg-hl-yellow ["#6E1463582687" "black" "black"])
      (bg-hl-green  ["#32617AE03261" "black" "black"])
      (bg-hl-cyan   ["#147A66666666" "black" "black"])
      (bg-hl-blue   ["#272A4D2E7332" "black" "black"])
      (bg-hl-violet ["#503623D66666" "black" "black"])

      ;; Dark backgrounds. For large areas. (10, -60)
      (bg-d-red     ["#4CCC228F228F" "black" "black"])
      (bg-d-orange  ["#666638E626E9" "black" "black"])
      (bg-d-yellow  ["#547A4D822603" "black" "black"])
      (bg-d-green   ["#319C6147319C" "black" "black"])
      (bg-d-cyan    ["#170A4CCC4CCC" "black" "black"])
      (bg-d-blue    ["#276C40825999" "black" "black"])
      (bg-d-violet  ["#3EB8228F4CCC" "black" "black"])

      ;; Final color lists. If at all possible, use these properties instead of
      ;; including raw colors per above.
      (dark-forest-fg-white (df-build-fg fg-white))

      (dark-forest-gray (df-build-fg gray))
      (dark-forest-dim-gray (df-build-fg gray-45))

      (dark-forest-ssl-blue (df-build-fg ssl-blue))

      (dark-forest-sl-blue (df-build-fg sl-blue))
      (dark-forest-sl-orange (df-build-fg sl-orange))

      (dark-forest-l-red (df-build-fg l-red))
      (dark-forest-l-orange (df-build-fg l-orange))
      (dark-forest-l-yellow (df-build-fg l-yellow))
      (dark-forest-l-green (df-build-fg l-green))
      (dark-forest-l-cyan (df-build-fg l-cyan))
      (dark-forest-l-blue (df-build-fg l-blue))

      (dark-forest-m-red (df-build-fg m-red))
      (dark-forest-m-orange (df-build-fg m-orange))
      (dark-forest-m-yellow (df-build-fg m-yellow))
      (dark-forest-m-green (df-build-fg m-green))
      (dark-forest-m-cyan (df-build-fg m-cyan))
      (dark-forest-m-blue (df-build-fg m-blue))
      (dark-forest-m-violet (df-build-fg m-violet))
      )
  (custom-theme-set-faces
   'dark-forest

   ;; aprops.el
   `(apropos-symbol ,(df-build-fg fg-white))
   `(apropos-keybinding ,(df-build-fg gray-80 ':inherit 'underline))

   ;; calendar.el
   `(holiday ,dark-forest-l-red)
   `(calendar-today ,dark-forest-l-green)
   `(diary ,dark-forest-l-blue)

   ;; comint.el
   `(comint-highlight-input ,dark-forest-fg-white)
   `(comint-highlight-prompt ,dark-forest-l-cyan)

   ;; company.el
   `(company-tooltip ,(df-build-fgbg black gray-80))
   `(company-tooltip-selection ,(df-build-fgbg fg-white d-blue))
   `(company-tooltip-common ,(df-build-fgbg gray-30 gray-80))
   `(company-tooltip-common-selection ,(df-build-fgbg gray-80 d-blue))
   `(company-tooltip-annotation ,(df-build-fgbg d-red gray-80))
   `(company-tooltip-annotation-selection ,(df-build-fgbg bg-d-red d-blue))
   `(company-scrollbar-fg ,(df-build-bg gray-45))
   `(company-scrollbar-bg ,(df-build-bg bg-2))
   `(company-preview ,(df-build-fg gray-60 ':underline 't))
   `(company-preview-common ,(df-build-fg gray-60 ':underline 't))
   `(company-echo-common ,(df-build-bg bg-d-red))

   ;; compile.el
   `(compilation-error ,dark-forest-m-red)
   `(compilation-warning ,(df-build-fg d-yellow))
   `(compilation-info ,(df-build-fg m-green ':weight 'normal))
   `(compilation-mode-line-fail ,(df-build-fg m-red ':weight 'bold))
   `(compilation-mode-line-run ,(df-build-fg m-yellow))
   `(compilation-mode-line-exit ,(df-build-fg m-green ':weight 'bold))
   `(compilation-line-number ,dark-forest-sl-blue)
   `(compilation-column-number ,dark-forest-sl-orange)

   ;; cus-edit.el (Faces for Customize)
   `(custom-invalid ,(df-build-fg d-red))
   `(custom-rogue ,dark-forest-l-red)
   `(custom-modified ,dark-forest-l-blue)
   `(custom-set ,(df-build-fgbg d-blue bg-3))
   `(custom-changed ,(df-build-fgbg bg-3 d-blue))
   `(custom-themed ,(df-build-fgbg fg-white bg-hl-blue))
   `(custom-button
     ,(df-build-fgbg black gray-60
                     ':box '(:line-width 2 :style released-button)))
   `(custom-button-moude
     ,(df-build-fgbg black gray-80
                     ':box '(:line-width 2 :style released-button)))
   `(custom-button-pressed
     ,(df-build-fgbg black gray-60
                     ':box '(:line-width 2 :style pressed-button)))
   `(custom-state ,(df-build-fg m-green))
   `(custom-comment ,(df-build-fgbg fg bg-2))
   `(custom-comment-tag ,(df-build-fg fg-white))
   `(custom-variable-tag ,dark-forest-l-blue)
   `(custom-group-tag-1 ,(df-build-fg m-red ':weight 'bold
                                    ':height '1.2 ':inherit 'variable-pitch))
   `(custom-group-tag ,(df-build-fg m-blue ':weight 'bold
                                    ':height '1.2 ':inherit 'variable-pitch))
   ;; diff-mode.el
   `(diff-header ,(df-build-fgbg fg bg-2))
   `(diff-file-header ,(df-build-fgbg b-yellow bg-2 ':weight 'normal))
   `(diff-hunk-header ,(df-build-fgbg fg gray-15))
   `(diff-removed ,dark-forest-l-red)
   `(diff-added ,dark-forest-l-green)
   `(diff-indicator-removed ,(df-build-fg m-red))
   `(diff-indicator-added ,(df-build-fg m-green))
   `(diff-function ,(df-build-fgbg fg-white gray-15))
   `(diff-context ,(df-build-fg gray-80))

   ;; faces.el
   `(default ,(df-build-fgbg fg bg ':weight 'normal ':width 'normal))
   `(fixed-pitch ((t (:inherit default))))
   `(shadow ,(df-build-fg gray))
   `(link ,(df-build-fg m-cyan ':underline 't))
   `(link-visited ,(df-build-fg m-violet ':inherit 'link))
   '(highlight ((t (:weight bold :underline t))))
   `(region ,(df-build-bg bg-3))
   `(secondary-selection ,(df-build-bg bg-2))
   `(trailing-whitespace ,(df-build-bg m-red))
   `(escape-glyph ,dark-forest-l-cyan)
   `(mode-line ,(df-build-fgbg black gray-60
                               ':box '(:line-width -1 :style released-button)))
   `(mode-line-buffer-id ,(df-build-fgbg black gray-60 ':weight 'bold
                                         ':inherit 'mode-line))
   `(mode-line-inactive ,(df-build-fgbg gray-80 gray-30
                                        ':box '(:line-width -1 :color "grey40")
                                        ':weight 'light))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                                :style released-button)))))
   `(header-line ,(df-build-fgbg fg-white bg-2 ':inherit 'mode-line ':box 'nil))
   `(minibuffer-prompt ,dark-forest-l-cyan)
   `(fringe ,(df-build-bg gray-15))
   `(cursor ,(df-build-fgbg black b-yellow))
   `(menu ,(df-build-fgbg black gray-60))
   `(help-argument-name ,dark-forest-l-yellow)
   `(shadow ,dark-forest-gray)
   `(error ,(df-build-fg l-red ':weight 'bold ':inherit 'error))
   `(warning ,(df-build-fg l-orange ':weight 'bold))
   `(success ,(df-build-fg l-green ':weight 'bold))
   `(tty-menu-enabled-face ,(df-build-fgbg gray-15 gray-80 ':weight 'bold))
   `(tty-menu-disabled-face ,(df-build-fgbg gray-60 gray-80))
   `(tty-menu-selected-face ,(df-build-bg d-red))

   ;; flymake.el
   '(flymake-errline ((t (:inherit flyspell-incorrect))))
   '(flymake-warnline ((t (:inherit flyspell-duplicate))))

   ;; flyspell.el
   ;;
   ;; These are more complicated because we want to support the new :style
   ;; wave; we assume that if we have underline wave support, we're
   ;; truecolor. The underline colors are m-red and m-yellow, but the text
   ;; colors are d-red and d-yellow. (Sadly, copying the constants here is the
   ;; simplest thing to do. :(
   '(flyspell-incorrect
     ((((supports :underline (:style wave)))
        :underline (:style wave :color "#E6657EB77EB7"))
       (((class color) (min-colors 256)) (:foreground "#af5f5f"))
       (t :foreground "red")))
   '(flyspell-duplicate
     ((((supports :underline (:style wave)))
        :underline (:style wave :color "#EE13DE0182F1"))
       (((class color) (min-colors 256)) (:foreground "#d7af5f"))
       (t :foreground "orange")))

   ;; git-gutter.el
   `(git-gutter:separator ,(df-build-fg m-cyan ':weight 'bold))
   `(git-gutter:modified ,(df-build-fg m-violet ':weight 'bold))
   `(git-gutter:added ,(df-build-fg m-green ':weight 'bold))
   `(git-gutter:deleted ,(df-build-fg m-red ':weight 'bold))
   `(git-gutter:unchanged ,(df-build-bg l-yellow))

   ;; git-commit-mode.el
   '(git-commit-summary-face ((t (:inherit default))))
   `(git-commit-comment-heading-face ,(df-build-fg m-blue))
   `(git-commit-comment-file-face ,(df-build-fg gray-80))
   `(git-commit-branch-face ,(df-build-fg ssl-blue))
   `(git-commit-no-branch-face ,(df-build-fg l-red))

   ;; gn.el
   `(gn-embedded-variable ,(df-build-fg sl-orange))
   `(gn-embedded-variable-boundary ,(df-build-fg l-orange ':weight 'bold))

   ;; ido.el
   `(ido-first-match ,(df-build-fg m-yellow ':weight 'bold))
   `(ido-only-match ,dark-forest-m-green)
   `(ido-subdir ,dark-forest-l-blue)
   `(ido-indicator ,(df-build-fgbg m-yellow d-red ':width 'condensed))

   ;; isearch.el
   `(isearch ,(df-build-fgbg bg-hl-red l-red))
   `(isearch-fail ,(df-build-bg bg-hl-red))
   `(lazy-highlight ,(df-build-bg bg-hl-blue))

   ;; make-mode.el
   `(makefile-space ,(df-build-bg m-red))
   `(makefile-makepp-perl ,(df-build-bg d-blue))

   ;; powerline.el
   `(mode-line-buffer-id-inactive ,(df-build-fgbg gray-80 gray-30
                                                  ':weight 'bold
                                                  ':inherit 'mode-line-inactive))
   `(powerline-active1 ,(df-build-fgbg black gray-50 ':inherit 'mode-line))
   `(powerline-active2 ,(df-build-fgbg black gray-40 ':inherit 'mode-line))
   `(powerline-inactive1 ,(df-build-fgbg gray-80 gray-25
                                         ':inherit 'mode-line-inactive))
   `(powerline-inactive2 ,(df-build-fgbg gray-80 gray-20
                                         ':inherit 'mode-line-inactive))

   ;; replace.el    (but mainly used in occur.el)
   `(match ,(df-build-fgbg fg-white bg-hl-blue))

   ;; smerge-mode.el
   `(smerge-mine ,(df-build-bg bg-d-red))
   `(smerge-other ,(df-build-bg bg-d-green))
   `(smerge-base ,(df-build-fgbg bg bg-d-yellow))
   `(smerge-markers ((t (:inherit diff-function))))
   `(smerge-refined-removed ,(df-build-bg bg-hl-red))
   `(smerge-refined-added ,(df-build-bg bg-hl-green))

   ;; term.el
   `(term-color-black ,(df-build-fgbg bg bg))
   `(term-color-red ,(df-build-fgbg d-red d-red))
   `(term-color-green ,(df-build-fgbg d-green d-green))
   `(term-color-yellow ,(df-build-fgbg d-yellow d-yellow))
   `(term-color-blue ,(df-build-fgbg d-blue d-blue))
   `(term-color-magenta ,(df-build-fgbg d-violet d-violet))
   `(term-color-cyan ,(df-build-fgbg d-cyan d-cyan))
   `(term-color-white ,(df-build-fgbg fg-white fg-white))

   ;; wid-edit.el (Widget definitions)
   `(widget-documentation ,(df-build-fg l-green))
   `(widget-field ,(df-build-fgbg fg bg-2))
   `(widget-single-line-field ,(df-build-fgbg fg bg-2))
   `(widget-button-pressed ,(df-build-fg d-red))

   ;; yasnippet.el
   `(yas-field-highlight-face
     ,(df-build-bg bg-2 ':box '(:line-width -1 :color "grey40")))

   ;; ---- Below this line, things that I haven't validated ----

   ;; EMMS
   `(emms-stream-name ,(df-build-fg b-yellow ':weight 'normal))
   `(emms-stream-url-face ,dark-forest-sl-blue)
   `(emms-playlist-selected-face ,dark-forest-l-blue)
   `(emms-playlist-track-face ,dark-forest-m-yellow)

   ;; erc
   `(erc-button ,(df-build-fg m-cyan ':weight 'normal ':underline 't))
   `(erc-direct-msg-face ,(df-build-fg m-red))
   `(erc-header-line ,(df-build-fgbg fg-white bg-2))
   `(erc-input-face ,(df-build-fg fg-white ':weight 'normal))
   `(erc-prompt-face ,(df-build-fgbg bg m-blue))
   `(erc-notice-face ,(df-build-fg gray-45 ':weight 'normal))
   `(erc-my-nick-face ,dark-forest-l-red)
   `(erc-nick-default-face ,(df-build-fg sl-blue ':weight 'normal))
   `(erc-nick-msg-face ,(df-build-fg m-red))
   `(erc-inverse-face ,(df-build-fgbg bg fg))
   `(erc-error-face ,dark-forest-m-red)
   `(erc-current-nick-face ,dark-forest-l-red)
   `(erc-dangerouse-host-face ,(df-build-fg d-red))
   `(erc-pal-face ,(df-build-fg l-blue))
   `(erc-fool-face ,(df-build-fg gray))
   `(erc-keyword-face ,dark-forest-m-green)
   `(erc-timestamp-face ,dark-forest-m-green)

   ;; TODO: Figure out what to do about teal.
   `(fg:erc-color-face0 ,dark-forest-fg-white)
   `(fg:erc-color-face1 ((t (:foreground "black"))))
   `(fg:erc-color-face2 ,(df-build-fg d-blue))
   `(fg:erc-color-face3 ,(df-build-fg d-green))
   `(fg:erc-color-face4 ,(df-build-fg d-red))
   `(fg:erc-color-face5 ,(df-build-fg d-yellow))
   `(fg:erc-color-face6 ,(df-build-fg d-violet))
   `(fg:erc-color-face7 ,(df-build-fg d-orange))
   `(fg:erc-color-face8 ,dark-forest-m-yellow)
   `(fg:erc-color-face9 ,dark-forest-m-green)
   '(fg:erc-color-face10 ((t (:foreground "teal"))))
   `(fg:erc-color-face11 ,dark-forest-m-cyan)
   `(fg:erc-color-face12 ,dark-forest-m-blue)
   `(fg:erc-color-face13 ,dark-forest-m-violet)
   `(fg:erc-color-face14 ,dark-forest-dim-gray)
   `(fg:erc-color-face15 ,dark-forest-gray)
   `(bg:erc-color-face0 ,(df-build-bg fg-white))
   `(bg:erc-color-face1 ((t (:background "black"))))
   `(bg:erc-color-face2 ,(df-build-bg d-blue))
   `(bg:erc-color-face3 ,(df-build-bg d-green))
   `(bg:erc-color-face4 ,(df-build-bg d-red))
   `(bg:erc-color-face5 ,(df-build-bg d-yellow))
   `(bg:erc-color-face6 ,(df-build-bg d-violet))
   `(bg:erc-color-face7 ,(df-build-bg d-orange))
   `(bg:erc-color-face8 ,(df-build-bg m-yellow))
   `(bg:erc-color-face9 ,(df-build-bg m-green))
   '(bg:erc-color-face10 ((t (:background "teal"))))
   `(bg:erc-color-face11 ,(df-build-bg m-cyan))
   `(bg:erc-color-face12 ,(df-build-bg m-blue))
   `(bg:erc-color-face13 ,(df-build-bg m-violet))
   `(bg:erc-color-face14 ,(df-build-bg gray-45))
   `(bg:erc-color-face15 ,(df-build-bg gray))

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
   `(font-lock-warning-face
     ,(df-build-fg l-red ':weight 'bold ':inherit 'error))

   ;; info
   `(info-title-1 ,dark-forest-fg-white)
   `(info-title-2 ,dark-forest-gray)
   `(info-title-3 ,dark-forest-dim-gray)
   `(info-header-node ,dark-forest-fg-white)
   `(info-menu-header ,dark-forest-gray)
   `(info-menu-star ,dark-forest-m-red)

   ;; magit.el (Incomplete; revist after using more)
   `(magit-branch ,(df-build-fg sl-blue))
   `(magit-tag ,(df-build-fg b-yellow))
   `(magit-log-sha1 ,(df-build-fg l-red))
   `(magit-log-author ,(df-build-fg l-red))
   `(magit-cherry-unmatched ,(df-build-fg l-cyan))
   `(magit-cherry-equivalent ,(df-build-fg m-violet))
   `(magit-log-head-label-bisect-good ,(df-build-fg d-green))
   `(magit-log-head-label-bisect-skip ,(df-build-fg d-yellow))
   `(magit-log-head-label-bisect-bad ,(df-build-fg d-red))
   `(magit-log-head-label-remote ,(df-build-fgbg m-green bg-2))
   `(magit-log-head-label-tags ,(df-build-fgbg m-orange bg-2))
   `(magit-log-head-label-patches ,(df-build-fg m-red))
   `(magit-log-head-label-local ,(df-build-fgbg m-blue bg-2))
   `(magit-log-head-label-head ,(df-build-fgbg fg-white bg-2))
   `(magit-log-head-label-default ,(df-build-bg bg-2))
   `(magit-log-head-label-wip ,(df-build-fg sl-blue))
   `(magit-signature-good ,(df-build-fg m-green))
   `(magit-signature-bad ,(df-build-fg m-red))
   `(magit-signature-untrusted ,(df-build-fg m-cyan))
   ;; TODO: reflog
   `(magit-process-ok ,(df-build-fg m-green ':inherit 'magit-section-title))
   `(magit-process-ng ,(df-build-fg m-red ':inherit 'magit-section-title))

   ;; markdown.el
   `(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
   `(markdown-code-face ((t (:inherit font-lock-constant-face))))

   ;; org-mode (WOEFULLY INCOMPLETE ON THE COLORING!)
   `(org-date ,(df-build-fg l-cyan ':underline 't))
   `(org-special-keyword ,dark-forest-l-yellow)
   `(org-level-1 ,dark-forest-m-blue)
   `(org-level-2 ,dark-forest-m-cyan)
   `(org-level-3 ,dark-forest-m-green)
   `(org-level-4 ,dark-forest-m-yellow)
   `(org-level-5 ,dark-forest-m-orange)
   `(org-level-6 ,dark-forest-m-red)
   `(org-level-7 ,dark-forest-m-violet)
   `(org-level-8 ,dark-forest-l-red)
   `(org-todo ,(df-build-fg l-red ':weight 'bold))
   `(org-done ,(df-build-fg l-green ':weight 'bold))
   `(org-agenda-done ,(df-build-fg l-green ':weight 'bold))
   `(org-table ,(df-build-fg l-blue))
   `(org-code ,(df-build-fg gray-80))
))

;; TODO(erg): Figure out variables here.
(custom-theme-set-variables
 'dark-forest
 '(echo-bell-background "#BFFF4CCC4CCC")

 ;; Currently experimenting with this until it looks good. Currently, s=45,
 ;; v=93, and the hue is in the comment to the right.
 `(vc-annotate-color-map
   '((  0 . "#EE1382F182F1") ;; 0
     ( 10 . "#EE138E8C82F1") ;; 6.5
     ( 20 . "#EE139A2782F1") ;; 13.0
     ( 30 . "#EE13A5C282F1") ;; 19.5
     ( 40 . "#EE13B15D82F1") ;; 26.0
     ( 50 . "#EE13BCF982F1") ;; 32.5
     ( 60 . "#EE13C89482F1") ;; 39.0
     ( 70 . "#EE13D42F82F1") ;; 45.5
     ( 80 . "#EE13DFCA82F1") ;; 52.0
     ( 90 . "#EE13EB6582F1") ;; 58.5
     (100 . "#E526EE1382F1") ;; 65.0
     (110 . "#D98AEE1382F1") ;; 71.5
     (120 . "#CDEFEE1382F1") ;; 78.0
     (130 . "#C254EE1382F1") ;; 84.5
     (140 . "#B6B9EE1382F1") ;; 91.0
     (150 . "#AB1EEE1382F1") ;; 97.5
     (160 . "#9F82EE1382F1") ;; 104.0
     (170 . "#93E7EE1382F1") ;; 110.5
     (180 . "#884CEE1382F1") ;; 117.0
     (190 . "#82F1EE138931") ;; 123.5
     (200 . "#82F1EE1394CC") ;; 130.0
     (210 . "#82F1EE13A067") ;; 136.5
     (220 . "#82F1EE13AC02") ;; 143.0
     (230 . "#82F1EE13B79D") ;; 149.5
     (240 . "#82F1EE13C338") ;; 156.0
     (250 . "#82F1EE13CED4") ;; 162.5
     (260 . "#82F1EE13DA6F") ;; 169.0
     (270 . "#82F1EE13E60A") ;; 175.5
     (280 . "#82F1EA81EE13") ;; 182.0
     (290 . "#82F1DEE6EE13") ;; 188.5
     (300 . "#82F1D34AEE13") ;; 195.0
     (310 . "#82F1C7AFEE13") ;; 201.5
     (320 . "#82F1BC14EE13") ;; 208.0
     (330 . "#82F1B079EE13") ;; 214.5
     (340 . "#82F1A4DEEE13") ;; 221.0
     (350 . "#82F19942EE13") ;; 227.5
     (360 . "#82F18DA7EE13"))) ;; 234.0
 '(vc-annotate-background "#1a1a1a") ; bg
 )


(provide-theme 'dark-forest)
