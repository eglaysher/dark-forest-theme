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
      (fg-tuple       ["#EE13DE0182F1" "#ffd787" "brightwhite"])
      (fg-white-tuple ["#EE13ED01E6EF" "#eeeeee" "brightwhite"]) ; gray-93

      ;; "Black" background colors. Our "gray" has the slightest hint of amber
      ;; tint, but this is pure black/gray. These colors are slightly adjusted
      ;; on the xterm256 from the hsv values on the right.
      (bg-tuple   ["#1a1a1a" "#1c1c1c" "black"])        ; 0/0/10
      (bg-2-tuple ["#333333" "#303030" "black"])        ; 0/0/20
      (bg-3-tuple ["#4D4D4D" "#4e4e4e" "brightblack"])  ; 0/0/30

      ;; Our grays on truecolor have a subtle tint of amber in them. (hue=51,
      ;; sat=3) Otherwise, we fall back to the closest 256 safe gray.
      (gray-80-tuple   ["#CCCCCBE0C6A7" "#c6c6c6" "gray"])
      (gray-tuple      ["#B332B264ADD2" "#b2b2b2" "gray"])
      (gray-60-tuple   ["#999998E894FD" "#9e9e9e" "white"])
      (dim-gray-tuple  ["#733272AE6FBE" "#767676" "brightblack"])
      (gray-30-tuple   ["#4CCC4C744A7E" "#4e4e4e" "gray"])
      (gray-15-tuple   ["#2666263A253F" "#262626" "gray"])

      ;; The super-superlight colors. (hsv shift -25, -5)
      (ssl-blue-tuple  ["#B603CE34E665" "#afd7d7" "blue"])

      ;; The superlight colors. (hsv shift -15, 5)
      (sl-blue-tuple   ["#B0A3D851FFFF" "#afd7ff" "blue"])
      (sl-orange-tuple ["#FFFFBC1DA147" "#ffd7d7" "orange"])

      ;; The light set of colors. (hsv shift -5, 5)
      (l-red-tuple    ["#F33291EA91EA" "#ffafaf" "brightred"])
      (l-orange-tuple ["#FFFFA9C487AD" "#ffd7af" "brightyellow"])
      (l-yellow-tuple ["#FAE0EBD29686" "#ffffaf" "brightyellow"])
      (l-green-tuple  ["#A8F5FFFFA8F5" "#afffaf" "brightgreen"])
      (l-cyan-tuple   ["#6D70F332F332" "#5fffff" "brightcyan"])
      (l-blue-tuple   ["#9709CB84FFFF" "#87afff" "brightblue"])

      ;; "Medium" colors. (hsv shift 0, 0)
      (m-red-tuple    ["#E6657EB77EB7" "#ff8787" "brightred"])
      (m-orange-tuple ["#FFFFA0987AE0" "#ffaf87" "brightyellow"])
      (m-yellow-tuple ["#EE13DE0182F1" "#ffff87" "brightyellow"])
      (m-green-tuple  ["#9908FAE09908" "#87ff87" "brightgreen"])
      (m-cyan-tuple   ["#5C28E665E665" "#5fd7d7" "brightcyan"])
      (m-blue-tuple   ["#8353BB42F332" "#87afff" "brightblue"])
      (m-violet-tuple ["#C3D67EB7E665" "#d787d7" "birghtmagenta"])

      ;; Bolder colors. (hsv shift 20, 10)
      (b-yellow-tuple ["#FFFFE7095999" "#ffff5f" "yellow"])

      ;; "Dark" colors, which have saturation 55 and value of 80. Mainly used
      ;; for flyspell checking. Significantly darker than any of the
      ;; above. (hsv shift 15, -15)
      (d-red-tuple    ["#BFFF4CCC4CCC" "#af5f5f" "red"])
      (d-orange-tuple ["#D998711D47CE" "#d7875f" "orange"])
      (d-yellow-tuple ["#C7ADB5B44FDE" "#d7af5f" "yellow"])
      (d-green-tuple  ["#61BDD47A61BD" "#5fd75f" "green"])
      (d-cyan-tuple   ["#2FFFBFFFBFFF" "#00afaf" "cyan"])
      (d-blue-tuple   ["#4FDE8E55CCCC" "#5f87d7" "blue"])
      (d-violet-tuple ["#99994CCCBFFF" "#875faf" "violet"])

      ;; Temporary black tuple until I figure out what's black and what should
      ;; be bg.
      (black-tuple ["#000000" "#000000" "#000000"])

      ;; TODO: These are unjustifiable. Maybe a dd-red and a dd-blue?
      (bg-red-tuple ["#660000" "#5f0000" "red"])   ; 0/100/40
      (bg-blue-tuple ["#3D4F66" "#5f87af" "blue"]) ; 214/40/40

      ;; Final color lists. If at all possible, use these properties instead of
      ;; including raw colors per above.
      (dark-forest-fg-white (df-build-fg fg-white-tuple))

      (dark-forest-gray (df-build-fg gray-tuple))
      (dark-forest-dim-gray (df-build-fg dim-gray-tuple))

      (dark-forest-ssl-blue (df-build-fg ssl-blue-tuple))

      (dark-forest-sl-blue (df-build-fg sl-blue-tuple))
      (dark-forest-sl-orange (df-build-fg sl-orange-tuple))

      (dark-forest-l-red (df-build-fg l-red-tuple))
      (dark-forest-l-orange (df-build-fg l-orange-tuple))
      (dark-forest-l-yellow (df-build-fg l-yellow-tuple))
      (dark-forest-l-green (df-build-fg l-green-tuple))
      (dark-forest-l-cyan (df-build-fg l-cyan-tuple))
      (dark-forest-l-blue (df-build-fg l-blue-tuple))

      (dark-forest-m-red (df-build-fg m-red-tuple))
      (dark-forest-m-orange (df-build-fg m-orange-tuple))
      (dark-forest-m-yellow (df-build-fg m-yellow-tuple))
      (dark-forest-m-green (df-build-fg m-green-tuple))
      (dark-forest-m-cyan (df-build-fg m-cyan-tuple))
      (dark-forest-m-blue (df-build-fg m-blue-tuple))
      (dark-forest-m-violet (df-build-fg m-violet-tuple))
      )
  (custom-theme-set-faces
   'dark-forest

   ;; faces.el
   `(default ,(df-build-fgbg fg-tuple bg-tuple ':weight 'normal
                             ':width 'normal))
   `(cursor ,(df-build-fgbg black-tuple b-yellow-tuple))
   `(escape-glyph ,dark-forest-l-cyan)
   `(minibuffer-prompt ,dark-forest-l-cyan)
   '(highlight ((t (:weight bold :underline t))))
   `(region ,(df-build-bg bg-3-tuple))
   `(secondary-selection ,(df-build-bg bg-2-tuple))
   `(trailing-whitespace ,(df-build-bg m-red-tuple))

   '(button ((t (:inherit link))))
   `(link ,(df-build-fg m-cyan-tuple ':underline 't))
   `(link-visited ,(df-build-fg m-violet-tuple ':inherit 'link))
   `(fringe ,(df-build-bg gray-15-tuple))
   '(match ((t (:background "RoyalBlue3"))))
   `(menu ((((type tty)) (:background "black" :foreground "white"))))
   '(next-error ((t (:inherit region))))

   `(shadow ,dark-forest-gray)
   `(error ,(df-build-fg l-red-tuple ':weight 'bold ':inherit 'error))
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
   `(compilation-info ,(df-build-fg m-green-tuple ':weight 'normal))
   `(compilation-line-number ,dark-forest-sl-blue)
   `(compilation-column-number ,dark-forest-sl-orange)
   `(compilation-error ,dark-forest-m-red)
   `(compilation-warning ,(df-build-fg d-yellow-tuple))

   ;; Custom mode
   `(custom-invalid ,dark-forest-l-red)
   `(custom-group-tag ,dark-forest-m-blue)
   `(custom-state ,dark-forest-m-green)
   `(custom-variable-tag ,dark-forest-l-blue)
   `(custom-comment-tag ,dark-forest-gray)
   `(custom-button ,(df-build-bg gray-80-tuple))
   `(custom-button-mouse  ,(df-build-bg fg-white-tuple))
   ;; TODO(erg): Theoretically there are other faces in this group, but I can't
   ;; see them ever used. :-/

   ;; Diff mode
   `(diff-file-header ,(df-build-fgbg b-yellow-tuple bg-2-tuple
                                      ':weight 'normal))
   `(diff-header ,(df-build-fgbg fg-tuple bg-2-tuple))
   `(diff-added ,dark-forest-l-green)
   `(diff-removed ,dark-forest-l-red)

   ;; EMMS
   `(emms-stream-name ,(df-build-fg b-yellow-tuple ':weight 'normal))
   `(emms-stream-url-face ,dark-forest-sl-blue)
   `(emms-playlist-selected-face ,dark-forest-l-blue)
   `(emms-playlist-track-face ,dark-forest-m-yellow)

   ;; erc
   `(erc-button ,(df-build-fg m-cyan-tuple ':weight 'normal ':underline 't))
   `(erc-current-nick-face ,dark-forest-l-red)
   `(erc-error-face ,dark-forest-m-red)
   `(erc-input-face ,(df-build-fg fg-white-tuple ':weight 'normal))
   `(erc-keyword-face ,dark-forest-m-green)
   `(erc-nick-default-face ,(df-build-fg sl-blue-tuple ':weight 'normal))
   `(erc-notice-face ,(df-build-fg dim-gray-tuple ':weight 'normal))
   `(erc-prompt-face ,(df-build-fgbg black-tuple m-blue-tuple))
   `(erc-timestamp-face ,dark-forest-m-green)
   `(erc-my-nick-face ,dark-forest-l-red)

   ;; TODO: Figure out what to do about teal.
   `(fg:erc-color-face0 ,dark-forest-fg-white)
   `(fg:erc-color-face1 ((t (:foreground "black"))))
   `(fg:erc-color-face2 ,(df-build-fg d-blue-tuple))
   `(fg:erc-color-face3 ,(df-build-fg d-green-tuple))
   `(fg:erc-color-face4 ,(df-build-fg d-red-tuple))
   `(fg:erc-color-face5 ,(df-build-fg d-yellow-tuple))
   `(fg:erc-color-face6 ,(df-build-fg d-violet-tuple))
   `(fg:erc-color-face7 ,(df-build-fg d-orange-tuple))
   `(fg:erc-color-face8 ,dark-forest-m-yellow)
   `(fg:erc-color-face9 ,dark-forest-m-green)
   '(fg:erc-color-face10 ((t (:foreground "teal"))))
   `(fg:erc-color-face11 ,dark-forest-m-cyan)
   `(fg:erc-color-face12 ,dark-forest-m-blue)
   `(fg:erc-color-face13 ,dark-forest-m-violet)
   `(fg:erc-color-face14 ,dark-forest-dim-gray)
   `(fg:erc-color-face15 ,dark-forest-gray)
   `(bg:erc-color-face0 ,(df-build-bg fg-white-tuple))
   `(bg:erc-color-face1 ((t (:background "black"))))
   `(bg:erc-color-face2 ,(df-build-bg d-blue-tuple))
   `(bg:erc-color-face3 ,(df-build-bg d-green-tuple))
   `(bg:erc-color-face4 ,(df-build-bg d-red-tuple))
   `(bg:erc-color-face5 ,(df-build-bg d-yellow-tuple))
   `(bg:erc-color-face6 ,(df-build-bg d-violet-tuple))
   `(bg:erc-color-face7 ,(df-build-bg d-orange-tuple))
   `(bg:erc-color-face8 ,(df-build-bg m-yellow-tuple))
   `(bg:erc-color-face9 ,(df-build-bg m-green-tuple))
   '(bg:erc-color-face10 ((t (:background "teal"))))
   `(bg:erc-color-face11 ,(df-build-bg m-cyan-tuple))
   `(bg:erc-color-face12 ,(df-build-bg m-blue-tuple))
   `(bg:erc-color-face13 ,(df-build-bg m-violet-tuple))
   `(bg:erc-color-face14 ,(df-build-bg dim-gray-tuple))
   `(bg:erc-color-face15 ,(df-build-bg gray-tuple))

   ;; Flyspell colors
   `(flyspell-duplicate ,(df-build-fg d-yellow-tuple ':weight 'bold))
   `(flyspell-incorrect ,(df-build-fg d-red-tuple ':weight 'bold))

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
     ,(df-build-fg l-red-tuple ':weight 'bold ':inherit 'error))

   ;; help-argument: Just a slight lightening of arguments to make them stand
   ;; out just a bit, but not a :weight bold bit. Consider making this a sl
   ;; variant.
   `(help-argument-name ,dark-forest-l-yellow)

   ;; ido
   `(ido-first-match ,(df-build-fg m-yellow-tuple ':weight 'bold))
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
   `(isearch ,(df-build-fgbg bg-red-tuple l-red-tuple))
   `(isearch-fail ,(df-build-bg bg-red-tuple))
   `(lazy-highlight ,(df-build-bg bg-blue-tuple))
   '(query-replace ((t (:inherit isearch))))

   ;; makefile
   `(makefile-space ,(df-build-bg m-red-tuple))

   ;; modeline
   `(header-line ,(df-build-fgbg fg-white-tuple bg-2-tuple ':inherit 'mode-line
                                 ':box 'nil))
   ;; TODO: Make a custom tuple for the modeline.
   `(mode-line ,(df-build-fgbg black-tuple gray-60-tuple
                               ':box '(:line-width -1 :style released-button)))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
   `(mode-line-inactive ,(df-build-fgbg gray-80-tuple gray-30-tuple
                                        ':box '(:line-width -1 :color "grey40")
                                        ':weight 'light))

   ;; org-mode (WOEFULLY INCOMPLETE ON THE COLORING!)
   `(org-date ,(df-build-fg l-cyan-tuple ':underline 't))
   `(org-special-keyword ,dark-forest-l-yellow)
   `(org-level-1 ,dark-forest-m-blue)
   `(org-level-2 ,dark-forest-m-cyan)
   `(org-level-3 ,dark-forest-m-green)
   `(org-level-4 ,dark-forest-m-yellow)
   `(org-level-5 ,dark-forest-m-orange)
   `(org-level-6 ,dark-forest-m-red)
   `(org-level-7 ,dark-forest-m-violet)
   `(org-level-8 ,dark-forest-l-red)
   `(org-todo ,(df-build-fg l-red-tuple ':weight 'bold))
   `(org-done ,(df-build-fg l-green-tuple ':weight 'bold))
   `(org-agenda-done ,(df-build-fg l-green-tuple ':weight 'bold))
   `(org-table ,(df-build-fg l-blue-tuple))
   `(org-code ,(df-build-fg gray-80-tuple))
))

(provide-theme 'dark-forest)
