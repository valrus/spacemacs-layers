; SCCS I.D.  %W%  delta: %G% %U%  date: %H% %T%

;;;; tablature-mode.el -- modes for editing guitar tablature in GNU emacs

; Contains chord-mode and lead-mode, both minor modes of tab-mode.

; Author:  Mark R. Rubin <mark@phineas.jpl.nasa.gov>
; Version: 1.00
; Date:    9/20/93

; This code is released into the public domain without any express or implied
; warranty.  The author accepts no responsibility for any consequences
; arising from its use.

; This code may be distributed and modified in any way; however, please
; retain notice of original authorship in this and any derivative work.

; revision history:
; 1.00  9/20/93		Added 'xfretboard and related functions and
;			variables.  Removed optional args from 'chord-mode
;			and 'lead-mode.  Removed "tab-" prefix from
;			'tab-lead-mode and 'tab-chord-mode variables.
;			Added 'tab-delete-note, and fixed 'tab-delete-
;			chord-backward (wasn't handling non-tab delete,
;			and wipe out tuning).  Added 'tab-forward-barline
;			(and backward), and 'tab-up-staff (and down).
;			Added 'tab-set-tuning and 'tab-delete-current-note.
; 0.09	9/ 4/93		Added chord-spelling, and changed logic of 'tab-
;			analyze-chord and 'tab-analyze-chord-internal.  Added
;			'tab-12-tone-chords flag and function.  Fixed in/out
;			of tab handling of `+' key.
; 0.08	8/ 8/93		Broke 'tab-analyze-chord into two parts for
;			detection of X/Y chords.  Changed complicated defmacro
;			to defun due to speed/garbage-collection concerns.
;			Added 'tab-current-tuning, and changed
;			'tab-learn-tuning to set it.  Changed 'tab-copy-retune
;			and 'tab-analyze-chord to use 'tab-current-tuning.
;			Added 'tab-higher-string and 'tab-lower-string.  Added
;			'tab-move-string and 'tab-goto-string utilities.
;			Changed 'tab-label-chord to handle "X,noY/Z" chords.
;			Changed 'tab-pending-embellishement, 'tab-analyze-note,
;			and 'tab-analyze-fret to use 'nil rather than normal
;			data type value for flag.  Removed redundant "(progn
;			(let", etc. constructs.  Changed 'tab-label-chord
;			alignment of name over tab chord.
; 0.07	 8/ 7/93	Finished 'tab-label-chord.  Added more chords to
;			'tab-analyze-chord.
; 0.06   8/ 6/93	Added generic 'tab-begin-end-region, with safety
;			checks.  Changed 'tab-kill-internal to use it.
;			Changed 'tab-transpose to work on region.  Improved
;			tab-mode documentation.  Added 'tab-analyze-chord
;			and 'tab-label-chord.  Allowed 'tab-change-position
;			to use prefix arg.  Added 'tab-note-name.
;			Change 'tab-transpose-chord to use 'tab-analyze-fret.
; 0.05   8/ 2/93	Added 'tab-copy-retune.  Changed 'tab-transpose to use
;			'tab-transpose-chord.
; 0.04   8/ 1/93	Fixed 'tab-transpose in lead mode.  Added alternate
;			tunings.  Improved mode documentation.
; 0.03	 7/31/93	Removed 'tab-delete-note-backward ("\C-h") and
;			replaced with mode-dependent 'tab-delete-note.
; 0.02	 7/29/93	Added hard-coded VT-100 arrow-key bindings.  Added
;			"pending embellishment", indicated on mode line.
;			Added "X" embellishment.
; 0.01	 7/28/93	Original posting to rec.music.makers.guitar.tablature,
;			alt.guitar.tab,rec.music.makers.guitar,alt.guitar,
;			gnu.emacs.sources

(require 'cl)

; CUSTOMIZABLE DEFAULTS

(defvar tab-note-names
  ["E" "F" "F#" "G" "Ab" "A" "Bb" "B" "C" "C#" "D" "Eb"]
  "Names of notes (like A# vs. Bb) for 'tab-analyze-chord.  Change via
\\[tab-note-name] (tab-note-name)"
  )

(defvar tab-current-tuning ; must match tab-X-string-prefix, below
	[0 7 3 10 5 0]
"Numeric values of the six strings, high-to-low, in current tuning."
)

; must match tab-current-tuning, above
(defvar tab-0-string-prefix "e-|" "Unique beginning of string 0 line")
(defvar tab-1-string-prefix "B-|" "Unique beginning of string 1 line")
(defvar tab-2-string-prefix "G-|" "Unique beginning of string 2 line")
(defvar tab-3-string-prefix "D-|" "Unique beginning of string 3 line")
(defvar tab-4-string-prefix "A-|" "Unique beginning of string 4 line")
(defvar tab-5-string-prefix "E-|" "Unique beginning of string 5 line")

(defcustom tab-12-tone-chords
  t
  "*Spell chords in understandable, rational, 12-tone system in addition
to normal 1st, 3rd, 5th, b7th, etc.  Can take value `t' for true, or
`nil' for false."
  :type 'boolean)

(defcustom default-tablature-width
  80
  "Default width of a tablature line."
  :type '(choice (const :tag "Current window width" nil)
                 (integer :tag "Number of characters")))

; end of customizable defaults


(defvar tab-mode-map
	 nil
"Mode map for tab mode.
Commands:
\\{tab-mode-map}")

(defvar tab-saved-point
  nil
  "Saved point for moving between staff and lyrics.")

(defvar tab-current-string
	0
"What string cursor is on.")

(defvar tab-position
	0
"What fret index finger is on.")

(defvar tab-position-as-string
	"0"
"String variant of tab-position, for mode line")

(defvar tab-pending-embellishment
	nil
"Embellishment to be added to next entered note, or blank if none")

(defvar tab-killed-width
	""
"Width of last killed region")

(defvar tab-last-chord
	""
"Chord analyzed by `\\[tab-analyze-chord]' (tab-analyze-chord), available
for automatic insertion into tab by `\\[tab-label-chord]' (tab-label-chord)")

(defvar tab-string-regexp
  "^[a-gA-G][-b#]\|")

(defconst tab-font-lock-keywords-1
  `(((,tab-string-regexp . font-lock-constant-face)
     ("\|" . font-lock-constant-face)
     ("\\([0-9]+\\)-" . (1 font-lock-variable-name-face))
     ("\n\t\\(.*\\)" . (1 font-lock-comment-face))))
  "Highlighting for tab mode")

(defvar tab-syntax-highlights tab-font-lock-keywords-1)

(define-derived-mode tab-mode fundamental-mode "Tablature"
  "Major mode for entering tablature.  Always use minor modes lead-mode
or chord-mode instead.

In tab-mode, single keys represent notes on the guitar fretboard, and
pressing them creates tablature.  This only happens if the cursor is
in a tablature staff; otherwise the keys have their normal, text, meaning.
The keys are:

                   strings

             E   A    D   G   B   e

                1   2   3   4   5   6         N
                 q   w   e   r   t   y        N+1    frets
                  a   s   d   f   g   h       N+2
                   z   x   c   v   b   n      N+3

In chord-mode, the cursor remains in place (for entry of multiple-note
chords) until manually moved forward with SPACE or `\\[forward-char]'.  In
lead-mode, the cursor automatically advances after each note.

For more information on a key or action, do:
  `\\[describe-key]' and then enter the key(s) for the action
or
  `\\[describe-function]' and then enter the name of the action


  KEY	ACTION

  {	enter chord mode
  }	enter lead mode

  =	make a new tablature staff

  <	decrement base fret position by one (is printed on mode line)
  >	increment base fret position by one (is printed on mode line)
  ?	prompt for numeric entry of base fret position

  SPACE 	move one tab position forward
  \\[tab-forward-char]	move one tab position forward
  \\[tab-backward-char]	move one tab position backward
  \\[tab-forward-barline]	move forward one bar line
  \\[tab-backward-barline]	move back one bar line
  \\[tab-up-staff]	move up one staff
  \\[tab-down-staff]	move down one staff


  C-h	delete previous (lead-mode) or current (chord-mode) note
  C-?	delete previous note/chord
  \\[tab-delete-chord-forward]	delete current note/chord

  C-i	insert blank space

  |	toggle bar line

  [	mark current note as hammer-on
  ]	mark current note as pull-off
  ;	mark current note as bend
  '	mark current note as release
  /	mark current note as slide-up
  \	mark current note as slide-down
  ~	mark current note as vibrato
  (	mark current note as ghost note
  -	mark current note as normal note

  +	transpose notes in region by N frets (tab-transpose)

  \\[tab-copy-region-as-kill]	memorize tab between dot and mark (incl).
  \\[tab-kill-region]	as above, but also delete
  \\[tab-yank]	insert previously killed tablature

  \\[tab-copy-retune]	copy tab staff, transposing to current tuning
  \\[tab-learn-tuning] memorize new tuning (cursor first string)
  \\[tab-retune-string] return current string and learn new tuning

  \\[tab-analyze-chord]	analyze chord (cursor on root note)
  \\[tab-label-chord]	insert previously analyzed chord name
  \\[tab-note-name]	change whether chords are A# vs. Bb, etc.

  \\[tab-higher-string]	move note to next higher string
  \\[tab-lower-string] move note to next higher string

  \\[tab-up-12]	move note up 12 frets
  \\[tab-down-12]	move note down 12 frets

Tablature mode recognizes when the cursor is on a tab staff (and draws
new tab staffs) with six, three-character long, strings.  Each of the six
must be unique.  To change these strings (e.g. for alternate tunings),
enter them (while *not* in tab-mode) at the beginnings of six consecutive
lines, and use `\\[execute-extended-command] tab-learn-tuning'.


Full list of commands:
\\{tab-mode-map}"

  (if (not tab-mode-map) (tab-make-mode-map))
  (use-local-map tab-mode-map)

  (make-local-variable 'tab-current-string)
  (make-local-variable 'tab-position)
  (make-local-variable 'tab-position-as-string)
  (make-local-variable 'tab-pending-embellishment)
  (make-local-variable 'tab-killed-width)
  (make-local-variable 'tab-note-names)
  (make-local-variable 'tab-last-chord)
  (make-local-variable 'tab-current-tuning)
  (make-local-variable 'tab-12-tone-chords)
  (make-local-variable 'tab-0-string-prefix)
  (make-local-variable 'tab-1-string-prefix)
  (make-local-variable 'tab-2-string-prefix)
  (make-local-variable 'tab-3-string-prefix)
  (make-local-variable 'tab-4-string-prefix)
  (make-local-variable 'tab-5-string-prefix)

  (setq font-lock-defaults tab-syntax-highlights))


(define-minor-mode lead-mode
  "Turn on lead-mode, a minor mode of tab-mode.
Use `\\[describe-function] tab-mode' to see documentation for tab-mode."
  :lighter " Lead"
  (if (not (equal major-mode 'tab-mode))
      (tab-mode))

  (setq chord-mode nil)

  ;; No-op, but updates mode line.
  (set-buffer-modified-p (buffer-modified-p)))


(define-minor-mode chord-mode
  "Turn on chord-mode, a minor mode of tab-mode.
Use `\\[describe-function] tab-mode' to see documentation for tab-mode."
  :lighter " Chord"
  (if (not (equal major-mode 'tab-mode))
      (tab-mode))

  (setq lead-mode nil)

  ;; No-op, but updates mode line.
  (set-buffer-modified-p (buffer-modified-p)))


(defun tab-toggle-minor-mode ()
  (interactive)

  (if (not (equal major-mode 'tab-mode))
      (tab-mode))

  (if lead-mode (chord-mode) (lead-mode)))


(defun tab-12-tone-chords (arg)
  "Toggle 'tab-12-tone-chords flag, or set/clear according to optional argument.
Flag controls whether chord spelling also includes rational 12-tone version."
  (interactive "P")
  (setq tab-12-tone-chords (if (null arg) (not tab-12-tone-chords)
                             (> (prefix-numeric-value arg) 0))))


(defun rebind-keys (stock custom)
  "Rebind to second arg all keys currently bound to first arg."
  (let ((binding-list (where-is-internal stock)))
    (while binding-list
      (define-key tab-mode-map (car binding-list) custom)
      (setq binding-list (cdr binding-list)))))


(defvar tab-normal-mode-map-alist
  '(("{" . chord-mode)
    ("}" . lead-mode)

    ("=" . tab-make-staff)

    ("<" . tab-decrement-position)
    (">" . tab-increment-position)
    ("?" . tab-set-position)

    (" " . tab-forward)
    ("|" . tab-barline)

    ("\C-h" . tab-delete-note)
    ("\C-?" . tab-delete-chord-backward)

    ("\C-i" . tab-insert)

    ("+" . tab-transpose)
    ("8" . tab-analyze-chord)
    ("*" . tab-label-chord)

    ("9" . tab-higher-string)
    ("o" . tab-lower-string)
    ("0" . tab-up-12)
    ("p" . tab-down-12)

    ("[" . tab-hammer)
    ("]" . tab-pull)
    (";" . tab-bend)
    ("'" . tab-release)
    ("/" . tab-slide-up)
    ("\\" . tab-slide-down)
    ("~" . tab-vibrato)
    ("(" . tab-ghost)
    ("." . tab-muffled)
    ("-" . tab-normal)))


(defun tab-make-mode-map ()
  "Create tab mode map"

  ;; DEBUG ... hard-coded arrow-key bindings
  (global-unset-key	 "\M-[")
  ;; (global-unset-key	 "\M-O")

  (setq tab-mode-map (copy-keymap (current-global-map)))

  ;; DEBUG ... hard-coded arrow-key bindings
  (define-key tab-mode-map "\M-[A"	'previous-line)
  (define-key tab-mode-map "\M-[B"	'next-line)
  (define-key tab-mode-map "\M-[C"	'tab-forward-char)
  (define-key tab-mode-map "\M-[D"	'tab-backward-char)
  ;; (define-key tab-mode-map "\M-OA"	'previous-line)
  ;; (define-key tab-mode-map "\M-OB"	'next-line)
  ;; (define-key tab-mode-map "\M-OC"	'tab-forward-char)
  ;; (define-key tab-mode-map "\M-OD"	'tab-backward-char)

  ;; DEBUG ... doesn't work in 19.X in non-X mode
  ;; (define-key tab-mode-map [up]		'previous-line)
  ;; (define-key tab-mode-map [down]		'next-line)
  ;; (define-key tab-mode-map [right]	'tab-forward-char)
  ;; (define-key tab-mode-map [left]		'tab-backward-char)

  (let ((key-ndx 32))
    (while (< key-ndx 128)
      (progn
        (define-key tab-mode-map (char-to-string key-ndx) 'tab-unused-key)
        (setq key-ndx (1+ key-ndx)))))

  (loop for (key . action) in tab-normal-mode-map-alist
        do (define-key tab-mode-map key action))

  (rebind-keys 'delete-char 'tab-delete-chord-forward)

  (rebind-keys 'backward-char 'tab-backward-char)
  (rebind-keys 'forward-char  'tab-forward-char)
  (rebind-keys 'scroll-down   'tab-up-staff)
  (rebind-keys 'scroll-up     'tab-down-staff)

  (rebind-keys 'kill-region 'tab-kill-region)
  (rebind-keys 'copy-region-as-kill 'tab-copy-region-as-kill)
  (rebind-keys 'yank 'tab-yank)

  ;; Chord diagram style keybindings
  (define-key tab-mode-map "\M-1"	'tab-E-open)
  (define-key tab-mode-map "\M-2"	'tab-A-open)
  (define-key tab-mode-map "\M-3"	'tab-D-open)
  (define-key tab-mode-map "\M-4"	'tab-G-open)
  (define-key tab-mode-map "\M-5"	'tab-B-open)
  (define-key tab-mode-map "\M-6"	'tab-e-open)
  (define-key tab-mode-map "!"	'tab-E-1)
  (define-key tab-mode-map "@"	'tab-A-1)
  (define-key tab-mode-map "#"	'tab-D-1)
  (define-key tab-mode-map "$"	'tab-G-1)
  (define-key tab-mode-map "%"	'tab-B-1)
  (define-key tab-mode-map "^"	'tab-e-1)
  (define-key tab-mode-map "1"	'tab-E0)
  (define-key tab-mode-map "2"	'tab-A0)
  (define-key tab-mode-map "3"	'tab-D0)
  (define-key tab-mode-map "4"	'tab-G0)
  (define-key tab-mode-map "5"	'tab-B0)
  (define-key tab-mode-map "6"	'tab-e0)
  (define-key tab-mode-map "q"	'tab-E1)
  (define-key tab-mode-map "w"	'tab-A1)
  (define-key tab-mode-map "e"	'tab-D1)
  (define-key tab-mode-map "r"	'tab-G1)
  (define-key tab-mode-map "t"	'tab-B1)
  (define-key tab-mode-map "y"	'tab-e1)
  (define-key tab-mode-map "a"	'tab-E2)
  (define-key tab-mode-map "s"	'tab-A2)
  (define-key tab-mode-map "d"	'tab-D2)
  (define-key tab-mode-map "f"	'tab-G2)
  (define-key tab-mode-map "g"	'tab-B2)
  (define-key tab-mode-map "h"	'tab-e2)
  (define-key tab-mode-map "z"	'tab-E3)
  (define-key tab-mode-map "x"	'tab-A3)
  (define-key tab-mode-map "c"	'tab-D3)
  (define-key tab-mode-map "v"	'tab-G3)
  (define-key tab-mode-map "b"	'tab-B3)
  (define-key tab-mode-map "n"	'tab-e3)
  (define-key tab-mode-map "Z"	'tab-E4)
  (define-key tab-mode-map "X"	'tab-A4)
  (define-key tab-mode-map "C"	'tab-D4)
  (define-key tab-mode-map "V"	'tab-G4)
  (define-key tab-mode-map "B"	'tab-B4)
  (define-key tab-mode-map "N"	'tab-e4))


(defun tab-check-in-tab ()
  "Return t/nil whether cursor is in a tab staff line.  Also, force cursor
to nearest modulo 3 note position. Set global variable tab-current-string."
  (let ((in-tab t)
        (strings-above 0)
        (real-case-fold-search case-fold-search))

    (save-excursion
      (beginning-of-line)

      (setq case-fold-search nil)

      ;; see how many tab strings are above this one (inclusive)
      (while (looking-at tab-string-regexp)
        (setq strings-above (1+ strings-above))
        (forward-line -1))

      (if (> strings-above 0)
          (setq tab-current-string (1- strings-above))
        (setq tab-current-string 0
              in-tab nil)))

    (setq case-fold-search real-case-fold-search)

    (let ((alignment (% (1+ (current-column)) 3)))
      ;; put cursor on note position
      (if in-tab
          (cond
           ((< (current-column) 5) (forward-char (- 5 (current-column))))
           ((/= alignment 0) (backward-char alignment)))))

    (setq in-tab in-tab)))


(defun tab-decrement-position ()
  (interactive)
  (if (tab-check-in-tab)
      (if (> tab-position 0) (setq tab-position (1- tab-position)))
    (insert (this-command-keys)))
  (setq tab-position-as-string (int-to-string tab-position))
  ;; No-op, but updates mode line.
  (set-buffer-modified-p (buffer-modified-p)))


(defun tab-increment-position ()
  (interactive)
  (if (tab-check-in-tab)
      (setq tab-position (1+ tab-position))
    (insert (this-command-keys)))
  (setq tab-position-as-string (int-to-string tab-position))
  ;; No-op, but updates mode line.
  (set-buffer-modified-p (buffer-modified-p)))


(defun tab-set-position (fret)
  "Prompt for numeric entry of current fret position"
  (interactive "P")
  (if (tab-check-in-tab)
      (progn
        (if fret
            (setq tab-position fret)
          ;; else
          (setq fret (read-string "Fret: "))
          (setq tab-position (string-to-int fret)))

        (if (< tab-position 0) (setq tab-position 0))
        (setq tab-position-as-string (int-to-string tab-position))
        (set-buffer-modified-p (buffer-modified-p)))
    ;; else
    (insert (this-command-keys))))


(defun tab-forward-char (count)
  (interactive "p")
  (let ((original-column (current-column)))

    (if (tab-check-in-tab)
        (progn
          (if (< original-column 5) (backward-char 3))
          (forward-char (* count 3)))
      ;; else
      (forward-char count))))


(defun tab-backward-char (count)
  (interactive "p")
  (if (tab-check-in-tab) (setq count (* count 3)))
  (backward-char count))


(defun tab-forward-barline ()
(interactive)
	(if (tab-check-in-tab) (progn
		(if (looking-at "|") (forward-char 1))
	(re-search-forward "|\\|$")
	(tab-check-in-tab))))


(defun tab-backward-barline ()
(interactive)
	(if (tab-check-in-tab) (progn
	(re-search-backward "|\\|^")
	(tab-check-in-tab))))


(defun choose-re-search (count)
  (if (> count 0) 're-search-forward 're-search-backward))


(defun tab-navigate-string (count)
  (let ((column (current-column))
        (real-case-fold-search case-fold-search)
        (search-fun (choose-re-search count))
        (loop-count (abs count)))

    (setq case-fold-search nil)
    (while
        (> loop-count 0)
      (progn
        (funcall search-fun tab-string-regexp nil t)
        (setq loop-count (1- loop-count))))
    (beginning-of-line)
    (forward-char column)
    (tab-check-in-tab)
    (setq case-fold-search real-case-fold-search)))


(defun tab-up-string (count)
  (interactive "p")
  (tab-navigate-string (- (1+ count))))


(defun tab-down-string (count)
  (interactive "p")
  (tab-navigate-string count))


(defun tab-restore-staff-location (string column)
  (when (tab-check-in-tab)
    (move-to-column column)
    (next-line string)
    (tab-check-in-tab)))


(defun tab-integer-sign (n)
  (if (= n 0)
      0
    (if (> n 0)
        1
      -1)))


(defun tab-move-beyond-staff (direction)
  "Move to the line above or below the current staff, depending on the sign
of direction. Return nil if there is no such line (or we're not in tab),
t otherwise."
  (if (not (tab-check-in-tab))
      nil
    (if (< direction 0)
        (forward-line (- (1+ tab-current-string)))
      (forward-line (- 6 tab-current-string)))
    (not (tab-check-in-tab))))


(defun tab-move-staff-start ()
  "Move to the beginning of a tab staff. Does nothing if not in tab."
  (when (tab-check-in-tab)
    (next-line (- tab-current-string))))


(defun tab-move-staff (direction)
  "Move one staff up or down depending on sign of direction. Leave the cursor on the first character of the first line of the staff moved to."
  (let ((tmp-saved-point (copy-marker (point)))
        (can-move t)
        (search-fun (choose-re-search direction)))
    (when (tab-check-in-tab)
      ;; go to the line after this staff, if possible
      (when (not (tab-move-beyond-staff direction))
        ;; no lines beyond this staff, bail
        (setq can-move nil)
        (goto-char tmp-saved-point)))
    ;; go to the "next" (up or down depending on direction) tab line
    (when can-move
      (if (funcall search-fun tab-string-regexp nil t)
          (tab-move-staff-start)
        (goto-char tmp-saved-point)
        (setq can-move nil)))
    can-move))


(defun tab-up-staff (count)
  (interactive "p")
  (let ((starting-column (current-column))
        (starting-string tab-current-string))

    (cl-loop repeat count
             do (tab-move-staff -1))

    (tab-restore-staff-location starting-string starting-column)))


(defun tab-down-staff (count)
  (interactive "p")
  (let ((starting-column (current-column))
        (starting-string tab-current-string))

    (cl-loop repeat count
             do (tab-move-staff 1))

    (tab-restore-staff-location starting-string starting-column)))


(defun new-tab-line-width ()
  (or default-tablature-width (window-body-width)))


(defun tab-toggle-lyric-line ()
  (interactive)
  (if (tab-check-in-tab)
      (progn
        (setq tab-saved-point (copy-marker (point)))
        (let ((starting-string tab-current-string))
          (if (tab-move-beyond-staff 1)
              (end-of-line)
            (newline))))
    (goto-char tab-saved-point)))


(defun tab-make-staff ()
  "Make a tab staff.  Do below current staff if in staff, or three lines below
cursor if not already in staff."
  (interactive)

  (let ((starting-string tab-current-string)
        (starting-column (current-column)))

    (save-excursion
      ;; if we're not in a staff, try to move to the closest previous one
      (if (not (tab-check-in-tab))
          (tab-move-staff -1))

      (if (tab-check-in-tab)
          (let ((newline-count (if (tab-move-beyond-staff 1) 4 5)))
            (end-of-line)
            (newline (forward-line newline-count))
            ;; lyric line
            (insert "   ")
            (forward-line -1))
        ;; there is no previous staff, make a lyric line and then move above it
        (newline 2)
        (forward-line -1)
        (beginning-of-line))

      (insert tab-0-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline)
      (insert tab-1-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline)
      (insert tab-2-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline)
      (insert tab-3-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline)
      (insert tab-4-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline)
      (insert tab-5-string-prefix) (insert-char ?- (- (new-tab-line-width) 5)) (newline))

    (tab-move-staff 1)
    (tab-restore-staff-location starting-string starting-column)))


(defun toggle-barline (advance)
  "Toggle barline at point on staff. If ARG is true, advance point too."
  (let ((linecount 6)
        (starting-string tab-current-string)
        (barline-string "--|"))

    (backward-char 2)
    (setq temporary-goal-column (current-column))
    (previous-line tab-current-string)

    (while (> linecount 0)
      (insert (if (looking-at barline-string) "---" barline-string))
      (delete-char 3)
      (backward-char 3)
      (if (> linecount 1) (next-line 1))
      (setq linecount (1- linecount)))

    (next-line (- starting-string 5))

    (if (and advance (< (current-column) (- (line-end-position) 5)))
        (forward-char 5)
      (forward-char 2))))


(defun tab-barline-in-place ()
  "Draw a barline down staff"
  (interactive)
	(if (tab-check-in-tab)
      (toggle-barline nil)
    (insert (this-command-keys))))


(defun tab-barline ()
  "Draw a barline down staff"
  (interactive)
  (if (tab-check-in-tab)
      (toggle-barline t)
    (insert (this-command-keys))))


(defun tab-forward ()
  "Move cursor forward one tablature space"
  (interactive)
  (let ((original-column (current-column)))

    (if (tab-check-in-tab)
        (progn
          (if (< original-column 5) (backward-char 3))
          (forward-char 3))
      ;; else
      (insert (this-command-keys)))))


(defun tab-delete ()
  "Delete vertical `chord' of notes"
  (let ((index 0) (placemark))
    (setq temporary-goal-column (current-column))
    (previous-line tab-current-string)
    (backward-char 2)
    (while (< index 6)
      (delete-char 3)
      (setq placemark (point-marker))
      (end-of-line)
      (insert "---")
      (goto-char placemark)
      (setq temporary-goal-column (current-column))
      (if (< index 5) (next-line 1))
      (setq index (1+ index)))

    (if (/= tab-current-string 5) (next-line (- tab-current-string 5)))
    (forward-char 2)))


(defun tab-delete-chord-forward (count)
  "Delete vertical `chord' of notes at cursor position"
  (interactive "p")
  (if (<= count 0) (setq count 1))

  (if (tab-check-in-tab)
      (while (> count 0)
        (progn
          (tab-delete)
          (setq count (1- count))))
    ;; else
    (delete-char count)))


(defun tab-delete-chord-backward (count)
  "Delete vertical `chord' of notes to left of cursor position"
  (interactive "p")
  (if (<= count 0) (setq count 1))

  (if (tab-check-in-tab)
      (while (and (> count 0) (> (current-column) 5))
        (progn
          (backward-char 3)
          (tab-delete)
          (setq count (1- count))))
    ;; else
    (delete-backward-char count)))


(defun tab-delete-note (count)
  "Delete previous note on current string (lead-mode)
or current note (chord-mode)."
  (interactive "p")

  (if (tab-check-in-tab)
      (progn
        (if (and (bound-and-true-p lead-mode) (> (current-column) 5))
            (progn
              (backward-char 2)
              (delete-backward-char 3)
              (insert "---")
              (backward-char 1)))

        (if (bound-and-true-p chord-mode) (tab-delete-current-note)))
    ;; else
    (delete-backward-char count)))


(defun tab-delete-current-note ()
  "Delete note (if any) that cursor is on, regardless of minor mode"
  (interactive)

  (when (tab-check-in-tab)
    (forward-char 1)
    (delete-backward-char 3)
    (insert "---")
    (backward-char 1)))


(defun tab-insert (count)
  "Insert blank tablature space at cursor position"
  (interactive "p")

  (if (tab-check-in-tab)
      (let ((index 0)
            (placemark))
        (setq temporary-goal-column (current-column))
        (previous-line tab-current-string)
        (backward-char 2)
        (while (< index 6)
          (setq placemark (point-marker))
          (insert-char ?- (* count 3))
          (end-of-line)
          (delete-backward-char (* count 3))
          (goto-char placemark)
          (setq temporary-goal-column (current-column))
          (if (< index 5) (next-line 1))
          (setq index (1+ index)))
        (next-line (- tab-current-string 5))
        (forward-char 2))
    ;; else
    (insert (this-command-keys))))


(defun tab-begin-end-region (caller-begin caller-end)
  "Set CALLER-BEGIN/CALLER-END to leftmost/rightmost of top tab line above
dot or mark.  Return t if dot was left of mark, nil otherwise.
Check that both dot and mark are inside same staff of tab."

  (let ((placemark (point-marker))
        (local-begin)
        (local-end)
        (begin-col)
        (end-col)
        (dot-before-mark))

    ;; figure beginning and end
    (setq begin-col (current-column))
    (goto-char (mark-marker))
    (setq end-col (current-column))

    (if (< begin-col end-col)
        (progn
          (setq local-begin placemark)
          (setq local-end (mark-marker))
          (setq dot-before-mark t))
      ;; else
      (setq local-begin (mark-marker))
      (setq local-end placemark)
      (setq dot-before-mark nil))

    ;; set beginning to top staff line
    (goto-char local-begin)
    (unless (tab-check-in-tab)
      (goto-char placemark)
      (error "Mark not in tablature"))
    (setq temporary-goal-column (current-column))
    (previous-line tab-current-string)
    (setq local-begin (point-marker))

    ;; set end to top staff line
    (goto-char local-end)
    (unless (tab-check-in-tab)
      (goto-char placemark)
      (error "Mark not in tablature"))
    (setq temporary-goal-column (current-column))
    (previous-line tab-current-string)
    (setq local-end (point-marker))

    ;; check begin and end in same tab staff
    (goto-char local-begin)
    (if (< local-end local-begin)
        (progn
          (goto-char placemark)
          (error "Dot and mark must be in same tab staff")))
    (re-search-forward "$" local-end 2 1)
    (if (/= local-end (point-marker))
        (progn
          (goto-char placemark)
          (error "Dot and mark must be in same tab staff")))

    ;; return values
    (set caller-begin local-begin)
    (set caller-end local-end)
    (setq dot-before-mark dot-before-mark)))


(defun tab-kill-internal (delete)
  "Delete region of tab, putting in rectangle-kill ring if ARG is t"
  (let ((placemark (point-marker))
        (begin) (end)
        (begin-col) (end-col)
        (index 0)
        (dot-before-mark)
        (original-string tab-current-string))

    ;; figure rectangle beginning and end (inclusive these notes)
    (setq dot-before-mark (tab-begin-end-region 'begin 'end))
    (goto-char begin)
    (backward-char 2)
    (setq begin (point-marker))
    (setq begin-col (current-column))
    (goto-char end)
    (setq temporary-goal-column (current-column))
    (next-line 5)
    (forward-char 1)
    (setq end (point-marker))
    (setq end-col (current-column))

    ;; do it
    (setq tab-killed-width (- end-col begin-col))
    (kill-rectangle begin end)
    (goto-char begin)

    (if delete
        (progn
          ;; extend staff
          (while (< index 6)
            (end-of-line)
            (insert-char ?- tab-killed-width)
            (forward-line)
            (setq index (1+ index)))
          (goto-char begin)
          (forward-char 2)
          (setq tab-current-string 0))
      ;; else
      (yank-rectangle)
      (setq tab-current-string original-string)
      (if dot-before-mark
          (progn
            (goto-char begin)
            (forward-char 2)
            (setq temporary-goal-column (current-column))
            (if (/= tab-current-string 0)
                (next-line tab-current-string)))
        ;; else
        (backward-char 1)
        (setq temporary-goal-column (current-column))
        (previous-line (- 5 tab-current-string))))))


(defun tab-kill-region ()
  "Kill region of tab to rectangle kill ring"
  (interactive)
  (if (tab-check-in-tab)
      (tab-kill-internal t)
    (kill-region (point-marker) (mark-marker))))


(defun tab-copy-region-as-kill ()
  "Copy region of tab to rectangle kill ring"
  (interactive)
  (if (tab-check-in-tab)
      (tab-kill-internal nil)
    (copy-region-as-kill (point-marker) (mark-marker))))


(defun tab-yank ()
  "Insert region of tab from rectangle kill ring"
  (interactive)
  (if (tab-check-in-tab)
      (let ((placemark (point-marker)) (top-line) (index 0))
        (setq temporary-goal-column (current-column))
        (previous-line tab-current-string)
        (backward-char 2)
        (setq top-line (point-marker))
        (while (< index 6)
          (end-of-line)
          (delete-backward-char tab-killed-width)
          (forward-line)
          (setq index (1+ index)))
        (goto-char top-line)
        (yank-rectangle)
        (goto-char placemark))
    (yank)))


(defun tab-transpose (frets)
  "Transpose notes in region up or down by numeric prefix or prompted-for frets"
  (interactive "P")

  (if (tab-check-in-tab)
      (let ((input-string)
            (fret-array [0 0 0 0 0 0])
            (begin)
            (end))

        (unless frets
          (setq input-string
                (read-string "Transpose region by N frets: "))
          (setq frets (string-to-int input-string)))

        (fillarray fret-array frets)
        (message "Transposing region by %d frets ..." frets)

        (tab-begin-end-region 'begin 'end)
        (goto-char begin)

        (while (<= (point-marker) end)
          (progn (tab-transpose-chord fret-array)
                 (if (< (current-column) (- (line-end-position) 3))
                     (forward-char 3)
                   (end-of-line))))

        (setq tab-current-string 0)
        (message "Finished transposing region by %d frets." frets))
    (insert (this-command-keys))))


(defun tab-copy-retune ()
  "If cursor is on top line of tab staff, will copy staff and change into
current tuning."
  (interactive)
  (let ((old-cursor)
        (new-cursor)
        (old-tuning	[0 0 0 0 0 0])
        (diff)
        (tuning-diff	[0 0 0 0 0 0])
        (ndx)
        (placemark))

    (message "Copying this staff and converting to current tuning ...")

    ;; make new staff
    (setq old-cursor (point-marker))
    (forward-line 6)
    ;; find blank line, or end of file
    (while (not (looking-at "^$")) (forward-line 1))
    (newline 1)
    (tab-make-staff)
    (beginning-of-line)
    (setq new-cursor (point-marker))

    ;; learn tunings
    (goto-char old-cursor)
    (tab-analyze-tuning old-tuning)
    (goto-char new-cursor)
    (setq ndx 0)
    (while (< ndx 6)
      (progn
        (setq diff (- (aref old-tuning ndx) (aref tab-current-tuning ndx)))
        (if (> diff  6) (setq diff (- diff 12)))
        (if (< diff -6) (setq diff (+ diff 12)))
        (aset tuning-diff ndx diff)
        (setq ndx (1+ ndx))))

    ;; copy old staff to new
    ;; delete new staff past tuning signature
    (goto-char new-cursor)
    (forward-char 3)
    (setq new-cursor (point-marker))
    (forward-line 5)
    (end-of-line)
    (kill-rectangle new-cursor (point-marker))
    (goto-char new-cursor)

    ;; memorize old staff past tuning signature
    (goto-char old-cursor)
    (forward-char 3)
    (setq old-cursor (point-marker))
    (forward-line 5)
    (end-of-line)
    (kill-rectangle old-cursor (point-marker))
    (goto-char old-cursor)
    (yank-rectangle)

    ;; copy
    (goto-char new-cursor)
    (yank-rectangle)
    (goto-char new-cursor)
    (forward-char 2)

    ;; change tuning
    (while (< (current-column) (- (line-end-position) 2))
      (progn
        (tab-transpose-chord tuning-diff)
        (if (< (current-column) (- (line-end-position) 3))
            (forward-char 3)
          (end-of-line))))

    (message "Finished copying into current tuning.")))


(defun tab-analyze-tuning (tuning)
  "Fill six-element array TUNING with numeric values representing letter
notes at beginning of current plus next 5 screen lines."
  (when (tab-check-in-tab)
    (save-excursion
      (tab-move-staff-start)
      (let ((ndx 0)
            (numeric))

        (while (< ndx 6)
          (progn
            (beginning-of-line)
            (cond
             ((looking-at "[Ee]") (setq numeric  0))
             ((looking-at "[Ff]") (setq numeric  1))
             ((looking-at "[Gg]") (setq numeric  3))
             ((looking-at "[Aa]") (setq numeric  5))
             ((looking-at "[Bb]") (setq numeric  7))
             ((looking-at "[Cc]") (setq numeric  8))
             ((looking-at "[Dd]") (setq numeric 10))
             (t		   (setq numeric  0)))
            (forward-char 1)
            (if (looking-at "#") (setq numeric (1+ numeric)))
            (if (looking-at "b") (setq numeric (1- numeric)))

            (if (< numeric 0) (setq numeric (+ 12 numeric)))
            (aset tuning ndx numeric)

            (forward-line 1)
            (setq ndx (1+ ndx))))))))


(defun tab-transpose-chord (transpositions)
  "Transpose chord at cursor by 6-element array of fret offsets."
  (let ((note))

    (setq tab-current-string 0)
    (while (< tab-current-string 6)
      (progn (setq note (tab-analyze-fret))
             (when note
               (setq note (+ note (aref transpositions tab-current-string)))
               (if (< note 0) (setq note (+ 12 note)))
               (tab-string (int-to-string note) tab-current-string)
               (if (bound-and-true-p lead-mode) (backward-char 3)))

             (setq temporary-goal-column (current-column))
             (if (< tab-current-string 5)
                 (next-line 1)
               (previous-line 5))
             (setq tab-current-string (1+ tab-current-string))))))


(defun tab-get-string-prefix-symbol (index)
  (cond
   ((= index 0) 'tab-0-string-prefix)
   ((= index 1) 'tab-1-string-prefix)
   ((= index 2) 'tab-2-string-prefix)
   ((= index 3) 'tab-3-string-prefix)
   ((= index 4) 'tab-4-string-prefix)
   ((= index 5) 'tab-5-string-prefix)))


(defun tab-get-string-prefix (index)
  (symbol-value (tab-get-string-prefix-symbol index)))


(defun tab-learn-tuning ()
  "Memorize 3-character beginning of current plus next 5 screen lines as
new tuning.  Each line must be unique."
  (interactive)

  (tab-analyze-tuning tab-current-tuning)

  (cl-loop for index from 0 to 5
           do (tab-learn-string (tab-get-string-prefix index)))
  (forward-line -6))


(defun tab-relabel-string (new-tuning)
  (save-excursion
    (beginning-of-line)
    (delete-char 2)
    (insert (if (= (length new-tuning) 1) (concat new-tuning "-") new-tuning))))


(defun tab-relabel-all-strings (string new-tuning)
  (save-excursion
    ;; Go to the beginning of the first staff
    (beginning-of-buffer)
    (tab-move-staff 1)
    (cl-loop do (progn
                  ;; Note: 4 is just to put us in the staff
                  (tab-restore-staff-location string 4)
                  (tab-relabel-string new-tuning))
             while (tab-move-staff 1))))


(defun tab-retune-string ()
  (interactive)

  (when (tab-check-in-tab)
    (let ((new-tuning (read-string "New tuning for string: ")))
      (unless (string-match-p "^[aAbBcCdDeEfFgG][#b]?$" new-tuning)
        (error "New tuning isn't a valid note name!"))
      (tab-relabel-all-strings tab-current-string new-tuning)
      (when (tab-check-in-tab)
        (tab-learn-string (tab-get-string-prefix-symbol tab-current-string))
        (tab-analyze-tuning tab-current-tuning)))))


(defun tab-learn-string (string)
  "Copy first three characters of line into STRING."
  (save-excursion
    (let ((begin))
      (beginning-of-line)
      (setq begin (point-marker))
      (forward-char 3)
      (set string (buffer-substring begin (point-marker)))
      (forward-line 1))))


(defun tab-note-name ()
  "Change names for printing chords (e.g. A# vs. Bb). First enter current name
of note, then new name."
  (interactive)

  (let ((old)
        (new)
        (ndx 0)
        (searching t))

    (setq old (read-string (format "Old note (one of %s): " tab-note-names)))
    (while (and searching (< ndx 12))
      (progn (if (string= old (aref tab-note-names ndx))
                 (setq searching nil))
             (setq ndx (1+ ndx))))

    (if searching
        (error "Must enter one of %s" tab-note-names)
      (setq ndx (1- ndx)))

    (setq new (read-string (format "New note name for %s: "
                                   (aref tab-note-names ndx))))
    (aset tab-note-names ndx new)))


(defun tab-goto-chord-label ()
  (interactive)

  ;; go to appropriate column, and to line above tab
  (let ((chord-column)
        (name-begin))

    (backward-char 1)
    (setq chord-column (current-column))
    (setq temporary-goal-column chord-column)
    (previous-line (1+ tab-current-string))

    ;; insert spaces if necessary
    (when (< (current-column) chord-column)
      (indent-to-column chord-column)
      (setq name-begin (point-marker))
      (beginning-of-line)
      (untabify (point-marker) name-begin)
      (move-to-column chord-column))))


(defun tab-delete-chord-label ()
  (interactive)

  (save-excursion
    (tab-goto-chord-label)
      ;; delete previous chord (replace with spaces)
      (save-excursion
        (while (looking-at "\\S-") (progn (delete-char 1) (insert " "))))))


(defun tab-label-chord ()
  "Insert previously analyzed chord above current tab staff.  Can only be
used immediately after `\\[tab-analyze-chord]' (tab-analyze-chord)"
  (interactive)

  (save-excursion
    (let ((name-width (length tab-last-chord))
          (chord-column)
          (name-begin)
          (name-end))

      (unless (equal last-command 'tab-analyze-chord)
        (error "Use only immediately after `%s' (tab-analyze-chord)"
               (car (where-is-internal 'tab-analyze-chord tab-mode-map))))

      (tab-delete-chord-label)

      ;; insert chord name
      (tab-goto-chord-label)
      (insert tab-last-chord)

      ;; remove spaces equal to inserted name
      (while (and (> name-width 0) (looking-at " " ))
        (progn
          (delete-char 1)
          (setq name-width (1- name-width)))))))



(defun tab-analyze-chord ()
  "Analyze chord.  Note cursor is on is assumed to be root.  Repeat usage
moves root to next chord note.  Use `\\[tab-label-chord]' (tab-label-chord)
immediately afterwards to insert chord into tab."

  (interactive)

  (if (tab-check-in-tab)
      (let ((root-note-marker)
            (root-string)
            (root)
            (note)
            (bass-note)
            (bass-note-name)
            (bass-note-pos)
            (chord [12 12 12 12 12 12]) ; "no note"
            (chord-notes [0 0 0 0 0 0 0 0 0 0 0 0])
            (root-name)
            (chord-name)
            (chord-disclaimer)
            (chord-spelling)
            (number-of-notes 0))

        (fillarray chord 12) ; "no note"
        (fillarray chord-notes 0)

                                        ; get root
        (if (or (equal last-command this-command)
                (not (looking-at "[0-9]")))
            (tab-next-chord-note))
        (setq root-note-marker (point-marker))
        (setq root-string tab-current-string)
        (setq root (tab-analyze-note))
        (setq root-name (aref tab-note-names root))

                                        ; get chord notes
        (setq temporary-goal-column (current-column))
        (previous-line tab-current-string)
        (setq tab-current-string 0)
        (while (< tab-current-string 6)
          (progn (setq note (tab-analyze-note))
                 (when note
                   (setq bass-note	note)

                   (setq note (- note root))
                   (if (< note 0)  (setq note (+ note 12)))
                   (if (> note 11) (setq note (- note 12)))

                   (if (= (aref chord-notes note) 0)
                       (setq number-of-notes (1+ number-of-notes)))

                   (aset chord tab-current-string note)
                   (aset chord-notes note (1+ (aref chord-notes note)))

                   (setq bass-note-name (aref tab-note-names bass-note))
                   (setq bass-note-pos note))
                 (setq temporary-goal-column (current-column))
                 (next-line 1)
                 (setq tab-current-string (1+ tab-current-string))))
        (goto-char root-note-marker)
        (setq tab-current-string root-string)

        ;; analyze chord
        (tab-analyze-chord-internal chord
                                    chord-notes
                                    'chord-name
                                    'chord-disclaimer
                                    'chord-spelling)

        ;; if unknown, and root != bass, and bass unique, try without
        (when (and (string= chord-name "??")
                   (/= root bass-note)
                   (= 1 (aref chord-notes bass-note-pos)))
          ;; remove bass note from chord and try again
          (aset chord-notes bass-note-pos 0)
          (setq number-of-notes (1- number-of-notes))
          (tab-analyze-chord-internal chord
                                      chord-notes
                                      'chord-name
                                      'chord-disclaimer
                                      'chord-spelling)
          (unless (string= chord-name "??")
            (setq chord-name
                  (concat chord-name "/" bass-note-name))))

        (setq tab-last-chord (concat root-name chord-name))
        (message "chord: %s%s ... %s"
                 tab-last-chord
                 chord-disclaimer
                 chord-spelling))
    ;; else
    (insert (this-command-keys))))



(defun tab-analyze-chord-internal (chord
                                   chord-notes
                                   chord-name-arg
                                   chord-disclaimer-arg
                                   chord-spelling-arg)
  "Given a 6-element CHORD array, with one note per string, low-to-high,
with 0=root and -1==no_note; a 12-element CHORD-NOTES array containing
occurrances of notes 0-11, 0=root.  Will fill in CHORD-NAME with name
of chord (`m7b5', etc.) or `??' if unknown, CHORD-DISCLAIMER with `,no5'
info, and CHORD-SPELLING with strings (`root', `5th', `X', etc.) describing
each note in chord."

  (let ((number-of-notes)
        (ndx 1)
        (chord-description [])
        (local-chord-name)
        (local-chord-disclaimer)
        (local-chord-spelling))

    (while (< ndx 12)
      (progn (if (> (aref chord-notes ndx) 0)
                 (setq chord-description
                       (vconcat chord-description (list ndx))))
             (setq ndx (1+ ndx))))
    (setq number-of-notes (1+ (length chord-description)))

    (defmacro tc (notes specials name disclaimer)
      (list 'tab-chordtest notes
            specials
            name
            disclaimer
            'chord-description
            'chord
            ''local-chord-name
            ''local-chord-disclaimer
            ''local-chord-spelling))

    (cond
     ((= number-of-notes 1)
      (tc [] []	 "" ",no3,no5"))
     ((= number-of-notes 2)
      (cond
       ((tc [ 3] []		"m"	",no5"	))
       ((tc [ 4] []		""	",no5"	))
       ((tc [ 7] []		"5"	""	))
       ((tc [10] []		"7"	",no3,5"))
       ((tc [11] []		"maj7"	",no3,5"))

       ((tc []   []		"??"	""	))))
     ((= number-of-notes 3)
      (cond
       ((tc [2  7] []		"sus2"	""	))

       ((tc [3  5] [5 "11"]	"m11"	",no5,7"))
       ((tc [3  6] []		"mb5"	""	))
       ((tc [3  7] []		"m"	""	))
       ((tc [3  8] [8 "+5"]	"m+"	""	))
       ((tc [3  9] []		"m6"	",no5"	))
       ((tc [3 10] []		"m7"	",no5"	))

       ((tc [4  5] [5 "11"]	"11"	",no5,7"))
       ((tc [4  6] []		"-5"	""	))
       ((tc [4  7] []		""	""	))
       ((tc [4  8] [8 "+5"]	"+"	""	))
       ((tc [4  9] []		"6"	",no5"	))
       ((tc [4 10] []		"7"	",no5"	))
       ((tc [4 11] []		"maj7"	",no5"	))

       ((tc [5  7] [5 "sus4"]	"sus4"	""	))
       ((tc [5 10] [5 "sus4"]	"7sus4"	",no5"	))

       ((tc [7 10] []		"7"	",no3"	))
       ((tc [7 11] []		"maj7"	",no3"	))

       ((tc []     []			"??"	""	))))
     ((= number-of-notes 4)
      (cond
       ((tc [2  3  7] [2 "9"]		"madd9"		""	))
       ((tc [2  3 10] [2 "9"]		"m9"		",no5"	))
       ((tc [2  4  7] [2 "9"]		"add9"		""	))
       ((tc [2  4 10] [2 "9"]		"9"		",no5"	))
       ((tc [2  7 10] [2 "sus2"]	"7sus2"		""	))
       ((tc [2  7 11] [2 "sus2"]	"maj7sus2"	""	))

       ((tc [3  4 10] [3 "#9"]		"7#9"		",no5"	))
       ((tc [3  5  7] [5 "11"]		"madd11"	""	))
       ((tc [3  6  9] [9 "bb7"]	"dim"		""	))
       ((tc [3  6 10] []		"m7b5"		""	))
       ((tc [3  7  9] []		"m6"		""	))
       ((tc [3  7 10] []		"m7"		""	))
       ((tc [3  8 10] [8 "+5"]		"m7+5"		""	))

       ((tc [4  5  7] [5 "11"]		"add11"		""	))
       ((tc [4  6  7] [6 "+11"]	"add+11"	""	))
       ((tc [4  6  9] []		"add6b5"	""	))
       ((tc [4  6 10] []		"7b5"		""	))
       ((tc [4  6 11] []		"maj7b5"	""	))
       ((tc [4  7  9] []		"6"		""	))
       ((tc [4  7 10] []		"7"		""	))
       ((tc [4  7 11] []		"maj7"		""	))
       ((tc [4  8 10] [8 "+5"]		"7+5"		""	))
       ((tc [4  8 11] [8 "+5"]		"maj7+5"	""	))

       ((tc [5  7 10] [5 "sus4"]	"7sus4"		""	))
       ((tc [5  7 11] [5 "sus4"]	"maj7sus4"	""	))

       ((tc []        []		"??"		""	))))
     ((= number-of-notes 5)
      (cond
       ((tc [1  3  7 10] [1 "b9"]		"m7b9"		""	))
       ((tc [1  4  7 10] [1 "b9"]		"7b9"		""	))

       ((tc [2  3  5  7] [5 "11"]		"m11"		",no7"	))
       ((tc [2  3  5 10] [5 "11"]		"m11"		",no5"	))
       ((tc [2  3  6  9] [2 "9" 9 "bb7"]	"dim9"		""	))
       ((tc [2  3  6 10] [2 "9"]		"m9b5"		""	))
       ((tc [2  3  7  9] [2 "9"]		"m6add9"	""	))
       ((tc [2  3  7 10] [2 "9"]		"m9"		""	))
       ((tc [2  4  5  7] [2 "9" 5 "11"]	"11"		",no7"	))
       ((tc [2  4  5 10] [2 "9" 5 "11"]	"11"		",no5"	))
       ((tc [2  4  6 10] [2 "9"]		"9b5"		""	))
       ((tc [2  4  7  9] [2 "9"]		"6add9"		""	))
       ((tc [2  4  7 10] [2 "9"]		"9"		""	))
       ((tc [2  4  7 11] [2 "9"]		"maj7add9"	""	))
       ((tc [2  5  7 10] [2 "9" 5 "sus4"]	"9sus4"		""	))

       ((tc [3  4  7 10] [3 "#9"]		"7#9"		""	))
       ((tc [3  5  7 10] [5 "11"]		"m11"		",no9"	))
                                        ; m11,no9 == m7add11

       ((tc [4  5  7 10] [5 "11"]		"11"		",no9"	))
                                        ; 11,no9 == 7add11
       ((tc [4  6  7 10] [6 "+11"]		"7add+11"	""	))

       ((tc []		  []			"??"		""	))))
     ((= number-of-notes 6)
      (cond
       ((tc [1  3  5  7 10] [1 "b9" 5 "11"]		"m11b9"	""	))
       ((tc [1  4  5  7 10] [1 "b9" 5 "11"]		"11b9"	""	))

       ((tc [2  3  5  6  9] [2 "9" 5 "11" 9 "bb7"]	"dim11" ""	))
       ((tc [2  3  5  6 10] [2 "9" 5 "11"]		"m11b5"	""	))
       ((tc [2  3  5  7  9] [2 "9" 5 "11" 9 "13"]	"m13"	",no7"	))
       ((tc [2  3  5  7 10] [2 "9" 5 "11"]		"m11"	""	))
       ((tc [2  3  5  9 10] [2 "9" 5 "11" 9 "13"]	"m13"	",no5"	))
       ((tc [2  4  5  6 10] [2 "9" 5 "11"]		"11b5"	""	))
       ((tc [2  4  5  7  9] [2 "9" 5 "11" 9 "13"]	"13"	",no7"	))
       ((tc [2  4  5  7 10] [2 "9" 5 "11"]		"11"	""	))
       ((tc [2  4  5  9 10] [2 "9" 5 "11" 9 "13"]	"13"	",no5"	))
       ((tc [2  5  7  9 10] [2 "9" 5 "11"]		"13"	"no3"	))

       ((tc [3  5  7  9 10] [5 "11" 9 "13"]		"m13"	",no9"	))
       ((tc [4  5  7  9 10] [5 "11" 9 "13"]		"13"	",no9"	))

       ((tc []		     []				"??"	""	)))))

    ;; 0     1    2    3     4    5        6       7     8    9     10      11
    ;; root  b2   2nd  min3  3rd  4th      b5      5th   b6   6th   7th     maj7th
    ;; 8th   b9   9th  b3         11th     +11           +    13th  dom7th  7th
    ;;            sus2            sus4                   aug
    (set chord-name-arg       local-chord-name)
    (set chord-disclaimer-arg local-chord-disclaimer)
    (set chord-spelling-arg   local-chord-spelling)))



(defun tab-chordtest (notes
                      degree-names
                      name
                      disclaimer
                      chord-description
                      chord
                      chord-name
                      chord-disclaimer
                      chord-spelling)
  "Given an n-element NOTES array, modified DEGREE_NAMES for them, NAME and
DISCLAIMER strings, and an n-element CHORD_DESCRIPTION, if notes and
chord-description match: use 6-element CHORD array and fill in
CHORD-NAME with name, CHORD-DISCLAIMER with disclaimer, and CHORD-SPELLING
and return t. Otherwise, leave all alone and returns nil."
  (let ((normal-names ["rt" "b2" "2" "b3" "3" "4" "b5"
                       "5" "b6" "6" "7" "maj7" "x"])
        (names)
        (ndx 0))

    (if (or (equal notes chord-description) (= (length notes) 0))
        (progn (set chord-name name)
               (set chord-disclaimer disclaimer)

               (setq names (copy-sequence normal-names))
               (while (< ndx (length degree-names))
                 (progn (aset names
                              (aref degree-names ndx)
                              (aref degree-names (1+ ndx)))

                        (setq ndx (+ ndx 2))))

               (set chord-spelling
                    (format "%s %s %s %s %s %s  (%s to %s)"
                            (aref names (aref chord 5))
                            (aref names (aref chord 4))
                            (aref names (aref chord 3))
                            (aref names (aref chord 2))
                            (aref names (aref chord 1))
                            (aref names (aref chord 0))
                            tab-5-string-prefix
                            tab-0-string-prefix))

               (if tab-12-tone-chords
                   (let ((adjusted-chord [0 0 0 0 0 0]) (ndx 0))

                     (while (< ndx 6)
                       (let ((note (aref chord (- 5 ndx))))
                         (if (= note 12)
                             (aset adjusted-chord ndx 'x)
                           (aset adjusted-chord ndx note))
                         (setq ndx (1+ ndx))))

                     (set chord-spelling
                          (format "%s  %s" (eval chord-spelling) adjusted-chord))))

               (eval t)))))


(defun tab-analyze-fret ()
  "Return numeric fret value of note cursor is on, or nil if no note"
  (let ((digits 1)
        (fret nil)
        (end))

    (when (looking-at "[0-9]")
      (forward-char 1)
      (setq end (point-marker))
      (backward-char 2)
      (unless (looking-at "[12]")
        (forward-char 1)
        (setq digits 0))

      (setq fret (string-to-int (buffer-substring (point-marker) end)))
      (forward-char digits))

    (setq fret fret)))


(defun tab-analyze-note ()
  "Return numeric note value of note cursor is on, or nil if no note"
  (let ((fret) (note nil))
    (setq fret (tab-analyze-fret))
    (when fret
      (setq note (+ fret (aref tab-current-tuning tab-current-string)))
      (if (>= note 12) (setq note (% note 12))))
    (eval note)))


(defun tab-next-chord-note ()
  (let ((strings-checked 0) (searching t))

    (while (and searching (< strings-checked 6))
      (progn (setq temporary-goal-column (current-column))
             (if (= tab-current-string 5)
                 (progn (previous-line 5)
                        (setq tab-current-string 0))
               (next-line 1)
               (setq tab-current-string (1+ tab-current-string)))

             (if (looking-at "[0-9]") (setq searching nil))
             (setq strings-checked (1+ strings-checked))))

    (when searching (error "No notes in chord"))))


(defun tab-higher-string ()
  "Move note to next-higher string, recursively with wrap-around until blank
string found or all six strings done."
  (interactive)
  (if (tab-check-in-tab)
      (tab-higher-lower-string t)
    (insert (this-command-keys))))


(defun tab-lower-string ()
  "Move note to next-lower string, recursively with wrap-around until blank
string found or all six strings done."
  (interactive)
  (if (tab-check-in-tab)
      (tab-higher-lower-string nil)
    (insert (this-command-keys))))


(defun tab-higher-lower-string (higher)
  "Internal routine to do work of 'tab-higher-string if ARG is t, else
'tab-lower-string'"

  (let ((notes-to-move t)
        (moving-note nil)
        (moving-fret nil)
        (in-way-note (tab-analyze-note))
        (in-way-fret (tab-analyze-fret))
        (moves -1))

    (if (null in-way-note)
        (error "Must be on note to move to higher/lower string"))

    (setq tab-pending-embellishment nil)

    (while notes-to-move
      ;; erase note in way
      (progn (delete-char 1)
             (delete-backward-char 2)
             (insert "---")
             (backward-char 1)

             ;; transpose moving note (if any) to this new string
             (when moving-note
               (let ((new-fret)
                     (old-fret))

                 (setq new-fret
                       (- moving-note (aref tab-current-tuning
                                            tab-current-string)))
                 (setq old-fret moving-fret)

                 (if (< new-fret 0) (setq new-fret (+ new-fret 12)))

                 (cond
                  ((and (> new-fret old-fret) (> new-fret 12))
                   (if (> (- new-fret old-fret) 6)
                       (setq new-fret (- new-fret 12))))
                  ((and (> old-fret new-fret) (<= new-fret 12))
                   (if (> (- old-fret new-fret) 6)
                       (setq new-fret (+ new-fret 12)))))

                 ;; put transposed note on new line
                 (tab-string (int-to-string new-fret) tab-current-string)))

             ;; note in the way will now move
             (setq moving-note in-way-note)
             (setq moving-fret in-way-fret)

             ;; set flag to exit
             (setq notes-to-move moving-note)

             ;; goto next string and get note in the way (if any)
             (if higher
                 (tab-move-string -1)
               (tab-move-string  1))
             (setq in-way-note (tab-analyze-note))
             (setq in-way-fret (tab-analyze-fret))

             ;; count how many notes moved
             (setq moves (1+ moves))))

    ;; get back to note cursor on at beginning
    (if (< moves 6)
        (if higher
            (tab-move-string moves)
          (tab-move-string (- 0 moves))))))


(defun tab-move-string (strings)
  "Move absolute value of STRINGS, down if positive, up if negative."

  (setq temporary-goal-column (current-column))

  (cond
   ((> strings 0)
    (if (<= (+ strings tab-current-string) 5)
        (progn (next-line strings)
               (setq tab-current-string (+ tab-current-string strings)))
      (setq strings (- 6 strings))
      (previous-line strings)
      (setq tab-current-string (- tab-current-string strings))))

   ((< strings 0)
    (if (>= (+ strings tab-current-string) 0)
        (progn (next-line strings)
               (setq tab-current-string (+ tab-current-string strings)))
      (setq strings (+ 6 strings))
      (next-line strings)
      (setq tab-current-string (+ tab-current-string strings))))))


(defun tab-goto-string (string)
  "Go to STRING string, where 0<=string<=5.  Reset tab-current-string"
  (setq temporary-goal-column (current-column))
  (next-line (- string tab-current-string))
  (setq tab-current-string string))


(defun tab-up-12 ()
  "Move current note up 12 frets"
  (interactive)
  (if (tab-check-in-tab)
      (let ((fret (tab-analyze-fret)))
        (when (and (/= fret -1) (<= fret 12))
          (setq fret (+ fret 12))
          (tab-string (int-to-string fret) tab-current-string)))
    ;; else
    (insert (this-command-keys))))


(defun tab-down-12 ()
  "Move current note up 12 frets"
  (interactive)
  (if (tab-check-in-tab)
      (let ((fret (tab-analyze-fret)))
        (when (and (/= fret -1) (>= fret 12))
          (setq fret (- fret 12))
          (tab-string (int-to-string fret) tab-current-string)))
    ;; else
    (insert (this-command-keys))))


(defun tab-unused-key ()
"Ignore keypress if on tab staff; insert normally otherwise"
(interactive)
	(if (not (tab-check-in-tab)) (insert (this-command-keys))))


(defun tab-toggle-embellishment-char (prev-char new-char)
  (if (string= prev-char new-char) "-" new-char))


(defun tab-embellishment (special-character)
  "Mark current note with ARG character"
  (if (tab-check-in-tab)
      (if (looking-at "-")
          (progn (setq tab-pending-embellishment
                       (tab-toggle-embellishment-char tab-pending-embellishment
                                                      special-character))
                 (set-buffer-modified-p (buffer-modified-p)))
        (backward-char 1)
        (if (looking-at "[12]") (backward-char 1))
        (let ((new-embellishment (tab-toggle-embellishment-char
                                  (string (char-after))
                                  special-character)))
          (delete-char 1)
          (insert new-embellishment))
        (forward-char 1)
        (if (not (looking-at "[0-9]")) (backward-char 1)))
    (insert (this-command-keys))))


(defun tab-hammer ()
  (interactive)
  (tab-embellishment "h"))

(defun tab-pull ()
  (interactive)
  (tab-embellishment "p"))

(defun tab-bend ()
  (interactive)
  (tab-embellishment "b"))

(defun tab-release ()
  (interactive)
  (tab-embellishment "r"))

(defun tab-slide-up ()
  (interactive)
  (tab-embellishment "/"))

(defun tab-slide-down ()
  (interactive)
  (tab-embellishment "\\"))

(defun tab-vibrato ()
  (interactive)
  (tab-embellishment "~"))

(defun tab-ghost ()
  (interactive)
  (tab-embellishment "("))

(defun tab-normal ()
  (interactive)
  (tab-embellishment "-"))

(defun tab-muffled ()
  (interactive)
  (tab-embellishment "X"))


(defun tab-string (symbol string)
  "Place first arg note on second arg string."
  (setq temporary-goal-column (current-column))
  (previous-line (- tab-current-string string))
  (delete-char 1)
  (backward-char 1)
  (if (looking-at "[-12]")
      (delete-char 1)
    ;; else
    (delete-backward-char 1)
    (forward-char 1))

  (when (< (length symbol) 2)
    (backward-char 1)
    (insert "-")
    (forward-char 1))

  (insert symbol)

  (when tab-pending-embellishment
    (backward-char (length symbol))
    (delete-backward-char 1)
    (insert tab-pending-embellishment)
    (forward-char (length symbol))
    (setq tab-pending-embellishment nil)
    (set-buffer-modified-p (buffer-modified-p)))

  (if (bound-and-true-p chord-mode) (backward-char 1))
  (if (bound-and-true-p lead-mode) (forward-char 2))

  (setq tab-current-string string))


(defun tab-E (symbol)
(tab-string symbol 5))

(defun tab-A (symbol)
(tab-string symbol 4))

(defun tab-D (symbol)
(tab-string symbol 3))

(defun tab-G (symbol)
(tab-string symbol 2))

(defun tab-B (symbol)
(tab-string symbol 1))

(defun tab-e (symbol)
(tab-string symbol 0))


(defun tab-E-fret (fret)
  (if (tab-check-in-tab)
      (tab-E (int-to-string fret))
    (insert (this-command-keys))))

(defun tab-A-fret (fret)
  (if (tab-check-in-tab)
      (tab-A (int-to-string fret))
    (insert (this-command-keys))))

(defun tab-D-fret (fret)
  (if (tab-check-in-tab)
      (tab-D (int-to-string fret))
    (insert (this-command-keys))))

(defun tab-G-fret (fret)
  (if (tab-check-in-tab)
      (tab-G (int-to-string fret))
    (insert (this-command-keys))))

(defun tab-B-fret (fret)
  (if (tab-check-in-tab)
      (tab-B (int-to-string fret))
    (insert (this-command-keys))))

(defun tab-e-fret (fret)
  (if (tab-check-in-tab)
      (tab-e (int-to-string fret))
    (insert (this-command-keys))))


(defun tab-E-open ()
(interactive)
(tab-E-fret 0))

(defun tab-A-open ()
(interactive)
(tab-A-fret 0))

(defun tab-D-open ()
(interactive)
(tab-D-fret 0))

(defun tab-G-open ()
(interactive)
(tab-G-fret 0))

(defun tab-B-open ()
(interactive)
(tab-B-fret 0))

(defun tab-e-open ()
(interactive)
(tab-e-fret 0))


(defun tab-E-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-E-fret (- tab-position 1))
	(tab-E-fret tab-position)))

(defun tab-A-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-A-fret (- tab-position 1))
	(tab-A-fret tab-position)))

(defun tab-D-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-D-fret (- tab-position 1))
	(tab-D-fret tab-position)))

(defun tab-G-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-G-fret (- tab-position 1))
	(tab-G-fret tab-position)))

(defun tab-B-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-B-fret (- tab-position 1))
	(tab-B-fret tab-position)))

(defun tab-e-1 ()
(interactive)
	(if (> tab-position 0)
	(tab-e-fret (- tab-position 1))
	(tab-e-fret tab-position)))


(defun tab-E0 ()
  (interactive)
  (tab-E-fret tab-position))

(defun tab-A0 ()
  (interactive)
  (tab-A-fret tab-position))

(defun tab-D0 ()
  (interactive)
  (tab-D-fret tab-position))

(defun tab-G0 ()
  (interactive)
  (tab-G-fret tab-position))

(defun tab-B0 ()
  (interactive)
  (tab-B-fret tab-position))

(defun tab-e0 ()
  (interactive)
  (tab-e-fret tab-position))


(defun tab-E1 ()
  (interactive)
  (tab-E-fret (+ tab-position 1)))

(defun tab-A1 ()
  (interactive)
  (tab-A-fret (+ tab-position 1)))

(defun tab-D1 ()
  (interactive)
  (tab-D-fret (+ tab-position 1)))

(defun tab-G1 ()
  (interactive)
  (tab-G-fret (+ tab-position 1)))

(defun tab-B1 ()
  (interactive)
  (tab-B-fret (+ tab-position 1)))

(defun tab-e1 ()
  (interactive)
  (tab-e-fret (+ tab-position 1)))


(defun tab-E2 ()
  (interactive)
  (tab-E-fret (+ tab-position 2)))

(defun tab-A2 ()
  (interactive)
  (tab-A-fret (+ tab-position 2)))

(defun tab-D2 ()
  (interactive)
  (tab-D-fret (+ tab-position 2)))

(defun tab-G2 ()
  (interactive)
  (tab-G-fret (+ tab-position 2)))

(defun tab-B2 ()
  (interactive)
  (tab-B-fret (+ tab-position 2)))

(defun tab-e2 ()
  (interactive)
  (tab-e-fret (+ tab-position 2)))


(defun tab-E3 ()
  (interactive)
  (tab-E-fret (+ tab-position 3)))

(defun tab-A3 ()
  (interactive)
  (tab-A-fret (+ tab-position 3)))

(defun tab-D3 ()
  (interactive)
  (tab-D-fret (+ tab-position 3)))

(defun tab-G3 ()
  (interactive)
  (tab-G-fret (+ tab-position 3)))

(defun tab-B3 ()
  (interactive)
  (tab-B-fret (+ tab-position 3)))

(defun tab-e3 ()
  (interactive)
  (tab-e-fret (+ tab-position 3)))


(defun tab-E4 ()
  (interactive)
  (tab-E-fret (+ tab-position 4)))

(defun tab-A4 ()
  (interactive)
  (tab-A-fret (+ tab-position 4)))

(defun tab-D4 ()
  (interactive)
  (tab-D-fret (+ tab-position 4)))

(defun tab-G4 ()
  (interactive)
  (tab-G-fret (+ tab-position 4)))

(defun tab-B4 ()
  (interactive)
  (tab-B-fret (+ tab-position 4)))

(defun tab-e4 ()
  (interactive)
  (tab-e-fret (+ tab-position 4)))


(defun tab-E5 ()
  (interactive)
  (tab-E-fret (+ tab-position 5)))

(defun tab-A5 ()
  (interactive)
  (tab-A-fret (+ tab-position 5)))

(defun tab-D5 ()
  (interactive)
  (tab-D-fret (+ tab-position 5)))

(defun tab-G5 ()
  (interactive)
  (tab-G-fret (+ tab-position 5)))

(defun tab-B5 ()
  (interactive)
  (tab-B-fret (+ tab-position 5)))

(defun tab-e5 ()
  (interactive)
  (tab-e-fret (+ tab-position 5)))


(defun tab-E6 ()
  (interactive)
  (tab-E-fret (+ tab-position 6)))

(defun tab-A6 ()
  (interactive)
  (tab-A-fret (+ tab-position 6)))

(defun tab-D6 ()
  (interactive)
  (tab-D-fret (+ tab-position 6)))

(defun tab-G6 ()
  (interactive)
  (tab-G-fret (+ tab-position 6)))

(defun tab-B6 ()
  (interactive)
  (tab-B-fret (+ tab-position 6)))

(defun tab-e6 ()
  (interactive)
  (tab-e-fret (+ tab-position 6)))

(provide 'tablature-mode)
