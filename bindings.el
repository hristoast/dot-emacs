;; Capitalization bindings:
;; M-c		Capitalize first letter of word
;; C-c u w	Uppercase word
;; C-c d w	Lowercase word
;; C-x u	Uppercase region
;;
;; Delete bindings:
;; C-d		Delete character under the cursor
;; M-d		Delete next word
;; M-Del	Delete previous word
;; C-k		Delete from cursor to end of line
;; M-k		Delete next sentence
;; C-x Del	Delete previous sentence
;; C-y		Restore deleted thing
;;
;; Emacs bindings:
;; C-c q q q	Save all buffers and close Emacs (useful for daemon mode)
;;
;; Fireplace bindings:
;; d	Move the fireplace up
;; s	Toggle smoke
;; u	Move the fireplace down
;;
;; Lisp bindings:
;; TODO
;;
;; Mark and region commands:
;; C-x C-x	Swap location of cursor and mark
;; C-w		Kill region
;; C-y		Paste most recently killed or copied text
;; M-w		Copy region
;; M-h		Mark paragraph
;; C-x C-p	Mark page
;; C-x h	Mark buffer
;;
;; Navigation bindings:
;; C-f		Move forward (right) one character
;; C-b		Move backward (left) one character
;; C-p		Move to previous line (up)
;; C-n		Move to next line (down)
;; M-f		Move one word forward (right)
;; M-b		Move one word backward (left)
;; C-a		Move to the beginning of the current line
;; C-e		Move to the end of the current line
;; M-e		Move forward (right) one sentence
;; M-a		Move backward (left) one sentence
;; M-}		Move forward (right) one paragraph
;; M-{		Move backward (left) one paragraph
;; C-v		Move forward (down) one screen
;; M-v		Move backward (up) one screen
;; C-x ]	Move forward one page
;; C-x [	Move backward one page
;; M-<		Start of file
;; M->		End of file
;; C-l		Redraw screen with current line in center.  Run twice to push
;;			current line to top, three times to push it to the bottom.
;; M-$n		Repeat the next command $n times
;; C-u $n	Repeat the next command $n times (or four times if $n is omitted)
;;
;; Org mode bindings:
;;
;; C-c C-o				org-open-at-point
;; C-c C-s				org-schedule
;; C-c C-t				org-todo
;; C-c a a				org-agenda
;; C-c a t				Enter global TODO list (requires org-agenda-files be set)
;; C-c l				org-store-link
;; M-S-RET				org-insert-todo-heading
;; S-TAB				Toggle overview/folding
;;
;; Org mode agenda bindings (http://orgmode.org/manual/Agenda-commands.html):
;;
;; A					Interactively select another agenda view and append it to the current view.
;; v d || d				org-agenda-day-view
;; v w || w				org-agenda-week-view
;; v t					org-agenda-fortnight-view
;; v m					org-agenda-month-view
;; v y					org-agenda-year-view
;; v SPC				org-agenda-reset-view
;; f					Go forward in time to display the following org-agenda-current-span days.
;;                      For example, if the display covers a week, switch to the following week.
;;                      With prefix arg, go forward that many times org-agenda-current-span days.
;; b					Go backward in time to display earlier dates.
;; .					Go to today.
;; j					Prompt for a date and go there.
;; J					Go to the currently clocked-in task in the agenda buffer.
;; D					Toggle the inclusion of diary entries.  See http://orgmode.org/manual/Weekly_002fdaily-agenda.html#Weekly_002fdaily-agenda
;; v l || l				Toggle logbook mode.
;;
;; Rectangle bindings:
;; C-x r k				Delete a rectangle and store it
;; C-x r d				Delete a rectangle and do not store it
;; C-x r y				Insert the last rectangle killed
;; C-x r c				Using spaces, blank out the area marked as a rectangle and do not store it
;; C-x r o				Insert a blank rectangle in the area marked
;; C-x r r r			Copy rectangle from register r (where r is any character)
;; C-x r i r			Insert a rectangle from register r (where r is any character)
;; C-x r t string Enter	Changes contents of marked rectangle to string (if string is narrower or
;;					   	wider than rectangle, dimensions change accordingly)
;;
;; Search bindings:
;; C-s C-w	Start an incremental search with the word the cursor is on as the search string
;; C-s C-y	Start an incremental search with the text from the cursor position to the end of
;;			the line as the search string
;; C-s M-y	Start an incremental search with text from the kill ring as the search string
;; C-s C-s	Repeat previous search
;; C-r C-r	Repeat previous reverse search
;;
;; Window management bindings:
;; C-x 2	Divide current window into two windows, one above the other
;; C-x 3	Divide current window into two side-by-side windows
;; C-x 1	Delete other windows but the current one;  run again to undo
;; C-x k	Kill the current buffer
;; C-x ^	Make the current window taller
;; C-x }	Make the current window wider
;; C-x {	Make the current window narrower
;; C-x -	Make the current window smaller if buffer is smaller than window
;; C-x +	Make windows the same size
;; C-M-v	Scroll other window
;;
;; Window movement bindings:
;; C-x o	Move to the other window; if there are several, move to the next window
;; M-k		Move window up
;; M-u		Move window right
;; M-j		Move window down
;; M-e		Move window left
;;
;; Neat functions that could use some bindings:
;; delete-whitespace-rectangle	If a rectangle includes initial whitespace,
;;								deletes it, narrowing rectangle
;; shrink-window				Make the current window taller
;; string-insert-rectangle		Prompts for string and inserts rectangle
;;
;;; Recommended reading:
;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;;;
