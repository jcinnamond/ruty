;;; ruty.el --- an automated typer of ruby code

;; Copyright 2016 John Cinnamond

;; Author: John Cinnamond
;; Version: 1.0.0

;;; Commentary:
;;
;; ruty takes some ruby code and retypes it, trying to make it look
;; natural. It is designed to automate part of recording screencasts
;; containing ruby It is designed to automate part of recording
;; screencasts containing ruby code.
;;
;; There are two main functions. `ruty/retype-buffer` clears the
;; current buffer and retypes its contents. `ruty/insert-buffer` types
;; the contents of another buffer at teh current point.

;;; License: see the file LICENSE.

;;; Code:

(defun ruty/retype-buffer ()
  "Clears the current buffer and retypes its contents"
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (erase-buffer)
    (ruty/parse-and-type content)))

(defun ruty/insert-buffer (b)
  "Types the contents of another buffer at point"
  (interactive "bBuffer to insert: ")
  (with-current-buffer b
    (setq content (buffer-substring-no-properties (point-min) (point-max))))
  (ruty/parse-and-type content))

(defun ruty/parse-and-type (content)
  (ruty/type (ruty/parse-str content)))


;; The automated typing code.
;;
;; The input to the type function should be the structured list
;; returned by the parser (below). The way each element is typed
;; depends on its type. Characters are simply inserted, but structured
;; syntactic elements (such as classes) are automatically closed by
;; inserting the corresponding 'end' immediately. This avoids
;; situations where a series of 'end's are typed at the end, and it
;; keeps the structure of the code clearer as it is typed.
(defun ruty/type (content)
  (unless (null content)
    (ruty/insert-element (car content)) ;; insert the current element
    (ruty/type (cdr content))))         ;; insert the rest of the elements

;; This is the function that understands the kind of content to
;; insert. It supports four types of content. Pairs are balanced
;; structures, typically expected to occur on the same line (e.g.,
;; parenthises). Multiline pairs are balanced structures expected to
;; span multiple lines. Newlines are treated specially because we want
;; to rely on the mode's indentation, rather than typing each space
;; character. Anything else is just inserted verbatim.
(defun ruty/insert-element (element)
  (if (listp element)
      (let ((type (car element))
	    (content-body (cdr element)))
	(case type
	  ('pair           (apply 'ruty/insert-pair content-body))
	  ('multiline-pair (apply 'ruty/insert-multiline-pair content-body))
	  ('newline        (ruty/insert-newline))))
    (ruty/insert element)))

;; Insert balanced content by typing both the opening and closing
;; characters simultaneously, moving the point back to between the
;; balanced pair, typing the content, and then moving to beyond the
;; closing character once done.
(defun ruty/insert-pair (start end content)
  (ruty/insert start)
  (insert end)
  (goto-char (- (point) (length end)))
  (ruty/type content)
  (goto-char (+ (point) (length end))))

;; Insert balanced content over multiple lines. This works much like
;; insert-pair, above, except that it puts the closing keyword on its
;; own line and ensures it is indented properly.
(defun ruty/insert-multiline-pair (start end content)
  (ruty/insert start)
  (newline)
  (insert end)
  (indent-according-to-mode)
  (forward-line -1)
  (end-of-line)
  (ruty/type content)
  (forward-line)
  (end-of-line))

;; Insert a single character at point, and then pause to simulate a
;; real typist.
;;
;; Note that we have to call 'redisplay' after inserting the character
;; because emacs buffers output when running an interactive function.
(defun ruty/insert (str)
  (each-char str (lambda (c)
		   (insert c)
		   (redisplay)
		   (ruty/pause))))

;; Insert a newline, using the current mode to sort out the indentation.
(defun ruty/insert-newline ()
  (newline-and-indent)
  (ruty/pause))

;; Simulate a real typist by pausing for a random amount of time
;; between each charater. Most pauses will up to 0.13 seconds, but
;; occasionally the pause could be up to 0.25 seconds. This variation
;; makes the typing less uncanny.
(defun ruty/pause ()
  (if (= 1 (random 20))
      (sleep-for (/ (random 25) 100.0))
    (sleep-for (/ (random 13) 100.0))))


;; The Ruby parser
;;
;; This is a crappy, hand rolled recursive descent parser for Ruby
;; that does just enough to pull out some important structures such as
;; classes, modules and methods. It returns a list of elements, where
;; each element is a single character to type or another list
;; containing some structured information. This list can be fed
;; directly to the automated typer.
;;
;; The parser never uses a lookahead which occasionally leads to convoluted
;; matching (and potential mismatches).

;; The parser returns any remaining (unparsed) string along with the
;; structured data. This helper function returns only the structured
;; data. The remaining string will be blank at this point beacuse we
;; parse the whole input.
(defun ruty/parse-str (str) (car (ruty/parse str)))

;; A recursive parser. It continually finds the next token until there
;; is no input remaining (which will result in the token 'done being
;; returned), or until some optional end token is encountered.
(defun ruty/parse (str &optional end-token ast)
  (destructuring-bind (rest token) (ruty/next str)
    (if (or (equal token 'done) (equal token end-token))
	(list (reverse ast) rest)
      (ruty/parse rest end-token (cons token ast)))))

;; Finds the next token in the input string, based on a set of
;; patterns. Once a token is identified, we move to the corresponding
;; function for that state. The state's function is responsible for
;; extracting the token and any content for that token. (E.g., the
;; class state is responsible for returning a 'class token, and for
;; extracting the contents of the class.)
;;
;; The return from this function is always of the form
;; '(<remaining string> <token>)
(defun ruty/next (str)
  (cond ((equal ""        str)                        '("" done))
	((ruty/match-word str "module")		      (ruty/state/with-end str "module"))
	((ruty/match-word str "class")		      (ruty/state/with-end str "class"))
	((ruty/match-word str "def")                  (ruty/state/with-end str "def"))
	((ruty/match-word str "if")		      (ruty/state/with-end str "if"))
	((ruty/re-match   str "[[:space:]\n]*end\\b") (ruty/state/end str))
	((ruty/match      str "\n")		      (ruty/state/newline str))
	((ruty/match      str "(")                    (ruty/state/lparen str))
	((ruty/match      str ")")		      (ruty/state/simple str '(rparen)))
	((ruty/match      str "[")		      (ruty/state/lbracket str))
	((ruty/match      str "]")		      (ruty/state/simple str '(rbracket)))
	((ruty/match      str "{")		      (ruty/state/lbrace str))
	((ruty/match      str "}")		      (ruty/state/simple str '(rbrace)))
	(t					      (ruty/state/char str))))

;; Utility functions for matching input.

;; Checks to see if a given string literal occurs at the start of the
;; input.
(defun ruty/match (str match)
  (if (>= (length str) (length match))
      (equal match (substring str 0 (length match)))))

;; Checks to see if a given string literal occurs with a word boundary
;; at the start of the input.
(defun ruty/match-word (str word)
  (ruty/re-match str (format "%s[[:space:]]" word)))

;; Checks to see if a given regexp occurs at the start of the input.
(defun ruty/re-match (str re)
  (equal 0 (string-match-p re str)))

;; The parser states.
;;
;; Each state represents being in a differnt part of the ruby program.
;; Each function is responsible for parsing the rest of the content in
;; that state (the 'recursive' part of 'recursive descent parser') and
;; then returning that parsed content along with the appropriate
;; token.
(defun ruty/state/char (str) (ruty/state/simple str (ruty/first str)))
(defun ruty/state/simple (str token) (list (ruty/rest str) token))

;; When we see a newline we strip any following space because we want
;; to use ruby-mode to indent the next line, rather than retyping each
;; space.
(defun ruty/state/newline (str)
  (list (ruty/strip " *" (p/rest str)) '(newline)))

;; When we encounter an 'end' we strip any leading space, again to
;; allow ruby-mode to indent the line.
(defun ruty/state/end (str)
  (list (ruty/strip "[[:space:]\n]*end" str) '(end)))

;; Various states comprise a ruby keyword, followed by some content
;; and finished by a corresponding 'end'. We delegate the work of
;; finding the closing 'end' to the 'ruty/with-balance' function.
(defun ruty/state/with-end (str start)
  (ruty/with-balance
   (substring str (length start))
   '(end)
   (lambda (ast) (list 'multiline-pair start "end" ast))))

;; Various states comprise an opening character (such as an opening
;; parenthesis), followed by some content and then a closing
;; character. These differ from the 'with-end' states in that they are
;; not expected to span multiple lines, so retyping them is subtly
;; different.
(defun ruty/state/lparen   (str) (ruty/state/pair str "(" ")" '(rparen)))
(defun ruty/state/lbracket (str) (ruty/state/pair str "[" "]" '(rbracket)))
(defun ruty/state/lbrace   (str) (ruty/state/pair str "{" "}" '(rbrace)))

(defun ruty/state/pair (str opening closing closing-token)
  (ruty/with-balance
   (ruty/rest str)
   closing-token
   (lambda (ast) (list 'pair opening closing ast))))

;; A helper function to call the parser, looking for a specific
;; closing token. The final parameter is a function to call with the
;; parser output once the closing token has been found.
(defun ruty/with-balance (str closing-token fn)
  (destructuring-bind (ast rest) (ruty/parse str closing-token)
    (list rest (funcall fn ast))))

;; Utility functions
;;
;; Various functions for manipulating the input string.

(defun ruty/first (str) (substring str 0 1))
(defun ruty/rest  (str) (substring str 1))
(defun ruty/strip (pattern str)
  (replace-regexp-in-string (format "\\`%s" pattern) "" str))
