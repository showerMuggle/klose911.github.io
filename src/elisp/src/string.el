;;;;;;;;;;;;
;; String ;;
;;;;;;;;;;;;

(setq foo "abc\000abc")                 ; => "abc^@abc"

;;; alphabet 
?A                                      ; => 65
?a                                      ; => 97

;;; punction char 
?\a ;; => 7                 ; control-g, `C-g'
?\b ;; => 8                 ; backspace, <BS>, `C-h'
?\t ;; => 9                 ; tab, <TAB>, `C-i'
?\n ;; => 10                ; newline, `C-j'
?\v ;; => 11                ; vertical tab, `C-k'
?\f ;; => 12                ; formfeed character, `C-l'
?\r ;; => 13                ; carriage return, <RET>, `C-m'
?\e ;; => 27                ; escape character, <ESC>, `C-['
?\s ;; => 32                ; space character, <SPC>
?\\ ;; => 92                ; backslash character, `\'
?\d ;; => 127               ; delete character, <DEL>

;;; control char
?\^I ;; => 9 (#o11, #x9, ?\C-i)
?\^i ;; => 9 (#o11, #x9, ?\C-i)
?\C-I ;; => 9 (#o11, #x9, ?\C-i)
?\C-i ;; => 9 (#o11, #x9, ?\C-i)

;;; meta char
(logior (lsh 1 27) ?A)                  ; => 134217793
?\M-A                                   ; => 134217793

;;; type function
(defun string-emptyp (str)
  (not (string< "" str)))

(string-emptyp "")

;;; constructor
(make-string 5 ?x)                      ; => "xxxxx"
(string ?a ?b ?c)                       ; => "abc"

(substring "0123456789" 3)              ; => "3456789"
(substring "0123456789" 3 5)            ; => "34"
(substring "0123456789" -3 -1)          ; => "78"

;;; compare string

;;; convert

;;; 
