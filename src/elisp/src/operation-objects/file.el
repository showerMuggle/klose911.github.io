;;;;;;;;;;;
;; File  ;;
;;;;;;;;;;;

;;; read 
(find-file "~/tmp/test.txt")
(with-current-buffer
    (find-file-noselect "~/tmp/test.txt")
  buffer-file-name)                     ; => "/home/klose/tmp/test.txt"
(find-buffer-visiting "~/tmp/test.txt") ; => #<buffer test.txt>
(get-file-buffer "~/tmp/test.txt")      ; => #<buffer test.txt>

;;; write 
