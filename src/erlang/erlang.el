(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("erlang-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/erlang" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/erlang"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil
	 :section-numbers nil
         ;; :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/main.css\" type=\"text/css\"/>"
         :html-preamble t
	 :html-postamble klose-html-postamble
	 :htmlized-source t
         )

        ;; These are static files (images, pdf, etc)
        ("erlang-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/erlang/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("erlang-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/erlang/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/erlang/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("erlang" :components ("erlang-notes" "erlang-static" "erlang-pic"))
        )
      )

(setq org-footnote-re
      (concat "\\[\\(?:"
	      ;; Match inline footnotes.
	      (org-re "fn:\\([-_[:word:]]+\\)?:\\|")
	      ;; Match other footnotes.
	      ;; "\\(?:\\([0-9]+\\)\\]\\)\\|"
	      (org-re "\\(fn:[-_[:word:]]+\\)")
	      "\\)"))

(setq org-footnote-definition-re
      (org-re "^\\[\\(fn:[-_[:word:]]+\\)\\]"))

(defun erlang-publish (no-cache)
  "Publish erlang"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "erlang" t)
    (org-publish "erlang" nil)))

