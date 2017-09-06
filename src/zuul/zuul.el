(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("zuul-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/zuul" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/zuul"
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
        ("zuul-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/zuul/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("zuul-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/zuul/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/zuul/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("zuul" :components ("zuul-notes" "zuul-static" "zuul-pic"))
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

(defun zuul-publish (no-cache)
  "Publish zuul"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "zuul" t)
    (org-publish "zuul" nil)))

