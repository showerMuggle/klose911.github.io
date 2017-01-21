(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("apue-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/apue" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/apue"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil
	 :section-numbers nil
         ;; :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/main.css\" type=\"text/css\"/>"
         :html-preamble t
	 :htmlized-source t
         )

        ;; These are static files (images, pdf, etc)
        ("apue-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/apue/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("apue-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/apue/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/apue/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("apue" :components ("apue-notes" "apue-static" "apue-pic"))
        )
      )

(defun apue-publish nil
  "Publish apue"
  (interactive)
  (org-publish "apue" nil))
