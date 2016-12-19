(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("docker-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker"
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
        ("docker-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/css"
         :recursive t
         :publishing-function org-publish-attachment
         )
	;; picture
        ("docker-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/docker/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/docker/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("docker" :components ("docker-notes" "docker-static" "docker-pic"))
        )
      )

(defun docker-publish nil
  "Publish docker"
  (interactive)
  (org-publish "docker" nil))
