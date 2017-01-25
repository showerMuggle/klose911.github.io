(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("redis-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/redis" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis"
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
        ("redis-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("redis" :components ("redis-notes" "redis-static"))
        )
      )

(defun redis-publish nil
  "Publish redis"
  (interactive)
  (org-publish "redis" nil))
