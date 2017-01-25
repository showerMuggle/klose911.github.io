(require 'ox-publish)

(setq klose-html-postamble
      "<p class=\"postamble\">Last Updated %C. Created by %a</p>
<div id=\"uyan_frame\"></div>
<script type=\"text/javascript\" src=\"http://v2.uyan.cc/code/uyan.js?uid=2124392\"></script>
") 
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("jcip-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/jcip" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/jcip"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil
	 :section-numbers nil
         ;; :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/main.css\" type=\"text/css\"/>"
         :html-preamble t
	  :html-postamble "<p class=\"postamble\">Last Updated %C. Created by %a</p>
<div id=\"uyan_frame\"></div>
<script type=\"text/javascript\" src=\"http://v2.uyan.cc/code/uyan.js?uid=2124392\"></script>
<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
  ga('create', 'UA-90850421-1', 'auto');
  ga('send', 'pageview');
</script>
"
	 :htmlized-source t
         )

        ;; These are static files (images, pdf, etc)
        ("jcip-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/jcip/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("jcip" :components ("jcip-notes" "jcip-static"))
        )
      )

(defun jcip-publish nil
  "Publish jcip"
  (interactive)
  (org-publish "jcip" nil))
