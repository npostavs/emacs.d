(defconst el-get-sources
  '((:name ace-jump-mode                   :checkout "c60f7d2e84c9eb283627e30d1949462f403d877b")
    (:name anaphora                        :checkout "20bf7dcfa502538d23525f0905b4f845d97993d3"
           :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
           :type github
           :pkgname "rolandwalker/anaphora")
    (:name ample-regexps                   :checkout "f24c5991927c8eb180d9f1d1614da238daba6423")
    (:name dash                            :checkout "31f321a9dcfe3a2ad0436a3d636331c942ff7acc"
           :branch "2.9.0")
    (:name diminish                        :checkout "73669b69e5f9a0c9261c5b53624329d0e24d1ed8")
    (:name dired-details                   :checkout "3de7e19ae874dac03edf9951648a1f11f52dead6")
    (:name el-get                          :checkout "d80c48b97a1986623be7cc31f8ff317081883ccd"
           :pkgname "npostavs/el-get" :branch "origin/current")
    (:name elisp-slime-nav                 :checkout "1a2cb6a832635bde694fd25cc6dce2962aad3807"
           :description "Slime-style navigation of Emacs Lisp source with M-. & M-,"
           :type github
           :pkgname "purcell/elisp-slime-nav")
    (:name i3-emacs                        :checkout "055510298fe2d5b52a346aebcec859af9b55ac3c"
           :description "i3 emacs integration"
           :type github
           :pkgname "vava/i3-emacs")
    (:name ido-ubiquitous                  :checkout "c430dd331dcadc1efc5dee7aa5226c175f2f7a56")
    (:name ido-complete-space-or-hyphen    :checkout "3fe1fe1e1a743f8deb8f4025977647afecd58f14"
           :description "Make ido completes like built-in M-x does"
           :type github :pkgname "doitian/ido-complete-space-or-hyphen")
    (:name lua-mode                        :checkout "2453e370ca39f38fced67a6d2db462aaea110f22")
    (:name magit                           :checkout "2df74ef6cd61ed3543b0166b20508ae48fa4d537"
           :branch "origin/current" :depends (dash)
           :pkgname "npostavs/magit" :autoloads t :build nil)
    (:name multiple-cursors                :checkout "54e408fc682d968ad46846d8bff079cd704ff6fe")
    (:name package                         :checkout -
           ;; cut out el-get's post-init stuff
           :post-init nil)
    (:name paredit                         :checkout "808b4305877355fd11dc9c4c7bf00990e636b01d")
    (:name pcre2el                         :checkout "5046f3323724300e42de14cb54e7f65b392ca383"
           :description "Parse, convert, and font-lock PCRE, Emacs and rx regexps"
           :type github
           :pkgname "joddie/pcre2el")
    (:name pretty-symbols                  :checkout "fc49af3da086431c498adf92a86a9b5f70c6aad3"
           :description "Minor mode for drawing multi-character tokens as Unicode glyphs"
           :type github :pkgname "drothlis/pretty-symbols")
    (:name smex                            :checkout "c2b14f46439246a23f2178555ba69085db226b19")
    (:name sml-mode                        :checkout "6.5"
           :description
           "SML-mode is a major Emacs mode for editing Standard ML source code."
           :type elpa)
    (:name undo-tree                       :checkout "bc9d09555f5aeac6ac4684d748be763f64a7d80a")
    (:name use-package                     :checkout "1df5bbcf5f5d23e2a43750fd3fa765109dae3255")
    (:name yasnippet                       :checkout "be2f815c43deb74e0f809ed47debc4aa2e67ea1e"))
 )


(provide 'np-recipes)
