(defvar el-get-sources
  '((:name ace-jump-mode                   :checkout "c60f7d2e84c9eb283627e30d1949462f403d877b")
    (:name anaphora                        :checkout "20bf7dcfa502538d23525f0905b4f845d97993d3"
           :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
           :type github
           :pkgname "rolandwalker/anaphora")
    (:name dash                            :checkout "8dfa9b6e6df6162ee6f0271a23276cf1995667ab"
           :branch "2.6.0")
    (:name diminish                        :checkout "73669b69e5f9a0c9261c5b53624329d0e24d1ed8")
    (:name dired-details                   :checkout "3de7e19ae874dac03edf9951648a1f11f52dead6")
    (:name el-get                          :checkout "50ec866d28ef4656706408bdecf20fca13299c7b"
           :pkgname "npostavs/el-get" :branch "origin/current")
    (:name elisp-slime-nav                 :checkout "1a2cb6a832635bde694fd25cc6dce2962aad3807"
           :description "Slime-style navigation of Emacs Lisp source with M-. & M-,"
           :type github
           :pkgname "purcell/elisp-slime-nav")
    (:name git-modes                       :checkout "d19ee56e17d2f1bac87ff81e864b880e8f9e5fca"
           :type github :pkgname "magit/git-modes" :branch "origin/next")
    (:name htmlize                         :checkout "1e48ccdfc3aec0f80323036578c232300673a64a")
    (:name i3-emacs                        :checkout "055510298fe2d5b52a346aebcec859af9b55ac3c"
           :description "i3 emacs integration"
           :type github
           :pkgname "vava/i3-emacs")
    (:name ido-complete-space-or-hyphen    :checkout "3fe1fe1e1a743f8deb8f4025977647afecd58f14"
           :description "Make ido completes like built-in M-x does"
           :type github :pkgname "doitian/ido-complete-space-or-hyphen")
    (:name lua-mode                        :checkout "67a90221b4b82c559d62275e73dd966ff5503ed3")
    (:name magit                           :checkout "548262dc3659d197efc6fd064e91b7a8d890546b"
           :branch "origin/current" :depends (dash)
           :pkgname "npostavs/magit" :autoloads t :build nil)
    (:name multiple-cursors                :checkout "54e408fc682d968ad46846d8bff079cd704ff6fe")
    (:name package                         :checkout BUILTIN
           ;; cut out el-get's post-init stuff
           :post-init nil)
    (:name paredit                         :checkout "b1bb6fdb0517a0df1cc7b6fe0730e7ac2d06e9fc")
    (:name pcre2el                         :checkout "5046f3323724300e42de14cb54e7f65b392ca383"
           :description "Parse, convert, and font-lock PCRE, Emacs and rx regexps"
           :type github
           :pkgname "joddie/pcre2el")
    (:name pretty-symbols                  :checkout "fc49af3da086431c498adf92a86a9b5f70c6aad3"
           :description "Minor mode for drawing multi-character tokens as Unicode glyphs"
           :type github :pkgname "drothlis/pretty-symbols")
    (:name smex                            :checkout "c2b14f46439246a23f2178555ba69085db226b19")
    (:name sml-mode                        :checkout "6.4"
           :description
           "SML-mode is a major Emacs mode for editing Standard ML source code."
           :type elpa)
    (:name undo-tree                       :checkout "bc9d09555f5aeac6ac4684d748be763f64a7d80a")
    (:name use-package                     :checkout "02833c292899d406c76233da255e3304c4ab24f3")
    (:name yasnippet                       :checkout "763f5faa14614b207d65e03594b80ba662f47efc"))
 )


(provide 'np-recipes)