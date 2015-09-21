(defconst el-get-sources
  '((:name ace-jump-mode                   :checkout "8351e2df4fbbeb2a4003f2fb39f46d33803f3dac")
    (:name anaphora                        :checkout "20bf7dcfa502538d23525f0905b4f845d97993d3"
           :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
           :type github
           :pkgname "rolandwalker/anaphora")
    (:name ample-regexps                   :checkout "f24c5991927c8eb180d9f1d1614da238daba6423")
    (:name dash                            :checkout "446c5226337ca132da6148893c43ebb3335d91ea"
           :branch "2.10.0")
    (:name diminish                        :checkout "73669b69e5f9a0c9261c5b53624329d0e24d1ed8")
    (:name dired-details                   :checkout "3de7e19ae874dac03edf9951648a1f11f52dead6")
    (:name el-get                          :checkout "8a0e577d297346d322fe38fc818976fa1fcb9ed7"
           :pkgname "npostavs/el-get" :branch "origin/current")
    (:name elisp-slime-nav                 :checkout "1a2cb6a832635bde694fd25cc6dce2962aad3807"
           :description "Slime-style navigation of Emacs Lisp source with M-. & M-,"
           :type github
           :pkgname "purcell/elisp-slime-nav")
    (:name i3-emacs                        :checkout "055510298fe2d5b52a346aebcec859af9b55ac3c"
           :description "i3 emacs integration"
           :type github
           :pkgname "vava/i3-emacs")
    (:name ido-ubiquitous                  :checkout "2c8c0810e1dc5457641bba63ca4c56aca3c9049c")
    (:name ido-complete-space-or-hyphen    :checkout "3fe1fe1e1a743f8deb8f4025977647afecd58f14"
           :description "Make ido completes like built-in M-x does"
           :type github :pkgname "doitian/ido-complete-space-or-hyphen")
    (:name lua-mode                        :checkout "2453e370ca39f38fced67a6d2db462aaea110f22")
    (:name magit                           :checkout "884c81ad5cf5435ccf4fe4354d1d3d34d3ecfa1e"
           :branch "origin/current" :pkgname "npostavs/magit"
           :build/windows-nt (with-temp-file "lisp/magit-autoloads.el" nil))
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
    (:name s                               :checkout "e59915ec6fc35983fa4a164ff2a3f73df135ca88")
    (:name smex                            :checkout "c2b14f46439246a23f2178555ba69085db226b19")
    (:name sml-mode                        :checkout "6.7"
           :description
           "SML-mode is a major Emacs mode for editing Standard ML source code."
           :type elpa)
    (:name ssh-agency                      :checkout "9f07001c00265101f4af56ad3a0cac216832a27a"
           :description "Use ssh-agent on win32 from Emacs"
           :type github :pkgname "magit/ssh-agency")
    (:name undo-tree                       :checkout "bc9d09555f5aeac6ac4684d748be763f64a7d80a")
    (:name use-package                     :checkout "af3ee10a9b0ba4c55d3fb12638ae541638b23e2c")
    (:name yasnippet                       :checkout "be2f815c43deb74e0f809ed47debc4aa2e67ea1e"))
 )


(provide 'np-recipes)
