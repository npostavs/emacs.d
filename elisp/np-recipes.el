(defconst el-get-sources
  '((:name ace-jump-mode                   :checkout "8351e2df4fbbeb2a4003f2fb39f46d33803f3dac")
    (:name anaphora                        :checkout "20bf7dcfa502538d23525f0905b4f845d97993d3"
           :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
           :type github
           :pkgname "rolandwalker/anaphora")
    (:name dash                            :checkout "63022432e1f7bcc5af9582f29d4ed0552a2e658b"
           :branch "2.15.0")
    (:name debbugs                         :checkout "0.16"
           :type elpa :description "SOAP library to access debbugs servers")
    (:name diminish                        :checkout "73669b69e5f9a0c9261c5b53624329d0e24d1ed8")
    (:name dired-details                   :checkout "3de7e19ae874dac03edf9951648a1f11f52dead6")
    (:name el-get                          :checkout "38cd5aa6b4627a6e14abea995e19fb8f648b27b1"
           :pkgname "npostavs/el-get" :branch "origin/current")
    (:name elisp-slime-nav                 :checkout "1a2cb6a832635bde694fd25cc6dce2962aad3807"
           :description "Slime-style navigation of Emacs Lisp source with M-. & M-,"
           :type github
           :pkgname "purcell/elisp-slime-nav")
    (:name ghub                            :checkout "c7ca6780bcd4d00d22e668e74b25f865ba892a45")
    (:name i3-emacs                        :checkout "055510298fe2d5b52a346aebcec859af9b55ac3c"
           :description "i3 emacs integration"
           :type github
           :pkgname "vava/i3-emacs")
    (:name ido-ubiquitous                  :checkout "2c8c0810e1dc5457641bba63ca4c56aca3c9049c")
    (:name ido-complete-space-or-hyphen    :checkout "3fe1fe1e1a743f8deb8f4025977647afecd58f14"
           :description "Make ido completes like built-in M-x does"
           :type github :pkgname "doitian/ido-complete-space-or-hyphen")
    (:name lua-mode                        :checkout "2453e370ca39f38fced67a6d2db462aaea110f22")
    (:name magit                           :checkout "8bb78f0b8651a05a96728484735d361c5e1bbd6a"
           :branch "origin/current" :pkgname "npostavs/magit"
           :build (with-temp-file "lisp/magit-autoloads.el" nil)
           :info nil)
    (:name magit-imerge                    :checkout "546f76d14f16b114837f38d21c29ea6f6adfeaa7"
           :description "Magit extension for git-imerge"
           :type github
           :pkgname "magit/magit-imerge")
    (:name multiple-cursors                :checkout "54e408fc682d968ad46846d8bff079cd704ff6fe")
    (:name package                         :checkout -
           ;; cut out el-get's post-init stuff
           :post-init nil)
    (:name paredit                         :checkout "9a696fdcce87c9d9eec4569a9929d0300ac6ae5c")
    (:name pcre2el                         :checkout "5046f3323724300e42de14cb54e7f65b392ca383"
           :description "Parse, convert, and font-lock PCRE, Emacs and rx regexps"
           :type github
           :pkgname "joddie/pcre2el")
    (:name pretty-symbols                  :checkout "fc49af3da086431c498adf92a86a9b5f70c6aad3"
           :description "Minor mode for drawing multi-character tokens as Unicode glyphs"
           :type github :pkgname "drothlis/pretty-symbols")
    (:name s                               :checkout "e59915ec6fc35983fa4a164ff2a3f73df135ca88")
    (:name smex                            :checkout "c2b14f46439246a23f2178555ba69085db226b19")
    (:name ssh-agency                      :checkout "f8042250174fb72dd935b3e65820580e3232a6fd"
           :description "Use ssh-agent on win32 from Emacs"
           :type github :pkgname "magit/ssh-agency")
    (:name transient                       :checkout "9e30038d2d69e4ad0f3333777137af52e4771442")
    (:name undo-tree                       :checkout "bc9d09555f5aeac6ac4684d748be763f64a7d80a")
    (:name use-package                     :checkout "38034854ac21bd5ddc1a1129fd6c8ff86d939f8a")
    (:name with-editor                     :checkout "38df9bfc2227bcb7ac4899c83a03756d5f171450")
    (:name yasnippet                       :checkout "ecd65d2ba5ea2623dfd0b474fbb5dae231503637"
           :branch "origin/current" :pkgname "npostavs/yasnippet"
           ;; Don't fetch snippets submodule
           :build nil)
    (:name yasnippet-snippets              :checkout "871c6f022d51f3cdffb83e649ddb29fa0cfddc79"))
 )


(provide 'np-recipes)
