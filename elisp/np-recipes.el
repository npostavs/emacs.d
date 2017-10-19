(defconst el-get-sources
  '((:name ace-jump-mode                   :checkout "8351e2df4fbbeb2a4003f2fb39f46d33803f3dac")
    (:name anaphora                        :checkout "20bf7dcfa502538d23525f0905b4f845d97993d3"
           :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
           :type github
           :pkgname "rolandwalker/anaphora")
    (:name dash                            :checkout "fec6f5480d0ce03ead0e6117ac77dc7e757e76f8"
           :branch "2.12.1")
    (:name debbugs                         :checkout "0.14"
           :type elpa :description "SOAP library to access debbugs servers")
    (:name diminish                        :checkout "73669b69e5f9a0c9261c5b53624329d0e24d1ed8")
    (:name dired-details                   :checkout "3de7e19ae874dac03edf9951648a1f11f52dead6")
    (:name el-get                          :checkout "cfb081e3563df0da60de554ecad4dbb20857f033"
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
    (:name magit                           :checkout "4def4474e3e1f2b21f38bff41922e0bea31a3620"
           :branch "origin/current" :pkgname "npostavs/magit"
           :build (with-temp-file "lisp/magit-autoloads.el" nil))
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
    (:name sml-mode                        :checkout "6.7"
           :description
           "SML-mode is a major Emacs mode for editing Standard ML source code."
           :type elpa)
    (:name ssh-agency                      :checkout "f8042250174fb72dd935b3e65820580e3232a6fd"
           :description "Use ssh-agent on win32 from Emacs"
           :type github :pkgname "magit/ssh-agency")
    (:name undo-tree                       :checkout "bc9d09555f5aeac6ac4684d748be763f64a7d80a")
    (:name use-package                     :checkout "38034854ac21bd5ddc1a1129fd6c8ff86d939f8a")
    (:name with-editor                     :checkout "8ae3c7aed92842f5988671c1b3350c65c58857e0")
    (:name yasnippet                       :checkout "688571f63ba6b074a99f0d7f93e54317774279e8"
           :branch "origin/current" :pkgname "npostavs/yasnippet"))
 )


(provide 'np-recipes)
