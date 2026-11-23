(require "vim-hx/init.scm")
(set-vim-keybindings!)

(require "forest/forest.scm")
(forest-set-keybinds!
  (hash 'wider ">"
        'narrower "<"
        'toggle-hidden "H"))

(require (only-in "helix/keymaps.scm" keymap deep-copy-global-keybindings))
(for-each
  (lambda (ext)
    (keymap
      (extension ext (inherit-from (deep-copy-global-keybindings)))
      (normal
        (space
          (k (c ":write")
             (v ":latex-view"))))))
  '("tex" "sty" "cls" "dtx" "ins" "bbx" "cbx" "Rd"))
