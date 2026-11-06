(require "vim-hx/init.scm")
(set-vim-keybindings!)

(require "forest/forest.scm")
(forest-set-keybinds!
  (hash 'wider ">"
        'narrower "<"
        'toggle-hidden "H"))
