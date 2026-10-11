# TODO

Target: GNU Emacs 31.1. Update README.md in the same change as each item that alters keybindings, install steps, or requirements.

Fix Existing Bugs

- [x] 1.1. Replace `:custom` blocks that contain `setq`/`add-hook` forms with `:config` or `:hook`
  - [x] 1.1.1. web-mode: markup, css, and code indent offsets, auto-pairing, element and column highlighting
  - [x] 1.1.2. rspec-mode: `compilation-scroll-output`
  - [x] 1.1.3. rubocop: add `rubocop-mode` to `ruby-mode-hook`
  - [x] 1.1.4. yaml-mode: `.yml` in `auto-mode-alist`
  - [x] 1.1.5. centaur-tabs: call `centaur-tabs-group-by-projectile-project` from `:config`
- [x] 1.2. Fix the rspec-mode `:hook` form so `inf-ruby-auto-enter-and-focus` lands on `compilation-filter-hook`
- [x] 1.3. Use `setq-default` for `tab-width`
- [x] 1.4. Remove the custom `duplicate-line` defun and bind the built-in `duplicate-line` to `C-c d`
- [x] 1.5. Remove duplicate configuration
  - [x] 1.5.1. Second `projectile-rails` block
  - [x] 1.5.2. Second `lsp-treemacs` block
  - [x] 1.5.3. Second `which-key` block
  - [x] 1.5.4. `robe-mode` hooked from both ruby-mode and robe blocks
  - [x] 1.5.5. rust-mode hooking `lsp` while lsp-mode hooks `lsp-deferred`
- [x] 1.6. Remove dead code
  - [x] 1.6.1. `lisp/jsonrpc.el`, its `require`, and the `~/.emacs.d/lisp` load-path entry
  - [x] 1.6.2. `copilot-node-executable`
  - [x] 1.6.3. rinari
  - [x] 1.6.4. `setq` lines for `show-paren-mode`, `column-number-mode`, and `size-indication-mode`
- [x] 1.7. README: fix the keybinding table (`C-c l` is listed three times, `mc/edit-lines` is bound to `C-c e`)
- [x] 1.8. Turn off `xterm-mouse-mode`, which Emacs 31 enables by default in compatible terminals

Startup Time

- [x] 2.1. Record a baseline with `M-x emacs-init-time` (0.763s)
- [x] 2.2. Move to the `~/.emacs.d/` layout with `early-init.el` and `init.el`, and update the README install steps
- [x] 2.3. In `early-init.el`, raise `gc-cons-threshold` during startup and reset it from `emacs-startup-hook`
- [x] 2.4. Remove the explicit `package-initialize` and `package-refresh-contents` calls
- [x] 2.5. Set `package-quickstart t`
- [x] 2.6. Defer packages that load eagerly
  - [x] 2.6.1. magit
  - [x] 2.6.2. org-roam
  - [x] 2.6.3. flycheck
  - [x] 2.6.4. dashboard stays eager because it draws the startup screen
  - [x] 2.6.5. projectile-rails
- [x] 2.7. Evaluate an Emacs 31 build with native compilation (emacs-plus@31 `--without-cocoa` loaded init in 0.40s against 0.27s for byte code, with no runtime gain beyond 4%, so the Homebrew bottle stays)
- [x] 2.8. Record the new `emacs-init-time` against the baseline (0.249s, down from 0.763s)

Emacs 31 Built-ins

- [x] 3.1. Enable tree-sitter modes with `treesit-enabled-modes` and `treesit-auto-install-grammar`
  - [x] 3.1.1. ruby-ts-mode
  - [x] 3.1.2. rust-ts-mode
  - [x] 3.1.3. yaml-ts-mode
  - [x] 3.1.4. css-ts-mode
  - [x] 3.1.5. html-ts-mode (`.html` and `.erb` stay in web-mode, which handles ERB)
  - [x] 3.1.6. typescript-ts-mode
- [x] 3.2. Replace lsp-mode, lsp-ivy, and lsp-treemacs with eglot
- [x] 3.3. Replace flycheck with flymake
- [x] 3.4. Drop `:ensure t` from which-key and editorconfig
- [x] 3.5. README: list Emacs 31.1 as the required version

Package Modernization

- [x] 4.1. Replace ivy, counsel, and smex with vertico, orderless, and consult
  - [x] 4.1.1. Rebind `C-x C-r` and `C-x B` to consult commands
  - [x] 4.1.2. Add embark
- [x] 4.2. Replace company with corfu
- [x] 4.3. Replace all-the-icons with nerd-icons
- [x] 4.4. README: update the keybinding table

LLM Integration

- [x] 5.1. Add agent-shell for Claude Code sessions
- [x] 5.2. Install the `claude-agent-acp` adapter and document it in the README install steps
- [x] 5.3. README: document the new keybindings
