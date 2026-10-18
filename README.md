
## emacs-config

My emacs configuration.

### Editing the config

The config lives in [config.org](config.org), an Org file of `emacs-lisp` source blocks with headings around them. Edit it in Emacs and save. Saving writes `init.el` and `early-init.el` with `org-babel-tangle`. Do not edit those two files by hand, because the next save of `config.org` overwrites them. Source blocks marked `:tangle no` stay in `config.org` for reference and are not written out.

Customize writes to `custom.el`, which `init.el` loads. That is where Emacs records `package-selected-packages` when it installs a package.

### Keyboard Shortcuts

| Keys            | Description                                             |
| --------------- | ------------------------------------------------------- |
| C-x B           | Switch buffer in other window.                          |
| C-x C-f         | Find file in file system.                               |
| C-x C-r         | Find recent file.                                       |
| C-x g           | Magit git status.                                       |
| C-x k           | Kill buffer.                                            |
| C-x 2           | Split window down.                                      |
| C-x 3           | Split window right.                                     |
| C-x 0           | Delete current window.                                  |
| C-x u           | Undo, works on regions too.                             |
| C-c k           | Kill current line.                                      |
| C-c a           | Act on the thing at point or the current candidate.     |
| C-c c           | Open the project's Claude Code sidebar, or start one.   |
| C-c o           | Show or hide the Claude Code sidebar.                   |
| C-c s           | Send the region or the error at point to Claude Code.   |
| C-c S           | Send the current file to Claude Code.                   |
| C-c j           | Mark all, do what I mean.                               |
| C-c e           | Create multiple cursors at start of selected lines.     |
| C-c m           | Mark all like this                                      |
| C-c p F         | Find file in project.                                   |
| C-c ,           | Mark previous like this.                                |
| C-c .           | Mark next like this.                                    |
| C-c d           | Duplicate current line.                                 |
| C-c /           | Comment out line(s) of code.  Select at least one line. |
| C-c , t         | Switch from implementation file to spec file.           |
| C-c , s         | Run a spec.                                             |
| C-c b           | Back one tab                                            |
| C-c f           | Forward one tab                                         |
| C-c l           | Move current tab left                                   |
| C-c r           | Move current tab right                                  |
| C-c t           | Show or hide the treemacs file tree.                    |
| C-d             | Delete character to the right of cursor.                |
| M-.             | Jump to definition under cursor.                        |
| M-,             | Return from jump to definition.                         |
| M-d             | Delete word to the right of cursor                      |
| M-+             | Expand or complete a template.                          |
| M-*             | Insert a template by name.                              |
| M-g g           | Goto line number.                                       |
| S-&lt;up&gt;    | Move line under cursor up.                              |
| S-&lt;down&gt;  | Move line under cursor down.                            |
| S-&lt;left&gt;  | Move selection under cursor left.                       |
| S-&lt;right&gt; | Move selection under cursor right.                      |
| F12             | Re-indent buffer.                                       |

### Packages

Installed from the package archives by `use-package` with `:ensure t`:

| Package               | Purpose                                                                    |
| --------------------- | -------------------------------------------------------------------------- |
| adoc-mode             | AsciiDoc editing.                                                          |
| agent-shell           | Claude Code sessions in a right-side window, one per project.              |
| blackjack             | The game of Blackjack.                                                     |
| centaur-tabs          | Tabs across the top, grouped by projectile project.                        |
| cider                 | Clojure REPL and evaluation.                                               |
| clojure-ts-mode       | Clojure editing with tree-sitter.                                          |
| consult               | Search and navigation commands, such as buffer switching and recent files. |
| corfu                 | In-buffer completion popup.                                                |
| crystal-mode          | Crystal editing.                                                           |
| dashboard             | Startup screen with recent files and projects.                             |
| diff-hl               | Version control changes in the margin.                                     |
| doom-modeline         | Mode line.                                                                 |
| doom-themes           | The doom-material-dark theme.                                              |
| drag-stuff            | Move lines and regions with `S-<arrow>`.                                   |
| embark                | Actions on the thing at point or the current completion candidate.         |
| embark-consult        | Embark integration for consult, such as exporting search results.          |
| gdscript-mode         | Godot GDScript editing.                                                    |
| haml-mode             | HAML editing.                                                              |
| inf-ruby              | Ruby REPL, used by robe and rspec-mode.                                    |
| kotlin-ts-mode        | Kotlin editing with tree-sitter.                                           |
| magit                 | Version control interface.                                                 |
| marginalia            | Annotations next to minibuffer completion candidates.                      |
| multiple-cursors      | Edit at several points at once.                                            |
| nerd-icons            | Icon glyphs from a Nerd Font.                                              |
| nerd-icons-completion | Icons next to minibuffer completion candidates.                            |
| orderless             | Space-separated, out-of-order matching for completion.                     |
| org-roam              | Linked notes in Org files.                                                 |
| projectile            | Project navigation.                                                        |
| projectile-rails      | Rails navigation within a projectile project.                              |
| rainbow-delimiters    | Colors nested parentheses by depth.                                        |
| raku-mode             | Raku editing.                                                              |
| rbenv                 | Uses the project's rbenv Ruby version.                                     |
| robe                  | Ruby code navigation and completion.                                       |
| rspec-mode            | Run specs and switch between code and spec files.                          |
| rubocop               | Run RuboCop from Ruby buffers.                                             |
| scss-mode             | SCSS editing.                                                              |
| sly                   | Common Lisp IDE that runs SBCL with `M-x sly`.                             |
| smalltalk-mode        | Smalltalk editing.                                                         |
| swift-mode            | Swift editing. eglot starts `sourcekit-lsp` in Swift buffers.              |
| tempel                | Templates offered as completion candidates.                                |
| tempel-collection     | Ready-made templates for tempel.                                           |
| terraform-mode        | Terraform and `.tfvars` editing.                                           |
| treemacs              | File tree on the left that follows the current file and project.           |
| treemacs-magit        | Refreshes treemacs git status after magit commands.                        |
| treemacs-nerd-icons   | nerd-icons file icons in treemacs.                                         |
| treemacs-projectile   | Adds projectile projects to treemacs.                                      |
| vertico               | Vertical minibuffer completion.                                            |
| web-mode              | HTML and ERB editing.                                                      |
| zig-ts-mode           | Zig editing with tree-sitter.                                              |

Built into Emacs 31, configured with `use-package` without `:ensure`:

| Package          | Purpose                                                                        |
| ---------------- | ------------------------------------------------------------------------------ |
| c-ts-mode        | C editing with the linux style, 2-space indent, and `case` indented one level. |
| editorconfig     | Applies a project's `.editorconfig` settings.                                  |
| eglot            | Language server client, started for Ruby, Rust, C, C++, and Swift.             |
| flymake          | On-the-fly diagnostics in programming buffers.                                 |
| markdown-ts-mode | Markdown editing with tree-sitter, mapped to `.md` files.                      |
| ruby-mode        | Ruby editing. Remapped to `ruby-ts-mode` by tree-sitter.                       |
| treesit          | Tree-sitter modes for Ruby, Rust, YAML, CSS, TypeScript, and other languages.  |
| which-key        | Shows the keys available after a prefix key.                                   |

### Install

Requires Emacs 31.1:

	brew install emacs

The Claude Code integration needs the Agent Client Protocol adapter. Without `claude-agent-acp` on the `PATH`, the config skips agent-shell and leaves its keys unbound:

	npm install -g @agentclientprotocol/claude-agent-acp

Then the config:

	mkdir ~/workspace
	cd ~/workspace
	git clone git@github.com:gdonald/emacs-config.git
	mkdir -p ~/.emacs.d
	ln -s ~/workspace/emacs-config/early-init.el ~/.emacs.d/early-init.el
	ln -s ~/workspace/emacs-config/init.el ~/.emacs.d/init.el
	emacs

Both links go in `~/.emacs.d/`, not the home directory. Emacs reads `early-init.el` only from `user-emacs-directory` (`~/.emacs.d/` or `~/.config/emacs/`), never from `~/`. If an old `~/.emacs` or `~/.emacs.el` is present, Emacs loads it instead of `~/.emacs.d/init.el`, so remove it.

The first time Emacs opens a file in a tree-sitter mode, it asks to install that language's grammar. Installing a grammar needs `git` and a C compiler.

External programs the config uses:

| Program       | Used for                                                                      |
| ------------- | ----------------------------------------------------------------------------- |
| ruby-lsp      | eglot in Ruby buffers. Install the gem in every Ruby version a project uses.  |
| rust-analyzer | eglot in Rust buffers.                                                        |
| clangd        | eglot in C and C++ buffers.                                                   |
| sourcekit-lsp | eglot in Swift buffers. Installed with Xcode.                                 |
| sbcl          | Common Lisp for SLY.                                                          |
| shellcheck    | flymake in shell script buffers.                                              |
| A Nerd Font   | Icons in tabs, the dashboard, and completion. Set it as the terminal's font.  |

### License

GNU General Public License v3.0. See [LICENSE](LICENSE).
