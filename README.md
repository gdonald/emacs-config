
## .emacs

My emacs configuration.

### Keyboard Shortcuts

| Keys            | Description                                             |
|-----------------|---------------------------------------------------------|
| C-c p F         | Find file in project.                                   |
| C-x C-r         | Find recent file.                                       |
| C-x C-f         | Find file in file system.                               |
| C-x g           | Magit git status.                                       |
| C-x B           | Switch buffer.                                          |
| C-x k           | Kill buffer.                                            |
| C-c k           | Kill current line.                                      |
| C-c l           | LSP commands.                                           |
| C-c j           | Mark all, do what I mean.                               |
| C-c l           | Create multiple cursors at start of selected lines.     |
| C-c m           | Mark all like this                                      |
| C-c ,           | Mark previous like this.                                |
| C-c .           | Mark next like this.                                    |
| C-c d           | Duplicate current line.                                 |
| C-c /           | Comment out line(s) of code.  Select at least one line. |
| C-c , t         | Switch from implementation file to spec file.           |
| C-c , s         | Run a spec.                                             |
| M-.             | Jump to definition under cursor.                        |
| M-,             | Return from jump to definition.                         |
| S-&lt;up&gt;    | Move line under cursor up.                              |
| S-&lt;down&gt;  | Move line under cursor down.                            |
| S-&lt;left&gt;  | Move selection under cursor left.                       |
| S-&lt;right&gt; | Move selection under cursor right.                      |

### Install

	mkdir ~/workspace
	cd ~/workspace
	git clone git@github.com:gdonald/emacs-config.git
	cd
	ln -s ~/workspace/emacs-config/.emacs

	emacs
	M-x package-install RET use-package RET


