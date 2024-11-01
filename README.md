
## .emacs

My emacs configuration.

### Keyboard Shortcuts

| Keys            | Description                                             |
|-----------------|---------------------------------------------------------|
| C-x B           | Switch buffer.                                          |
| C-x C-f         | Find file in file system.                               |
| C-x C-r         | Find recent file.                                       |
| C-x g           | Magit git status.                                       |
| C-x k           | Kill buffer.                                            |
| C-x 2           | Split window down.                                      |
| C-x 3           | Split window right.                                     |
| C-x 0           | Delete current window.                                  |
| C-c k           | Kill current line.                                      |
| C-c l           | LSP commands.                                           |
| C-c j           | Mark all, do what I mean.                               |
| C-c l           | Create multiple cursors at start of selected lines.     |
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
| C-d             | Delete character to the right of cursor.                |
| M-.             | Jump to definition under cursor.                        |
| M-,             | Return from jump to definition.                         |
| M-d             | Delete word to the right of cursor                      |
| M-g g           | Goto line number.                                       |
| S-&lt;up&gt;    | Move line under cursor up.                              |
| S-&lt;down&gt;  | Move line under cursor down.                            |
| S-&lt;left&gt;  | Move selection under cursor left.                       |
| S-&lt;right&gt; | Move selection under cursor right.                      |
| F12             | Re-indent buffer.                                       |

### Install

	mkdir ~/workspace
	cd ~/workspace
	git clone git@github.com:gdonald/emacs-config.git
	cd
	ln -s ~/workspace/emacs-config/.emacs
	emacs

