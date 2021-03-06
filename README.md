# emacs-gcloud-mode

Global minor-mode for changing gcloud project and display current project in Emacs mode line

## Install

Clone this repository and add to `load-path`:
```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-gcloud-mode")
(require 'gcloud-mode)
(gcloud-mode 1)
```

### Keybindings

| Keys        | Description               |
| ---------   | ------------------------- |
| `C-c C-g p` | Set gcloud project        |

### Mode line

The variable `gcloud-mode-line-string-format` defines what to display in the mode line, and
mode line will be updated every 10 seconds or what is defined in
`gcloud-mode-line-update-interval`.

You can also trigger the update with `emacsclient` as a `PROMPT_COMMAND`:

```bash
export PROMPT_COMMAND="emacsclient -e '(gcloud-mode-line-update)' &>/dev/null"
```
