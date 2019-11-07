# emacs-gcloud-mode

Display current gcloud project in Emacs mode line

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-gcloud-mode")
(require 'gcloud-mode)
```

Enable it with `M-x gcloud-mode` or `(gcloud-mode 1)`

The mode line will be updated every 10 seconds or what is defined in
`gcloud-mode-mode-line-update-interval`. 

You can also trigger the update with `emacsclient` as a `PROMPT_COMMAND`:

```bash
export PROMPT_COMMAND="emacsclient -e '(gcloud-mode-mode-line-update)' &>/dev/null"
```
