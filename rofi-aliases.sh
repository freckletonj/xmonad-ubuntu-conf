cat ~/.bashrc | sed -r -n  "s/^ *alias +([a-z0-9]+)\=.+$/\1/p"
