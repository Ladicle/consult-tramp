# consult-tramp

This package provides `consult-tramp` command to select tramp targets.
Supported completion sources are ssh config, known hosts, and docker
containers.

## Installation

```emacs-lisp
(use-package consult-tramp
  :el-get "Ladicle/consult-tramp")
```

> TODO: If you want to use docker as a completion candidate,
> [docker-tramp](https://github.com/emacs-pe/docker-tramp.el) is required.

## Customization

- consult-tramp-method: (default: `scpx`)
- consult-tramp-ssh-config:  (default: `~/.ssh/config`)
- consult-tramp-enable-shosts: (default: `t`)
- consult-tramp-known-hosts: (default: `~/.ssh/known_hosts`)
- consult-tramp-enable-docker: (default: `t`)
- consult-tramp-path: (default: `~`)
- consult-tramp-docker-path: (default: `/`)
- consult-tramp-sudo-path: (default: `/`)
- consult-tramp-extra-targets: (default: `'()`)
