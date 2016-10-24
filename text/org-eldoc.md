# org-eldoc #

This is org-eldoc, an eldoc plugin for org-mode.

Eldoc is a minor-mode that displays documentation for `thing-at-point' in minibuffer.

Org-eldoc shows:

* outline path (breadcrumbs)
* src block parameters
* documentation for *lang* when in src block body

### How does it look like? ###

#### On header line ####
![breadcrumb.png](https://bitbucket.org/repo/XM4XXr/images/3965780426-breadcrumb.png)

#### On src block header ####
![src_header.png](https://bitbucket.org/repo/XM4XXr/images/4292527493-src_header.png)

#### In src block body
![inside_src.png](https://bitbucket.org/repo/XM4XXr/images/760124667-inside_src.png)


### Installation ###
org-eldoc is part of org-mode contrib repository

### How do I get set up? ###

Add to your ~/.emacs
```
#!emacs-lisp
  (org-eldoc-hook-setup)
```
To have it auto start with org-mode.