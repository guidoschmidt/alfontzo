![Alfontzo Logo](logo.png)

# Alfontzo
> Machine-dependant font configuration for Emacs

Alfontzo provides a font configuration per operating system type and machine — e.g. you work on your Laptop and use Fira Code at size 14, but on your desktop you have a display with a higher resolution and thus want to work with Consolas at size 18. Alfontzo provides `M-x alfontzo-fontface` and `M-x alfontzo-typescale` as a fast and convenient way of setting fontface and size on your current machine and stores them in `custem-set.el` to remember it for the next time Emacs is used on that specific machine.


### Initial Idea
I use Emacs across different machines across the day
(Macbook at work with Retina displays, Macbook Air at home,
Linux machine at home with an older 1280px display). On each
machine, I prefer a different font size, because each machine 
has a different screen resolution.


### Usage

*A. Initialization without further customizations:*
```emacs-lisp
(use-package alfontzo
  :ensure t
  :config
  (alfontzo-init))
```

- Use `M-x alfontzo-typescale` to change and store the font-size for the current machine
- Use `M-x alfontzo-fontface` to change and store the font-face for the current machine


*B. Initialization with customizations for operating systems:*
```emacs-lisp
(use-package alfontzo
  :ensure t
  :config
  ;; Set the default font-sizes per operating system type
  (setq alfontzo-os-font-size-map
        `((,alfontzo-os-windows . 13)
          (,alfontzo-os-mac . 14)
          (,alfontzo-os-linux . 11)))
  ;; Set the default font-face per operating system type
  (setq alfontzo-os-font-name-map
        `((,alfontzo-os-windows . "Consolas")
          (,alfontzo-os-mac . "Menlo")
          (,alfontzo-os-linux . "Courier")))
  (alfontzo-init))
```

*C. Initialization with pre-defined configurations for machines:*
```emacs-lisp
(use-package alfontzo
  :ensure t
  :config
  (setq alfontzo-host-font-name-map
        '(("YOUR_FIRST_HOST_NAME_HERE" . "Menlo")
          ("YOUR_SECOND_HOST_NAME_HERE" . "Consolas")))
  (setq alfontzo-host-font-scales-map
        '(("YOUR_FIRST_HOST_NAME_HERE" . 12)
          ("YOUR_FIRST_HOST_NAME_HERE" . 18)))
  (alfontzo-init))
```
