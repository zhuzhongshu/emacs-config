(TeX-add-style-hook
 "org-setup"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ctex" "UTF8" "heading=true") ("geometry" "top=1in" "bottom=1in" "left=1.25in" "right=1.25in") ("inputenc" "utf8") ("ulem" "normalem") ("hyperref" "unicode=true" "colorlinks=no" "pdfborder=no")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "ctex"
    "fontspec"
    "geometry"
    "minted"
    "inputenc"
    "xcolor"
    "bm"
    "fixltx2e"
    "graphicx"
    "longtable"
    "float"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "marvosym"
    "wasysym"
    "amssymb"
    "hyperref")
   (TeX-add-symbols
    "song"
    "hei"
    "kai"
    "li"
    "you")
   (LaTeX-add-xcolor-definecolors
    "bg"))
 :latex)

