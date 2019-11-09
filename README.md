<p align="center"><img width=512px src="xenops.jpg" alt="" /></p>

Xenops is a LaTeX editing environment for Emacs. Xenops alters the visual appearance of a LaTeX document as displayed by Emacs, but it does not change the LaTeX code that is written to disk. It is an Emacs minor-mode: the major-mode of the document is still your normal Emacs LaTeX editing mode (`latex-mode`).

Xenops is an extension of LaTeX editing features already present in Emacs ([auctex](https://www.gnu.org/software/auctex/) and [org-mode](https://orgmode.org/manual/Previewing-LaTeX-fragments.html)). It does the following:

- LaTeX math content is rendered by an external process and displayed inline as SVG.<sup>1</sup>
- When the cursor enters a math image, the underlying LaTeX code is revealed for editing, and when the cursor exits that code, the image is regenerated if necessary, and display switches to show the image.
- Similarly, double-click on a math image reveals the underlying code for editing, and a click away from the math re-generates it.
- You can use the mouse to drag an existing math image to a new location, to use it as the starting point for a new one, or you can use `xenops-avy-copy-math` to do this.
- `xenops-avy-goto-math` to jump to math blocks using [avy](https://github.com/abo-abo/avy).
- While LaTeX math code is displayed, math markup is replaced by unicode equivalents<sup>2</sup>
- LaTeX tables are displayed as SVGs, in the same way as LaTeX math content.
- Images (`\includegraphics`) are displayed inline automatically.
- Images pasted from system clipboard (e.g. screenshots) are written to disk, captured as LaTeX (`\includegraphics`), and displayed inline.
- Opinionated alterations are made to clean up the visual appearance of common LaTeX markup: (`\begin{*}...\end{*}` environments, `\section`,  etc).
- Source code in many languages can be executed from within the LaTeX buffer using [org-babel](https://orgmode.org/manual/Working-with-source-code.html).<sup>3</sup>
- SVG bounding boxes and image positioning are set appropriately for inline vs. display math.

<sub>Streaked Xenops (_Xenops rutilans_) image by [Dubi Shapiro](https://conservationtours.rockjumperbirding.com/dt_gallery/gallery-tours-brazils-atlantic-rainforest/streaked-xenops-by-dubi-shapiro-001).</sub>

----------------------------------------------------------------------------------------------------

<sub><sup>1</sup>This is like auctex's [preview-latex](https://www.gnu.org/software/auctex/manual/preview-latex.html), but using SVG.</sub>

<sub><sup>2</sup>This is auctex's `prettify-symbols-mode`, with some additional replacements.</sub>

<sub><sup>3</sup> To execute mathematica code, use something like</sub>
  ```emacs-lisp
  (use-package ob-mathematica
    :load-path "~/path/to/org-mode/contrib/lisp")
  ```
  <sub>Org-mode code blocks can be placed within a `verbatim` or `comment` environment.</sub>
