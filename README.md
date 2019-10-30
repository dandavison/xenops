<p align="center"><img width=512px src="xenops.jpg" alt="" /></p>

Xenops is a LaTeX editing environment for Emacs. Xenops alters the visual appearance of a LaTeX document as displayed by Emacs, but it does not change the LaTeX code that is written to disk. It is an Emacs minor-mode: the major-mode of the document is still your normal Emacs LaTeX editing mode (`latex-mode`).

Xenops is an extension of LaTeX editing features already present in Emacs ([auctex](https://www.gnu.org/software/auctex/) and [org-mode](https://orgmode.org/manual/Previewing-LaTeX-fragments.html)). It does the following:

- LaTeX math content is rendered by an external process and displayed inline as SVG. (Like [preview-latex](https://www.gnu.org/software/auctex/manual/preview-latex.html) but using SVG.)
- When the cursor enters the SVG image, display switches to editable LaTeX code.
- LaTeX math content is automatically re-rendered when the markup changes.
- When viewed as editable LaTeX code, math markup (`\int`, `\alpha`, etc) is replaced by unicode equivalents (This is auctex `prettify-symbols-mode`, with some additional replacements).
- Images (e.g. `\includegraphics`) are displayed inline automatically.
- Images pasted from system clipboard (e.g. screenshots) are written to disk, captured as LaTeX (e.g. `\includegraphics`), and displayed inline.
- Opinionated alterations are made to clean up the visual appearance of common LaTeX markup: (`\begin{*}...\end{*}` blocks, `\section`,  etc).
- `org-babel` code blocks can be executed from within the LaTeX buffer (e.g. within a `verbatim` or `comment` environment).

<sub>Streaked Xenops (_Xenops rutilans_) image by [Dubi Shapiro](https://conservationtours.rockjumperbirding.com/dt_gallery/gallery-tours-brazils-atlantic-rainforest/streaked-xenops-by-dubi-shapiro-001).</sub>
