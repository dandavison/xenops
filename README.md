<table style="width:100%">
  <tr>
    <td>
      <img width=300px src="etc/img/demo-1.gif" alt="demo" />
    </td>
    <td>
      <img width=512px src="etc/img/xenops.jpg" alt="Xenops rutilans" />
    </td>
  </tr>
</table>

<p align="right">
  <a href="https://travis-ci.com/dandavison/xenops">
    <img src="https://travis-ci.com/dandavison/xenops.svg?token=CX7zhABhKYrVPxKn4rWz&branch=master" alt="Build Status" />
  </a>
</p>

   * [Overview](#overview)
   * [Getting started](#getting-started)
   * [How to use Xenops](#how-to-use-xenops)
      * [Rendering math, tables and TikZ content](#rendering-math-tables-and-tikz-content)
      * [Rendering all the content in the document](#rendering-all-the-content-in-the-document)
      * [Executing code](#executing-code)
      * [Working with images](#working-with-images)
      * [Xen mode](#xen-mode)
   * [Command and variable reference](#command-and-variable-reference)
   * [Misc](#misc)
   * [Documentation TODOs](#documentation-todos)
   * [Contributing](#contributing)
   * [Internals](#internals)
   * [Credit](#credit)

# Overview

Xenops is a LaTeX editing environment for mathematical documents in Emacs.

Installation instructions are below. To use Xenops, open a `.tex` file in Emacs as usual, and do `M-x xenops-mode`. The key features are:

- **LaTeX math, tables, and TikZ content are automatically displayed as SVG images in the Emacs buffer.**

  You don't need to do anything: rendering is triggered automatically. The result is that you can work on the document in Emacs without needing to frequently check the appearance of the PDF. Rendering is asynchronous, so it doesn't interrupt your writing. One of the motivations for Xenops is that LaTeX math looks beautiful in Emacs when rendered as SVG.

- **A LaTeX file edited with Xenops is just a normal `.tex` file.**

  Other people collaborating on the same document do not have to use Xenops (or Emacs), and using Xenops does not involve adding non-LaTeX content to your `.tex` files.

- **Source code in `minted` blocks can be executed, and is syntax-highlighted.**

  Mathematica and SymPy are both capable of returning symbolic expressions as LaTeX code. This means that you can perform a symbolic calculation in a Mathematica or SymPy `minted` block, and Xenops will render the symbolic expression that results as an image of LaTeX-typeset traditional math notation.

- **Xenops provides an optional decluttered view of your document.**

  This view can be toggled on/off with `M-x xenops-xen-mode`. It works by hiding common LaTeX markup, and restyling certain document elements.

- **Images on disk are displayed in the Emacs buffer, and screenshots can be captured from the system clipboard.**

  So, for example, `\includegraphics{myfile.png}` will be displayed as an image, and if you capture a screenshot to the system clipboard, then paste (e.g. `C-y`) will prompt for a file name, save the image to disk, insert the `\includegraphics` element, and display the image.



When using Xenops, you can continue to use [auctex](https://www.gnu.org/software/auctex/): Xenops replaces the `preview-latex` functionality in auctex.

Xenops can also be used with [org-mode](https://orgmode.org) documents that contain LaTeX fragments.


# Getting started


1. **Ensure that you have [LaTeX](https://www.latex-project.org/get) installed on your machine.**

    The commands `which latex` and `which dvisvgm` must both return paths to the executables. `dvisvgm` should be present as part of your LaTeX installation, but it's also available [here](https://dvisvgm.de/Downloads).

1. **Ensure that your Emacs version is at least Emacs 26.**

    If you are using MacOS, install emacs from homebrew using either the `emacs-plus` or `emacs-mac` packages, since they provide the required SVG support.

    Xenops can only display images if you run Emacs as a GUI application, not as a terminal application.

1. **Clone this repository.**

    (Xenops will be submitted to MELPA soon.)

1.  **Load Xenops in your Emacs config file.**
    ```emacs-lisp
    (add-to-list 'load-path "/path/to/xenops/repo")
    (require 'xenops)
    (add-hook 'latex-mode-hook #'xenops-mode)
    (add-hook 'LaTeX-mode-hook #'xenops-mode)
    ```

1. **`M-x xenops-doctor`**

    This will check all the requirements listed above and some others. You should see this:
    <img width=250px src="https://user-images.githubusercontent.com/52205/76026875-3b1d5d00-5ef5-11ea-9eeb-9df0fcadf6ab.png" alt="image" /><br>
    If so, move on to the next section, `"How to use Xenops"`. If not, the `xenops-doctor` output will explain what is wrong.


# How to use Xenops

You don't need to learn any commands for Xenops to render your math: you just type, and Xenops will render it. The animation at the top of this README gives an idea of what this looks like (refresh the page to make it play again).

More explicitly, here are the basic steps for working with math in Xenops:

  1. **Enter some LaTeX math code, using any of the standard delimiters:**<br>`$...$`, `\(...\)`, `\begin{equation|align|table|tabular|tikzpicture}`.

  2. **Move the cursor out of the block: an image of the math will appear.**

  3. **Move the cursor over the image and hit Enter: the image will disappear and you can edit the LaTeX code.**

You may also want to create the image for all math/table/TikZ elements in the document. To do that, position the cursor outside any element and issue `xenops-dwim`.

The other command to know from the start is `xenops-doctor`: run this if something is not working.

The following sections give more detailed instructions and examples.

## Rendering math, tables and TikZ content

This section gives step-by-step instructions and shows what to do if it is not working.

1. **Open a file containing LaTeX math content:**

    ```latex
    \documentclass{article}
    \usepackage{amsmath}
    \begin{document}
    An example equation is
    \begin{equation*}
    \frac{\partial \mathcal{L}}{\partial q} - \frac{d}{dt}\frac{\partial \mathcal{L}}{\partial \dot{q}} = 0.
    \end{equation*}
    The document continues.
    \end{document}
    ```

    Xenops recognizes the usual delimiters: `$...$`, `\(...\)`, `\begin{equation|align|table|tabular|tikzpicture}`.

2. **Move the cursor into a math element:**

    <img width=800px src="https://user-images.githubusercontent.com/52205/76102955-6c9a3500-5f96-11ea-9ef8-e6eae94b271a.png" alt="image" />

3. **Move the cursor out of that element. The content in the block will shortly be replaced by an image:**

    <img width=200px src="https://user-images.githubusercontent.com/52205/76103007-8a679a00-5f96-11ea-8b31-56d1343b4477.png" alt="image" />

4. **To remove the image and continue editing, click on it or move the cursor over the image and hit Enter.**

5. **What if something went wrong?**

    There are a few things that could go wrong. For example, Emacs might not be able to find the `latex` executable on your machine, or `latex` might fail due to errors in your code.

    If something went wrong, a yellow warning triangle will appear at the beginning of the math block:
    <img width=750px src="https://user-images.githubusercontent.com/52205/76103232-f518d580-5f96-11ea-9626-b7d788032bb9.png" alt="image" />

    To investigate what the problem is, you can hover the mouse over the triangle, or right-click on the triangle to bring up a menu. Here's what you might see if you hover the mouse over the warning triangle:
    <img width=750px src="https://user-images.githubusercontent.com/52205/76103282-09f56900-5f97-11ea-8f3a-882ef6afb5e5.png" alt="image" />

    So that's fairly clear: either `latex` is not installed, or Emacs is not finding the executable. Xenops has another troubleshooting tool which it would make sense to use at this point: `M-x xenops-doctor`. Here's what is displayed when we run that:

    <img width=400px src="https://user-images.githubusercontent.com/52205/76025786-3b1c5d80-5ef3-11ea-9739-17e7c3a15377.png" alt="image" />

    Alternatively, right-click on the warning triangle brings up a menu:
    <img width=750px src="https://user-images.githubusercontent.com/52205/76103900-1928e680-5f98-11ea-9b84-505e157f0481.png" alt="image" /><br>
    Select `"View failing command output"` to see the errors from the `latex` process, or select `"Copy failing command"` in order to debug the problem from the command-line.


## Rendering all the content in the document

Place the cursor anywhere in the document, but not in a math/table/TikZ element, and issue `M-x xenops-dwim`.

This will kick off one asynchronous processing task for every renderable element in the buffer whose image is not already cached. You can carry on editing the document while these are being processed. They will gradually complete, and the images will be inserted in the buffer.

Alternatively, you can select a region, and `xenops-dwim` will act on just the elements in that region.

## Executing code

Emacs provides sophisticated facilities for executing blocks of code written in many different languages: see [org-babel](https://orgmode.org/manual/Working-with-source-code.html).

Xenops makes use of this to allow [minted](https://github.com/gpoore/minted) blocks in LaTeX buffers to be executed. Place point in a minted code block and issue `M-x xenops-dwim`:<br>
<img width=300px src="https://user-images.githubusercontent.com/52205/76038047-3e711280-5f0e-11ea-8307-963341af1f35.png" alt="image" /><br>
(Use `(require 'ob-python)` to enable python execution.)

There are many possibilities with documents that are a hybrid of code, code results, and traditional content: please see the [org-babel](https://orgmode.org/manual/Working-with-source-code.html) documentation.
<br><br>
With Xenops, we can use this to check calculations done by hand against the output of a symbolic algebra package.

Both SymPy and Mathematica can return their results to the Emacs buffer as LaTeX code, and Xenops will render this immediately as an image. The result is that it feels as if Sympy/Mathematica are returning their results as an image, typeset in traditional mathematical notation, which can be helpful for a quick check:

```latex
The derivative is
\begin{align*}
  \frac{\dif}{\dif x} \frac{y'}{\sqrt{1 + y'^2}}
  &= \frac{y''\sqrt{1 + y'^2} - y'\frac{1}{2}\frac{1}{\sqrt{1 + y'^2}}2y'y''}{1 + y'^2} \\
  &= \frac{y''}{\sqrt{1 + y'^2}} - \frac{y'^2y''}{(1 + y'^2)^{3/2}}.
\end{align*}

(Check this in Mathematica):
\begin{minted}{wolfram} :results latex
D[y'[x]/Sqrt[1 + y'[x]^2], x]
\end{minted}
```

`M-x xenops-dwim` on that minted block yields:

<img width=700px src="https://user-images.githubusercontent.com/52205/76138564-27feb000-600f-11ea-849c-bad5b79e77cd.png" alt="image" />


## Working with images

The size of images displayed in the buffer can be changed with `xenops-increase-size` and `xenops-decrease-size`.

Xenops recognizes the `\includegraphics` command, and these images will be displayed by `xenops-render`. If you capture a screenshot to your system clipboard and then paste (e.g. `C-y`) into the LaTeX buffer, Xenops will notice that you're pasting image data, prompt for a file to save it in, and insert the `\includegraphics` link (implemented in MacOS only currently, see issue [#1](https://github.com/dandavison/xenops/issues/1)):

```latex
Some facts:
\begin{mdframed}
  \includegraphics[width=400pt]{xenops.png}
\end{mdframed}
The document continues.
```

<img
  width="300px"
  src="https://user-images.githubusercontent.com/52205/76043722-fbb73680-5f1d-11ea-85ad-8135f7cf8aed.png"
  alt="image"
/>



## Xen mode

The command `xenops-xen-mode` toggles an alternative cleaner view that hides common LaTeX markup and applies some visual styling. These are changes in appearance only -- the actual text content of the document is never changed and hitting save after turning on Xen mode will not cause any new changes to be written to disk.

In the example below, Xen-mode has made the following changes to the visual appearance of the document:

- `\begin{align}`, `\end{align}`, `\begin{minted}`, `\end{minted}` have been replaced with a unicode character (⚡).
- `\section` markup has been hidden and the section heading has been styled and indented according to its level.
- Some LaTeX mathematical syntax has been replaced by unicode characters.

<table>
  <tr>
    <td>
      <b>Default view</b>
    </td>
    <td>
      <img
        width="500px"
        src="https://user-images.githubusercontent.com/52205/76045031-a67d2400-5f21-11ea-8a06-f4fa320c72a6.png"
        alt="image"
      />
    </td>
    <td>
      <img
        width="360px"
        src="https://user-images.githubusercontent.com/52205/76056086-3b404b80-5f3b-11ea-86b4-91db7623b4d5.png"
        alt="image"
      />
    </td>
  </tr>
  <tr>
    <td>
      <b>Xen mode</b>
    </td>
    <td>
      <img
        width="480px"
        src="https://user-images.githubusercontent.com/52205/76044986-8e0d0980-5f21-11ea-9d3a-5def0804936a.png"
        alt="image"
      />
    </td>
    <td>
      <img
        width="360px"
        src="https://user-images.githubusercontent.com/52205/76045199-04aa0700-5f22-11ea-8a09-245e8c07a0e3.png"
        alt="image"
      />
    </td>
  </tr>
</table>


# Command and variable reference

For commands that operate on one or more elements the rule is:
- If there is an element at the current cursopr position, operate on that element.
- If there is an active selection, operate on elements in the selected region.
- Otherwise, operate on all elements in the buffer.



| Command                             |                                                                                                                                         | Default<br>keybinding |
|-------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|-----------------------|
| `xenops-dwim`                       | Render, Reveal, Regenerate, or Execute element(s).<br>(_do-what-i-mean_)                                                                | `C-c !`               |
|                                     |                                                                                                                                         |                       |
| `xenops-render`                     | Render element(s) as an image.                                                                                                          |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-reveal`                     | Remove image and reveal element(s) for editing.                                                                                         |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-regenerate`                 | Regenerate image for element(s),<br>refusing to use a cached version.                                                                   |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-execute`                    | Execute code block.                                                                                                                     |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-doctor`                     | Check your Emacs environment for any problems<br>that will prevent Xenops from working correctly.                                       |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-show-waiting-tasks`         | Display a count of latex background processing tasks<br>that are waiting in the queue                                                   |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-cancel-waiting-tasks`       | Cancel all latex background processing tasks<br>that are waiting in the queue                                                           |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-increase-size`              | Increase size of image(s).                                                                                                              |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-decrease-size`              | Decrease size of image(s).                                                                                                              |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-xen-mode`                   | Toggle Xen mode on/off.                                                                                                                 | `C-c /`               |
|                                     |                                                                                                                                         |                       |
| `xenops-select-font-family`         | Select a font for the Xenops buffer.                                                                                                    |                       |
|                                     |                                                                                                                                         |                       |
| `<copy>`                            | If the cursor is on a rendered element, copy the entire element.                                                                        | `C-w` etc             |
|                                     |                                                                                                                                         |                       |
| `<paste>`                           | If the clipboard contains an image, write the image<br>to disk and insert an `\includegraphics` link.                                   | `C-y` etc             |
|                                     |                                                                                                                                         |                       |
| `xenops-clear-latex-preamble-cache` | Force Xenops to recreate the latex preamble for the<br>current document. This is necessary if you have added e.g. a `\usepackage` line. |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-avy-goto-math`              | Use [avy](https://github.com/abo-abo/avy) to jump to a math element.                                                                    |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-avy-copy-math-and-paste`    | Use [avy](https://github.com/abo-abo/avy) to copy a math element from elsewhere<br>and paste it at point.                               |                       |
|                                     |                                                                                                                                         |                       |
| `xenops-math-concatenate`           | Concatenate contiguous math blocks in the active region.                                                                                |                       |


In addition, the keybinding `C-c ,` is bound to a keymap behind which all Xenops commands are available.

<br>
<br>

| Variable                                |                                                                                                                            | Default value            |
|-----------------------------------------|----------------------------------------------------------------------------------------------------------------------------|--------------------------|
| `xenops-cache-directory`                | The location of the directory in which Xenops<br>saves SVG images.                                                         | ~/.emacs.d/xenops-cache/ |
|                                         |                                                                                                                            |                          |
| `xenops-math-latex-max-tasks-in-flight` | The maximum number of latex processing tasks<br>that are permitted to be simultaneously active.                            | 32                       |
|                                         |                                                                                                                            |                          |
| `xenops-font-family`                    | The font family to use in Xenops buffers.<br>(You can choose this interactively using<br>`M-x xenops-select-font-family`.) | nil                      |
|                                         |                                                                                                                            |                          |
| `xenops-tooltip-delay`                  | The time delay in seconds before displaying<br>a tooltip on mouseover.                                                     | 0.2                      |
|                                         |                                                                                                                            |                          |



# Misc

- Xenops caches the SVG images. If it notices that it already has the image for some LaTeX math/table/TikZ content, then it will not re-run `latex`. The cache location on disk is determined by the variable `xenops-cache-directory`.

- Xenops understands the auctex variable [`TeX-master`](https://www.gnu.org/software/auctex/manual/auctex/Multifile.html). This means that you can work on multi-file LaTeX projects with Xenops: as long as `TeX-master` is set correctly, then Xenops will include the necessary LaTeX packages and preamble when it is generating images for math/table/TikZ fragments. Please see the [auctex documentation](https://www.gnu.org/software/auctex/manual/auctex/Multifile.html).

- If you are encountering any problems, the first thing to try is `M-x xenops-doctor`. Beyond that, please don't hesitate to open Github issues!


# Documentation TODOs

- Generating graphics from minted blocks
- SymPy examples
- Relation to preview-latex in auctex
- Relation to org-mode

# Contributing

Xenops can be used profitably for serious work! However, there are still bugs and missing features. Please don't hesitate to get in touch and/or open Github issues. And if you know some emacs-lisp, your help would be very welcome. Please get in touch (dandavison7@gmail.com) and/or jump on the Github issues.

# Internals

This section describes how Xenops is implemented and is most relevant to people who might be interested in improving Xenops.

The main user actions in Xenops are:

- `xenops-render`
- `xenops-reveal`
- `xenops-regenerate`
- `xenops-execute`

Those four verbs are examples of *operations*. The `xenops-ops` data structure defines the set of Xenops operations.

Operations are done on *elements*. An element is a special substring of the buffer such as a math block, a table, a minted code block, a footnote, an `\includegraphics` link, etc. The set of Xenops element types is defined by the `xenops-elements` data structure. An element string is parsed into a plist data structure, that we also refer to as an element. The `:type` key of the plist holds the type of the element (`'block-math`, `'table`, `'minted`, `footnote`, `'image`, etc).

The organizing principle of Xenops is that a user action consists of applying an operation to a set of elements. The set of elements is determined by the context under which the action was invoked: either a single element at point, or all elements in the active region, or all elements in the buffer.

Xenops carries out a user action as follows:

1. Identify the set of *handlers* corresponding to the operation. A handler is a function that takes an element plist as its first argument. The mapping from operations to handlers is defined in the `xenops-ops` data structure.

2. Visit each element in sequence:

   2.1 At an element, select a single handler which is valid for that element type. (There will usually be only one choice.) The mapping from element types to valid handlers is defined in the `xenops-elements` data structure.

   2.2 Call the selected handler on the element.

This traversal and dispatch-to-handler logic is implemented in [xenops-apply.el](lisp/xenops-apply.el).

The handler for the `render` operation on elements of type `'block-math`, `'inline-math`, and `'table` involves calling external latex and dvisvgm processes to generate an SVG image. This is done asynchronously, using [emacs-aio](https://github.com/skeeto/emacs-aio).


# Credit

- [auctex](https://www.gnu.org/software/auctex/)
- [emacs-aio](https://github.com/skeeto/emacs-aio)
- [org-mode](https://orgmode.org/)


<sub>Streaked Xenops (_Xenops rutilans_) image by [Dubi Shapiro](https://conservationtours.rockjumperbirding.com/dt_gallery/gallery-tours-brazils-atlantic-rainforest/streaked-xenops-by-dubi-shapiro-001).</sub>
