# yx-xpdf

> Simple wrapper for a PDF viewer with xpdf-compatible command-line interface.

- Open a PDF file:

`yx xpdf {{path/to/file.pdf}}`

- Open a specific page in a PDF file:

`yx xpdf {{path/to/file.pdf}} :{{page_number}}`

- Open a PDF file in full-screen mode:

`yx xpdf {{--fullscreen|-fullscreen}} {{path/to/file.pdf}}`

- Use textual viewer to open a PDF file:

`DISPLAY= yx xpdf {{path/to/file.pdf}}`
