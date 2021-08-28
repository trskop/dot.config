# yx-paper.addurl

> Add a paper into a git-annex repository for publicly available papers.

- Add a paper into papers repository under a specific name {{file-name.pdf}}:

`yx paper.addurl --file='{{file-name.pdf}}' {{https://example.com/some/paper.pdf}}`

- Configuration file where papers repository is configured:

`${XDG_CONFIG_HOME:-${HOME}/.config}/yx/yx-paper.addurl.dhall`
