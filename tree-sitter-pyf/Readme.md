# Syntax highlighting with tree-sitter

With this grammar for tree-sitter, you can get PyF syntax highlighting into your
tree-sitter compatible editor, such as nvim or emacs.

![](nvim_ts_highlight.png)
## Installation

### Neovim

You can use this documentation: [NVim documentation for adding parsers](https://github.com/nvim-treesitter/nvim-treesitter#adding-parsers).

I've added the following in my nvim lua configuration file:

```lua
-- Install my own parser
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.pyf = {
  install_info = {
    url = "https://github.com/guibou/PyF",
    files = {"src/parser.c"},
    branch = "main",
    location = "tree-sitter-pyf",
    generate_requires_npm = false, -- if stand-alone parser without npm dependencies
  },
}
```

Then do `:TSInstallFromGrammar pyf` and you are good to go.

Then, install the content of [./vim-plugin](./vim-plugin) as a vim plugin. I'm using [vim-plug](https://github.com/junegunn/vim-plug) to do it:

```vim
Plug 'guibou/PyF', { 'rtp': 'tree-sitter-pyf/vim-plugin' }
```

### Other editors

Ensure tree-sitter support, and then, the `highlight.scm` and `injections.scm`
files are in the [./vim-plugin](./vim-plugin) directory. The parser is located
here, in [./grammar.js](./grammar.js). Please open an issue if you know / want
more informations for specific editors.

## Development

## Building the grammar

Go into this directory, start `nix develop` (Or install `tree-sitter CLI`) and then:

```bash
$ tree-sitter generate
```

You can also experiment with `tree-sitter parse example-file`

```sexp
(source_file [0, 0] - [1, 0]
  (text [0, 0] - [0, 19])
  (escape [0, 19] - [0, 21])
  (text [0, 21] - [0, 27])
  (interpolation [0, 27] - [0, 48]
    (interpolation_content [0, 28] - [0, 47]))
  (text [0, 48] - [0, 49])
  (escape [0, 49] - [0, 51])
  (text [0, 51] - [0, 56])
  (interpolation [0, 56] - [0, 67]
    (interpolation_content [0, 57] - [0, 62])
    (format_string [0, 62] - [0, 66]
      (format_spec [0, 62] - [0, 66]
        (precision_dot [0, 63] - [0, 64])
        (precision [0, 64] - [0, 65])
        (type [0, 65] - [0, 66]))))
  (text [0, 67] - [0, 108])
  (interpolation [0, 108] - [0, 121]
    (interpolation_content [0, 109] - [0, 118])
    (format_string [0, 118] - [0, 120]
      (format_spec [0, 118] - [0, 120]
        (type [0, 119] - [0, 120]))))
  (text [0, 121] - [1, 0]))
```

