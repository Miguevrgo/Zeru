# Neovim Setup for Zeru Language

This guide explains how to set up syntax highlighting, indentation, and text objects for Zeru files in Neovim.

## Prerequisites

- Neovim 0.9+ with tree-sitter support
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) plugin

## Quick Setup

### 1. Add Parser Configuration

Add this to your Neovim config (e.g., `~/.config/nvim/lua/plugins/treesitter.lua`):

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

parser_config.zeru = {
  install_info = {
    url = "https://github.com/miguevr/Zeru",
    files = { "tree-sitter-zeru/src/parser.c" },
    branch = "main",
    location = "tree-sitter-zeru",
    generate_requires_npm = false,
    requires_generate_from_grammar = false,
  },
  filetype = "zeru",
}
```

### 2. Register Filetype

```lua
vim.filetype.add({
  extension = {
    zr = "zeru",
  },
})
```

### 3. Copy Query Files

```bash
# Create the queries directory
mkdir -p ~/.config/nvim/queries/zeru

# Copy query files (from the Zeru repository)
cp tree-sitter-zeru/queries/*.scm ~/.config/nvim/queries/zeru/
```

### 4. Install the Parser

Open Neovim and run:

```vim
:TSInstall zeru
```

Or use the Lua API:

```lua
require("nvim-treesitter.install").ensure_installed({ "zeru" })
```

## Local Development Setup

If you're developing locally:

```bash
cd ~/GitHub/Zeru/tree-sitter-zeru

# Generate the parser
npm install
npm run generate

# Link queries to Neovim
ln -sf $(pwd)/queries ~/.config/nvim/queries/zeru
```

## Manual Installation (without nvim-treesitter plugin)

```lua
-- In your init.lua
vim.treesitter.language.register('zeru', 'zeru')

-- Set up the parser (compile it first with tree-sitter generate)
local parser_path = vim.fn.expand("~/GitHub/Zeru/tree-sitter-zeru")
vim.opt.runtimepath:append(parser_path)
```

## Features

Once installed, you'll have:

- ✅ **Syntax Highlighting** - Full syntax coloring for all Zeru constructs
- ✅ **Indentation** - Smart auto-indentation
- ✅ **Folding** - Code folding based on syntax
- ✅ **Text Objects** - `af`/`if` for functions, `ac`/`ic` for classes, etc.

## Verification

Open a `.zr` file and run:

```vim
:InspectTree
```

You should see the syntax tree for your Zeru code.

## Troubleshooting

### Parser not found

Make sure the parser is compiled:

```bash
cd tree-sitter-zeru
npm run generate
```

### Highlighting not working

Check that queries are in the right place:

```bash
ls ~/.config/nvim/queries/zeru/
# Should show: highlights.scm, locals.scm, indents.scm, etc.
```

### Wrong filetype

Verify filetype detection:

```vim
:set ft?
" Should show: filetype=zeru
```
