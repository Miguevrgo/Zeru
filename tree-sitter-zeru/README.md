# Tree-sitter Grammar for Zeru

A [tree-sitter](https://tree-sitter.github.io/) grammar for the [Zeru](https://github.com/miguevr/Zeru) programming language.

## Features

- Full syntax highlighting for all Zeru constructs
- Support for generics, traits, enums, and structs
- Optional (`T?`) and Result (`T!`) type syntax
- Module imports and scoped identifiers
- For-in loops, match expressions, and more

## Installation

### Neovim (with nvim-treesitter)

1. Add the parser to your nvim-treesitter config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.zeru = {
  install_info = {
    url = "https://github.com/miguevr/Zeru",
    files = { "tree-sitter-zeru/src/parser.c" },
    branch = "main",
    location = "tree-sitter-zeru",
  },
  filetype = "zeru",
}

vim.filetype.add({
  extension = {
    zr = "zeru",
  },
})
```

2. Copy queries to your Neovim config:

```bash
mkdir -p ~/.config/nvim/queries/zeru
cp queries/*.scm ~/.config/nvim/queries/zeru/
```

3. Install the parser:

```vim
:TSInstall zeru
```

### Manual Installation (Neovim)

```bash
cd tree-sitter-zeru
npm install
npm run generate
```

Then copy the generated files:
- `src/parser.c` → your tree-sitter parsers directory
- `queries/*.scm` → `~/.config/nvim/queries/zeru/`

### VSCode

Coming soon! For now, use the TextMate grammar in the `vscode-zeru` extension.

## Development

```bash
# Install dependencies
npm install

# Generate the parser
npm run generate

# Run tests
npm test

# Parse a file
npm run parse -- path/to/file.zr
```

## Supported Syntax

| Feature | Status |
|---------|--------|
| Functions | ✅ |
| Structs | ✅ |
| Enums | ✅ |
| Traits | ✅ |
| Generics | ✅ |
| Imports | ✅ |
| Pointers | ✅ |
| Optional types (`T?`) | ✅ |
| Result types (`T!`) | ✅ |
| Match expressions | ✅ |
| For-in loops | ✅ |
| Arrays & Slices | ✅ |
| Tuples | ✅ |
| Comments | ✅ |

## License

MIT
