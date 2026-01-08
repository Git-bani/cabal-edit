# cabal-edit Tutorial

## Introduction

`cabal-edit` allows you to manage your Haskell project dependencies directly from the command line, similar to `cargo-edit` for Rust or `npm` for Node.js.

## Installation

```bash
cabal install cabal-edit
```

## Basic Usage

### 1. Adding a Dependency

To add a library to your project:

```bash
cabal-edit add aeson
```

This will:
1. Fetch the latest version of `aeson` from Hackage.
2. Add it to the `build-depends` section of your `.cabal` file.
3. Preserve your existing formatting and comments.

**Interactive Search:**

If you are unsure of the exact package name, or want to explore Hackage, use the interactive mode:

```bash
cabal-edit add -i json
```

This searches Hackage for "json" and presents a selectable list of matching packages.

**Renaming Dependencies (Mixins):**

You can use Cabal Mixins to rename modules or hide them to avoid conflicts. Use the `--mixin` flag to specify the mixin clause.

```bash
# Adds 'aeson' and renames 'Data.Aeson' to 'JSON'
cabal-edit add aeson --mixin "(Data.Aeson as JSON)"

# Adds 'base' but hides 'Prelude' to avoid conflicts with custom preludes
cabal-edit add base --mixin "hiding (Prelude)"
```

**Adding with constraints:**

```bash
cabal-edit add text --version "^>= 1.2"
```

**Adding to a specific section:**

You can target specific components like executables, test suites, or benchmarks. Use the `--section` flag with the component type and name:

```bash
# Add to an executable
cabal-edit add lens --section exe:my-app

# Add to a test suite
cabal-edit add hspec --section test:unit-tests

# Add to a benchmark
cabal-edit add criterion --section bench:performance
```

**Adding to conditional blocks:**

You can add a dependency directly to an `if` block using the `--if` flag:

```bash
cabal-edit add Win32 --if "os(windows)"
```

Alternatively, use the `--flag` shorthand to add a dependency conditional on a specific Cabal flag:

```bash
cabal-edit add lens --flag use-lens
```

If the conditional block doesn't exist within the targeted section, `cabal-edit` will create it for you at the end of that section.

### 2. Removing a Dependency

To remove a library:

```bash
cabal-edit rm old-library
```

**Interactive Removal:**

To visually select which dependencies to remove from a list:

```bash
cabal-edit rm -i
```

### 3. Listing Dependencies

You can view all dependencies in your project, grouped by section (Library, Executables, etc.):

```bash
cabal-edit list
```

This will output a formatted list of all packages and their version constraints.

### 4. Version Management

You can update the project version field at the top of your `.cabal` file:

```bash
cabal-edit set-version 1.2.3.4
```

### 5. Upgrading Dependencies

To upgrade all dependencies to their latest versions on Hackage:

```bash
cabal-edit upgrade
```

### Interactive Upgrade

Selective upgrades are supported via the interactive mode:

```bash
cabal-edit upgrade -i
```

## Advanced Features

### Managing Cabal Flags

`cabal-edit` provides a dashboard and commands for managing flags in your `.cabal` file:

### Flag Dashboard

Open a visual dashboard to toggle existing flags:

```bash
cabal-edit flag -i
```

This allows you to quickly switch flag defaults between `True` and `False` without hunting through the file.

### CLI Flag Operations

```bash
# Add a new flag (default: False, manual: True)
cabal-edit flag add my-feature

# Enable an existing flag (sets default: True)
cabal-edit flag enable my-feature

# Disable an existing flag (sets default: False)
cabal-edit flag disable my-feature

# Remove a flag stanza completely
cabal-edit flag remove my-feature
```

### Dry Runs & Safety

**Dry Runs:**
Preview changes with **colorized diffs** without writing to disk by using the `--dry-run` flag. This is supported by all modifying commands (`add`, `rm`, `upgrade`, `set-version`, `flag`).

```bash
cabal-edit add lens --dry-run
```

The output will show a `git diff` style view, with removed lines in Red and added lines in Green.

**Surgical Edits:**
Unlike tools that re-format the entire file (often losing comments or custom alignment), `cabal-edit` uses a **high-fidelity AST engine**. It parses your `.cabal` file into a lossless structure that captures every comment, empty line, and indentation choice. When you add or remove a dependency, `cabal-edit` modifies only the relevant nodes in the AST and serializes it back, guaranteeing byte-for-byte fidelity for all unmodified parts.

**Atomic Writes:**
To prevent file corruption during power failures or crashes, `cabal-edit` writes to a temporary file first and then atomically moves it to the final destination.

### Hpack Support

If a `package.yaml` file is present, `cabal-edit` will automatically prioritize it. Commands like `add` and `rm` will modify your `package.yaml` instead of the `.cabal` file, ensuring that your changes aren't lost the next time Hpack generates the Cabal file.

## Workspace Support

Manage multiple packages in a `cabal.project` workspace:

```bash
# Add a dependency to ALL packages in the workspace
cabal-edit --workspace add lens

# Target specific packages within the workspace
cabal-edit --package my-lib --package my-app upgrade
```


## Configuration

You can customize `cabal-edit` via `~/.cabal-edit/config.json`.

```json
{
  "hackage_url": "https://hackage.haskell.org",
  "leading_comma": true
}
```

## Running Evaluation Benchmarks

For researchers and developers working on automated agents, `cabal-edit` supports evaluation benchmarks.

### Prerequisites
- Python 3.10+
- OpenRouter API Key

### Execution
Run the benchmarking suite via the research proxy:

```bash
# Set your OpenRouter key
export OPENROUTER_API_KEY="your_key"

# Run SimpleQA and BrowseComp
python -m local_deep_research.benchmarks --task simpleqa
python -m local_deep_research.benchmarks --task browsecomp
```

Results are stored in `./benchmarks/results/` for further analysis.