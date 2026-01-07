# API Documentation

## Internal Module Structure

`cabal-edit` is designed with a layered architecture:

### Core Layer (`src/Core/`)

- **Types.hs**: Defines the core data structures (`CabalFile`, `Dependency`, `Section`).
- **AST/Types.hs**: Defines the high-fidelity AST representation of a Cabal file.
- **AST/Parser.hs**: Lossless parser that captures comments, whitespace, and line endings into the AST.
- **AST/Serializer.hs**: Converts the AST back to text, guaranteeing byte-for-byte fidelity for unmodified parts.
- **AST/Editor.hs**: Performs high-level manipulations (add/remove/update) directly on the AST.
- **HpackEditor.hs**: Provides surgical editing for `package.yaml` (Hpack) files.
- **DependencyResolver.hs**: Logic for resolving versions against Hackage.
- **ProjectContext.hs**: Handles workspace logic (`cabal.project` parsing).
- **Safety.hs**: Provides atomic file writes and syntax verification mechanisms.

### Business Layer (`src/Business/`)

- **Add.hs**: Implements the logic for adding dependencies.
- **Remove.hs**: Implements logic for removing dependencies.
- **Hpack.hs**: Specialized business logic for Hpack-based projects.
- **Upgrade.hs**: Implements dependency upgrade workflows.
- **SetVersion.hs**: Logic for updating package version.
- **Flag.hs**: Logic for managing Cabal flags.
- **List.hs**: Implements logic for listing dependencies.
- **Validation.hs**: Cabal syntax validation and safety checks.

### External Layer (`src/External/`)

- **Hackage.hs**: Interface for the Hackage API (fetching package versions).
- **Network.hs**: HTTP client utilities.

### Utils Layer (`src/Utils/`)

- **Config.hs**: Configuration loading and management (JSON).
- **Logging.hs**: TTY-aware structured logging utilities.
- **Terminal.hs**: Interactive CLI utilities for item selection.
- **Formatting.hs**: Text formatting helpers.

## Data Types

### `CabalFile` (Legacy Container)

Represents a basic parsed view of a Cabal file. In the new engine, this is primarily used for metadata, while `CabalAST` handles editing.

```haskell
data CabalFile = CabalFile
  { cfPackageName :: PackageName
  , cfSections :: [Section]
  , cfRawContent :: Text
  , cfLineEndings :: Text
  }
```

### `CabalAST` (Primary Editor Data Structure)

A high-fidelity tree structure that captures every detail of the source file, including comments and whitespace.

```haskell
newtype CabalAST = CabalAST { unCabalAST :: [CabalItem] }

data CabalItem
  = FieldItem FieldLine
  | SectionItem SectionLine [CabalItem]
  | IfBlock IfLine [CabalItem] (Maybe (ElseLine, [CabalItem]))
  | CommentItem Text Text
  | EmptyLineItem Text Text
```

### `Dependency`

Represents a single dependency entry.

```haskell
data Dependency = Dependency
  { depName :: PackageName
  , depVersionConstraint :: Maybe VersionConstraint
  , depType :: DependencyType
  }
```

### `Result` (Error Handling)

`cabal-edit` employs an **exception-free architecture**. All functions that can fail return a `Result` type. Callers are guaranteed that the library will not throw runtime exceptions (like `IOException` or custom `Error` exceptions) during normal operation.

```haskell
data Result a
  = Success a
  | Failure Error
```

### `PackageName`

Strongly typed package name with validation. Created via `mkPackageName` (for user input) or `trustedMkPackageName` (for verified internal sources).

```haskell
newtype PackageName = PackageName Text
```

## Configuration

```json
{
  "hackage_url": "https://hackage.haskell.org",
  "cache_expiry_hours": 6,
  "leading_comma": true
}
```
