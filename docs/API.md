# API Documentation

## Internal Module Structure

`cabal-edit` is designed with a layered architecture:

### Core Layer (`src/Core/`)

- **Types.hs**: Defines the core data structures (`CabalFile`, `Dependency`, `Section`).
- **Parser.hs**: Handles parsing of `.cabal` files while preserving raw content for exact round-tripping.
- **Serializer.hs**: Manages the serialization and surgical editing of file content (insertions/deletions).
- **HpackEditor.hs**: Provides surgical editing for `package.yaml` (Hpack) files.
- **DependencyResolver.hs**: Logic for resolving versions against Hackage.
- **ProjectContext.hs**: Handles workspace logic (`cabal.project` parsing).
- **Safety.hs**: Provides file locking and verification mechanisms.

### Business Layer (`src/Business/`)

- **Add.hs**: Implements the logic for adding dependencies.
- **Remove.hs**: Implements logic for removing dependencies.
- **Hpack.hs**: Specialized business logic for Hpack-based projects.
- **Upgrade.hs**: Implements dependency upgrade workflows.
- **SetVersion.hs**: Logic for updating package version.
- **Flag.hs**: Logic for managing Cabal flags.
- **Validation.hs**: Cabal syntax validation and safety checks.

### External Layer (`src/External/`)

- **Hackage.hs**: Interface for the Hackage API (fetching package versions).
- **Network.hs**: HTTP client utilities.

### Utils Layer (`src/Utils/`)

- **Config.hs**: Configuration loading and management (JSON).
- **Logging.hs**: TTY-aware structured logging utilities.
- **Formatting.hs**: Text formatting helpers.

## Data Types

### `CabalFile`

Represents a parsed Cabal file with metadata for format preservation.

```haskell
data CabalFile = CabalFile
  { cfPackageName :: PackageName
  , cfSections :: [Section]
  , cfRawContent :: Text
  , cfLineEndings :: Text -- "\n" or "\r\n"
  }
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

### `PackageName`

Strongly typed package name with validation.

```haskell
newtype PackageName = PackageName Text
```
## Configuration

Configuration is stored in `~/.cabal-edit/config.json`.

```json
{
  "hackage_url": "https://hackage.haskell.org",
  "cache_expiry_hours": 6,
  "leading_comma": true
}
```
