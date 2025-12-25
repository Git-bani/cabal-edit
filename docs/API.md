# API Documentation

## Internal Module Structure

`cabal-edit` is designed with a layered architecture:

### Core Layer (`src/Core/`)

- **Types.hs**: Defines the core data structures (`CabalFile`, `Dependency`, `Section`).
- **Parser.hs**: Handles parsing of `.cabal` files while preserving raw content for exact round-tripping.
- **Serializer.hs**: Manages the serialization and surgical editing of file content (insertions/deletions).
- **DependencyResolver.hs**: Logic for resolving versions against Hackage.
- **ProjectContext.hs**: Handles workspace logic (`cabal.project` parsing).
- **Safety.hs**: Provides file locking and verification mechanisms.

### Business Layer (`src/Business/`)

- **Add.hs**: Implements the logic for adding dependencies.
- **Remove.hs**: Implemented logic for removing dependencies.
- **Upgrade.hs**: Implements dependency upgrade workflows.

### External Layer (`src/External/`)

- **Hackage.hs**: Interface for the Hackage API (fetching package versions).
- **Network.hs**: HTTP client utilities.

### Utils Layer (`src/Utils/`)

- **Config.hs**: Configuration loading and management (JSON).
- **Logging.hs**: Structured logging utilities.

## Data Types

### `CabalFile`

Represents a parsed Cabal file.

```haskell
data CabalFile = CabalFile
  { cfPackageName :: Text
  , cfSections :: [Section]
  , cfRawContent :: Text  -- Original content for format preservation
  }
```

### `Dependency`

Represents a single dependency entry.

```haskell
data Dependency = Dependency
  { depName :: Text
  , depVersionConstraint :: Maybe VersionConstraint
  , depType :: DependencyType
  }
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
