# Contributing to cabal-edit

We welcome contributions to `cabal-edit`! Please follow these guidelines to ensure a smooth process.

## Getting Started

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/cabal-edit.git
   cd cabal-edit
   ```

2. **Build the project**:
   ```bash
   cabal build
   ```

3. **Run tests**:
   ```bash
   cabal test
   ```

## Development Workflow

1. Create a new branch for your feature or bugfix.
2. Implement your changes.
3. Add tests for your changes. We use `hspec` for unit and integration testing.
4. Ensure all tests pass.
5. Format your code (try to match existing style).
6. Submit a Pull Request.

## Code Style

- Use 2 or 4 spaces for indentation (be consistent with surrounding code).
- Use camelCase for function names and UpperCamelCase for types.
- Add comments for complex logic.

## Project Structure

- `src/`: Source code
  - `Core/`: Core logic (Types, Parser, Serializer)
  - `Business/`: Business logic (Add, Remove, Upgrade)
  - `External/`: External integrations (Hackage, Network)
  - `Utils/`: Utilities (Logging, Config)
- `test/`: Test suite
  - `Core/`, `Business/`, `Integration/`: Test modules
- `app/`: CLI entry point

## Reporting Issues

Please report bugs and feature requests on the GitHub Issue Tracker. Include:
- Steps to reproduce
- Expected behavior
- Actual behavior
- `cabal-edit` version
