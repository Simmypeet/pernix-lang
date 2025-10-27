# Pernix Language Development Container

This directory contains the configuration for a VS Code Dev Container that provides a complete development environment for the Pernix programming language compiler.

## What's Included

The development container includes all the necessary tools and dependencies:

- **Rust Toolchain**
  - Rust stable (>= 1.88.0) with clippy
  - Rust nightly with rustfmt
  
- **LLVM & Clang**
  - LLVM 18.1 for Inkwell
  - Clang 18 for object code linking
  
- **Testing Tools**
  - cargo-insta for snapshot testing
  - cargo-nextest for enhanced test running

- **VS Code Extensions**
  - rust-analyzer for Rust IDE support
  - Even Better TOML for better TOML file editing
  - crates for managing Cargo dependencies

## Getting Started

### Prerequisites

- [Docker](https://www.docker.com/products/docker-desktop)
- [Visual Studio Code](https://code.visualstudio.com/)
- [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

### Using the Dev Container

1. Clone the repository:
   ```bash
   git clone https://github.com/Simmypeet/pernix-lang.git
   cd pernix-lang
   ```

2. Open the folder in VS Code:
   ```bash
   code .
   ```

3. When prompted, click "Reopen in Container" or:
   - Press `F1` or `Ctrl+Shift+P` (Windows/Linux) / `Cmd+Shift+P` (macOS)
   - Type "Dev Containers: Reopen in Container"
   - Press Enter

4. Wait for the container to build and start (first time may take several minutes)

5. Once the container is ready, you can start developing! The container will have all dependencies pre-installed.

### Available Commands

Inside the container, you can use:

```bash
# Build the project
cargo build

# Run tests with nextest
cargo nextest run

# Format code with nightly rustfmt
cargo +nightly fmt

# Run clippy lints
cargo clippy

# Run snapshot tests
cargo insta test
cargo insta review

# Check LLVM installation
llvm-config --version
```

## Troubleshooting

### Container fails to build

If the container fails to build, try:
1. Ensure Docker has enough resources allocated (at least 4GB RAM, 2 CPUs)
2. Clear Docker cache: `docker system prune -a`
3. Rebuild the container: Dev Containers: Rebuild Container

### LLVM-related errors

If you encounter LLVM-related build errors, verify the environment variables are set:
```bash
echo $LLVM_SYS_180_PREFIX
echo $LLVM_CONFIG
llvm-config --version
```

## Contributing

When using the dev container, all your changes are made in your local filesystem and are persisted even when the container is stopped or rebuilt. Git operations work normally.
