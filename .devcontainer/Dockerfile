# Use Ubuntu 22.04 as the base image to match the CI environment
FROM ubuntu:22.04

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install --no-install-recommends \
        # Essential build tools
        build-essential \
        curl \
        wget \
        git \
        ca-certificates \
        # Required for LLVM/Clang
        libtinfo5 \
        libxml2 \
        # Additional utilities
        sudo \
        vim \
        less \
        gnupg \
        lsb-release \
        software-properties-common \
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Install LLVM 18.1 and Clang
RUN wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh 18 \
    && rm llvm.sh \
    && apt-get update \
    && apt-get -y install --no-install-recommends \
        clang-18 \
        llvm-18 \
        llvm-18-dev \
        llvm-18-runtime \
        llvm-18-tools \
        libclang-18-dev \
        libllvm18 \
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Set LLVM 18 as the default version
RUN update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-18 100 \
    && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-18 100 \
    && update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-18 100

# Create a non-root user
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Switch to non-root user
USER $USERNAME
ENV HOME=/home/$USERNAME

# Install Rust 1.88.0 (project MSRV) with clippy
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    --default-toolchain 1.88.0 \
    --component clippy

# Add Rust to PATH
ENV PATH="$HOME/.cargo/bin:$PATH"

# Install Rust nightly with rustfmt
RUN rustup toolchain install nightly \
    && rustup component add rustfmt --toolchain nightly

# Install cargo-insta for snapshot testing
RUN cargo install cargo-insta

# Install cargo-nextest for better test runner
RUN cargo install cargo-nextest --locked

# Set environment variables for LLVM
ENV LLVM_SYS_180_PREFIX=/usr/lib/llvm-18
ENV LLVM_CONFIG=/usr/bin/llvm-config-18

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# Set the working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
