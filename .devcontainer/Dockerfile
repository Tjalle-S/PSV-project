FROM ubuntu:22.04

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

# Don't install suggested or recommended dependencies.
RUN echo 'APT::Install-Suggests "0";'   >> /etc/apt/apt.conf.d/00-docker
RUN echo 'APT::Install-Recommends "0";' >> /etc/apt/apt.conf.d/00-docker

# Install dependencies.
RUN apt-get update \
 && apt-get install -y git curl libz3-dev \
    software-properties-common gnupg pkg-config g++ make libgmp-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Create the vscode user.
ARG USERNAME=vscode
RUN adduser ${USERNAME}
ENV PATH "/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:${PATH}"
USER "${USERNAME}"

# Install minimal GHCup.
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=True
ENV BOOTSTRAP_HASKELL_MINIMAL=True
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC and related tools.
RUN ghcup install cabal 3.10.3.0
RUN ghcup install ghc --set 9.4.8
RUN ghcup install hls 2.7.0.0

# Make sure cabal's package database is up to date.
RUN cabal update

# Set shell to bash to use auto completion (e.g. arrow up for last command).
ENV SHELL="/bin/bash"