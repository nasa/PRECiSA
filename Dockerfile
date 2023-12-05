FROM ubuntu:22.04
LABEL precisa-ci version="1.0"

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN apt update -yq
RUN apt upgrade -yq
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt install -yq    \
      build-essential  \
      ca-certificates  \
      cmake            \
      git              \
      g++-multilib     \
      locales          \
      make             \
      vim              \
      wget
RUN locale-gen en_US.UTF-8

# GHCUp
RUN apt install -yq build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
      BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
      BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
      BOOTSTRAP_HASKELL_GHC_VERSION=8.10 \
      BOOTSTRAP_HASKELL_CABAL_VERSION=recommended \
      BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
      sh
RUN echo ". ~/.ghcup/env" >> ~/.bashrc
