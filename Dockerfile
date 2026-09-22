# # HOWTO Build and Test
#
# 1. Clone the PRECiSA repository, e.g.:
#    ```
#    $ git clone https://github.com/nasa/PRECiSA precisa
#    ```
# 2. Change your current directory to the repository:
#    ```
#    $ cd precisa
#    ```
# 3. Run:
#    ```
#    $ docker build --tag precisa-ci .
#    ```
# 4. To test it run the following command:
#    ```
#    docker run -v<full-path-of-precisa-repo>:/builds/larc-scasb-fm/fp/precisa -it precisa-ci /bin/bash
#    ```
#    and you will be in the container of the GitLab CI.
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
      jq               \
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
      BOOTSTRAP_HASKELL_GHC_VERSION=9.6.7 \
      BOOTSTRAP_HASKELL_CABAL_VERSION=recommended \
      BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
      sh
RUN echo ". ~/.ghcup/env" >> ~/.bashrc

# PVS
RUN apt install -yq sbcl emacs
RUN git clone https://github.com/SRI-CSL/PVS.git
RUN cd /PVS && ./configure && make

RUN cd /PVS && git clone https://github.com/nasa/pvslib.git nasalib
ENV PVS_DIR /PVS
ENV PVS_LIBRARY_PATH /builds/larc-scasb-fm/fp/precisa/PVS:/PVS/nasalib
RUN cd /PVS/nasalib && ./cleanbin-all
RUN cd /PVS/nasalib && ./install-scripts

# Pre-compile Kodiak
ADD http://www2.math.uni-wuppertal.de/wrswt/software/filib++/filibsrc-3.0.2.tar.gz /
RUN tar xvfz filibsrc-3.0.2.tar.gz
RUN cd /filibsrc && ./configure CFLAGS=-fPIC CPPFLAGS="-fPIC -std=c++11" CXXFLAGS="-fPIC -std=c++11" && make -j && make install

# Pre-fetch Cabal dependencies
RUN mkdir -p /builds/larc-scasb-fm/fp/precisa
ADD . /builds/larc-scasb-fm/fp/precisa
WORKDIR /builds/larc-scasb-fm/fp/precisa
RUN cat > cabal.project.local <<EOF
optimization: True

package precisa
  extra-lib-dirs: /builds/larc-scasb-fm/fp/precisa/Kodiak/build
EOF
RUN . ~/.ghcup/env && cabal build all --only-dependencies --enable-tests -j

# Pre-typecheck repository
ENV PATH /PVS:$PATH
RUN pvs -raw -E "(progn (tc \"/builds/larc-scasb-fm/fp/precisa/PVS/PRECiSA/top.pvs\" t) (make-new-pvs-image \"precisa-ready\"))"
