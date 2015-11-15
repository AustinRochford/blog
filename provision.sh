#!/bin/bash

apt-get update

# Install Haskell
apt-get install -y software-properties-common
add-apt-repository -y ppa:hvr/ghc
apt-get update
apt-get install -y cabal-install-1.20 ghc-7.8.4
cat >> /home/vagrant/.bashrc <<EOF
export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
EOF
export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
cabal update
cabal install alex happy

# Install blog dependencies
apt-get install -y zlib1g-dev
cd /vagrant
cabal sandbox init
cabal update
cabal configure
cabal install --dependencies-only
