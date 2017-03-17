#!/bin/bash

mkdir -p $HOME/.recipe-tracker

if [ -f $HOME/.nvm/nvm.sh ]; then
  echo "NVM exists, skipping"
else
  echo "Installing NVM"
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.1/install.sh | bash
fi

echo "Installing node 6.10.0"
$HOME/.nvm/nvm.sh install 6.10.0

echo "Using node 6.10.0"
$HOME/.nvm/nvm.sh use 6.10.0

echo "Installing elm"
npm install -g elm@0.18.0

mkdir -p $HOME/.recipe-tracker/static-deps

if [ -f $HOME/.recipe-tracker/static-deps/ratchet/dist/js/ratchet.min.js ]; then
  echo "Ratchet exists, skipping"
else
  echo "Installing Ratchet"
  curl https://codeload.github.com/twbs/ratchet/zip/v2.0.2 > /tmp/ratchet.zip

  unzip /tmp/ratchet.zip

  mv `dirname $0`/ratchet* $HOME/.recipe-tracker/static-deps/ratchet
fi

if [ -f $HOME/.recipe-tracker/es/run-node.sh ]; then
  echo "EventStore exists, skipping"
else
  echo "Installing EventStore"
  curl http://download.geteventstore.com/binaries/EventStore-OSS-Ubuntu-14.04-v3.9.3.tar.gz > /tmp/es.tar.gz

  gunzip /tmp/es.tar.gz
  tar xf /tmp/es.tar
  rm /tmp/es.tar

  mv `dirname $0`/EventStore* $HOME/.recipe-tracker/es/
fi
