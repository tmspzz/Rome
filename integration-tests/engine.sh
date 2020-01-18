#!/usr/bin/env bash

set -e

ACTION="$1"

STORAGE_DIR="server-cache"

if [ "$ACTION" == "upload" ]; then
  LOCAL_PATH="$2"
  REMOTE_PATH="$3"
  echo "🚂 Engine invocation: upload $LOCAL_PATH to $REMOTE_PATH"
  # create directory structure if it doesn't exist yet
  mkdir -p $STORAGE_DIR/$(dirname $REMOTE_PATH)
  # fake the upload of a file by just copying binary to the storage directory
  cp $LOCAL_PATH $STORAGE_DIR/$REMOTE_PATH

elif [ "$ACTION" == "download" ]; then
  REMOTE_PATH="$2"
  OUTPUT_PATH="$3"
  echo "🚂 Engine invocation: download $REMOTE_PATH to $OUTPUT_PATH"
  # create directory structure if it doesn't exist yet
  mkdir -p $(dirname $OUTPUT_PATH)
  # fake download by just copying binary from the storage directory
  # don't fail script if copy fails (some bcsymbolmap get requested wrongly at the moment, bug in Rome)
  if [[ -f $STORAGE_DIR/$REMOTE_PATH ]]; then 
    cp $STORAGE_DIR/$REMOTE_PATH $OUTPUT_PATH
  fi

elif [ "$ACTION" == "list" ]; then
  REMOTE_PATH="$2"
  echo "🚂 Engine invocation: list $REMOTE_PATH"
  # verify that the list command contains the cache prefix
  if [[ ! "$REMOTE_PATH" =~ "travis" ]]; then
    exit 1
  fi

  # fake list command by just checking if file exists
  if [[ ! -f "$STORAGE_DIR/$REMOTE_PATH" ]]; then
    exit 1
  fi
  
else
  # unsupported command
  exit 1
fi
