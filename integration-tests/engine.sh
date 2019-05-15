#!/usr/bin/env bash

set -e

ACTION="$1"

STORAGE_DIR="server-cache"

if [ "$ACTION" == "upload" ]; then
  LOCAL_PATH="$2"
  REMOTE_PATH="$3"
  # POST request to upload file to remote path
  # create directory structure if it doesn't exist yet
  mkdir -p $STORAGE_DIR/$(dirname $REMOTE_PATH)
  # fake the upload of a file by just copying binary to the storage directory
  cp $LOCAL_PATH $STORAGE_DIR/$REMOTE_PATH

elif [ "$ACTION" == "download" ]; then
  REMOTE_PATH="$2"
  OUTPUT_PATH="$3"
  # fake download by just copying binary from the storage directory
  cp $STORAGE_DIR/$REMOTE_PATH $OUTPUT_PATH

elif [ "$ACTION" == "list" ]; then
  REMOTE_PATH="$2"
  # fake list command by just checking if file exists
  if [ ! -f "$STORAGE_DIR/$REMOTE_PATH" ]; then
    exit 1
  fi
  
else
  # unsupported command
  exit 1
fi
