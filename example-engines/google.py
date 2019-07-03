#!/usr/bin/env python3

import argparse
import os
import zipfile

from google.cloud import storage
from google.api_core.exceptions import NotFound


from pathlib import Path
from shutil import rmtree

# Basics
TEMP_DIR = Path('/tmp/rome/')

# Commands
COMMAND_UPLOAD = 'upload'
COMMAND_DOWNLOAD = 'download'
COMMAND_LIST = 'list'

# Platforms
PLATFORM_IOS = 'iOS'
PLATFORM_MAC = 'Mac'
PLATFORM_WATCHOS = 'WatchOS'
PLATFORM_TVOS = 'tVOS'

PLATFORMS = [PLATFORM_IOS, PLATFORM_MAC, PLATFORM_TVOS, PLATFORM_WATCHOS]

# GCS info
PROJECT = 'YOUR_GOOGLE_PROJECT'
BUCKET = 'YOUR_GOOGLE_BUCKET'

parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers(dest='command', required=True)

upload_parser = subparsers.add_parser(COMMAND_UPLOAD)
upload_parser.add_argument('local_path', help='local file to upload')
upload_parser.add_argument('remote_path', help='remote path to upload to')

download_parser = subparsers.add_parser(COMMAND_DOWNLOAD)
download_parser.add_argument('remote_path', help='remote path to download')
download_parser.add_argument('local_path', help='destination local path')

list_parser = subparsers.add_parser(COMMAND_LIST)
list_parser.add_argument('local_path', help='remote path for the local file')

kwargs = vars(parser.parse_args())
command = kwargs['command']

# Instantiate the client
gcs_client = storage.Client(project=PROJECT)
bucket = gcs_client.get_bucket(BUCKET)


if command == COMMAND_UPLOAD:
    local_path = kwargs['local_path']
    remote_path = kwargs['remote_path']
    print(f'uploading {local_path} to {remote_path}')

    blob = bucket.blob(remote_path)
    blob.upload_from_filename(local_path)

    # Cleanup
    path = Path(local_path)
    os.remove(local_path)
    rmtree(path.parents[0])
elif command == COMMAND_DOWNLOAD:
    remote_path = kwargs['remote_path']
    local_path = Path(kwargs['local_path'])

    platform = local_path.parts[1]
    if platform not in PLATFORMS:
        exit(0)

    TEMP_DIR.mkdir(parents=True, exist_ok=True)
    zip_file_path = Path(f"{TEMP_DIR.joinpath(local_path.stem)}{local_path.parts[-1]}")
    blob = bucket.blob(remote_path)
    try:
        blob.download_to_filename(str(zip_file_path))
    except NotFound:
        print(f"{remote_path} 404'ed")
        zip_file_path.unlink()
        exit(1)

    # Unzip and cleanup
    zip_ref = zipfile.ZipFile(zip_file_path, 'r')
    zip_ref.extractall('.')
    zip_ref.close()
elif command == COMMAND_LIST:
    local_path = kwargs['local_path']
    print(f'finding {local_path}')

    blob = bucket.get_blob(local_path)
    if not blob:
        print(f'{local_path} does not exist')
        exit(1)
