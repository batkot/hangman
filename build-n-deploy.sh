#!/bin/bash

nix build ".#hangman.server-docker"
export DOCKER_HOST=${TARGET_HOST:+ssh://$TARGET_HOST}
export TARGET_NAME=${DOCKER_HOST:-local}
echo "[${TARGET_NAME}] Deploying revision $GIT_REV"
echo "[${TARGET_NAME}] Loading docker image"
docker load < result
echo "[${TARGET_NAME}] Tagging image as latest"
docker tag hangman-server:$GIT_REV hangman-server:latest
echo "[${TARGET_NAME}] Stopping existing container"
# A bit brute
docker stop $(docker container ls -q)
echo "[${TARGET_NAME}] Starting latest container"
docker run --restart unless-stopped -d -p 80:8080 hangman-server:latest
echo "[${TARGET_NAME}] Succeeded"

