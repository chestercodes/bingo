#!/bin/bash

set -o allexport
source /opt/games-room.env
set +o allexport

docker-compose up -d