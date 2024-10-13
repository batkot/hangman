set dotenv-load := true

revision := ```
    commit_sha=$(git rev-parse HEAD)
    if [ -z "$(git status --porcelain)" ]; then
        echo $commit_sha
    else
        echo "${commit_sha}-dirty"
    fi
    ```
docker-image := 'server-image'

default:
    @just --list

hpack:
    find -iname package.yaml -exec hpack {} \;

build: hpack
    cabal build all

run: build
    cabal exec hangman-server-exe -- -p 8081

build-image:
    nix build ".#hangman.server-docker" -o {{ docker-image }}

release: build-image
    @echo "Deploying {{ revision }} to: {{ env('DOCKER_HOST', 'local') }}"
    docker load < {{ docker-image }}
    docker tag hangman-server:{{ env('FLAKE_IMAGE_TAG') }} hangman-server:{{ revision }}
    docker tag hangman-server:{{ revision }} hangman-server:latest
    docker image rm hangman-server:{{ env('FLAKE_IMAGE_TAG') }}
    docker-compose -f {{ env('DOCKER_COMPOSE_FILE', 'docker-compose.yaml -f docker-compose.local.yaml') }} up -d --remove-orphans
    rm {{ docker-image }}
