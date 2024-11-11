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
    # nix build ".#hangman.server-docker" -o {{ docker-image }}
    DOCKER_HOST= docker build -t hangman-server:{{ revision }} -f DOCKERFILE . --platform linux/amd64

release: build-image
    @echo "Deploying {{ revision }} to: {{ env('DOCKER_HOST', 'local') }}"
    DOCKER_HOST= docker save hangman-server:{{ revision }} > {{ docker-image }}
    docker load < {{ docker-image }}
    # docker tag hangman-server:{{ env('FLAKE_IMAGE_TAG') }} hangman-server:{{ revision }}
    docker tag hangman-server:{{ revision }} hangman-server:latest
    # docker image rm hangman-server:{{ env('FLAKE_IMAGE_TAG') }}
    docker-compose -f {{ env('DOCKER_COMPOSE_FILE', 'docker-compose.yaml -f docker-compose.local.yaml') }} up -d --remove-orphans
    rm {{ docker-image }}

push-image release-rev="latest":
    docker login https://{{ env('DOCKER_REGISTRY_HOST') }} -u {{ env('DOCKER_REGISTRY_USER') }}
    docker tag hangman-server:{{ release-rev }} {{ env('DOCKER_REGISTRY_HOST') }}/hangman-server:{{ release-rev }}
    docker push {{ env('DOCKER_REGISTRY_HOST') }}/hangman-server:{{ release-rev }}
