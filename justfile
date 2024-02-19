set dotenv-load

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

build-image:
    nix build .#"hangman.server-docker" -o {{docker-image}}

release: build-image
    echo "Deploying {{revision}} to: {{env('DOCKER_HOST', 'local')}}"
    docker load < {{docker-image}}
    docker tag hangman-server:{{env('FLAKE_IMAGE_TAG')}} hangman-server:{{revision}}
    docker tag hangman-server:{{revision}} hangman-server:latest
    docker image rm hangman-server:{{env('FLAKE_IMAGE_TAG')}}
    echo "Stopping existing container"
    # A bit brute
    docker stop $(docker container ls -q)
    echo "Starting latest container"
    docker run --restart unless-stopped -d -p 80:8080 hangman-server:latest
    rm {{docker-image}}

