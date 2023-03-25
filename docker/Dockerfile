FROM haskell:9.4.4 as build-env
RUN mkdir /opt/build
WORKDIR /opt/build

COPY stack.yaml stack.yaml.lock /opt/build/
COPY hangman/package.yaml /opt/build/hangman/
COPY hangman-server/package.yaml /opt/build/hangman-server/

RUN stack build --dependencies-only

COPY . /opt/build/

RUN stack build

RUN cp -r "$(stack path --local-install-root)/bin" /opt/build/bin

FROM archlinux:latest
WORKDIR /opt/server
COPY --from=build-env /opt/build/bin .

ENTRYPOINT ["/opt/server/hangman-server-exe"]
CMD ["-p 8080"]
