FROM fpco/stack-build:lts-16.2 as build
RUN mkdir /opt/build && mkdir /opt/build/bin
WORKDIR /opt/build
COPY package.yaml stack.yaml stack.yaml.lock patova-proxy.cabal /opt/build/
RUN stack build --system-ghc --only-dependencies --test
COPY . /opt/build/
RUN stack install --system-ghc --local-bin-path /opt/build/dist/ && \
    cp login.mustache /opt/build/dist/

FROM ubuntu:18.04
RUN mkdir -p /opt/patova-proxy
WORKDIR /opt/patova-proxy
RUN apt-get update -y && \
    apt-get install -y ca-certificates libgmp-dev && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /opt/build/dist/ .
CMD ["/opt/patova-proxy/patova-proxy"]
