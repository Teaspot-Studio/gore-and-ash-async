FROM haskell:8.0.1

RUN apt-get update
RUN apt-get -y install curl

ADD mkdocs /opt/project/
ADD LICENSE /opt/project/
ADD Setup.hs /opt/project/
ADD README.md /opt/project/
ADD gore-and-ash-async.cabal /opt/project/
ADD stack.yaml /opt/project/
ADD src /opt/project/src

WORKDIR /opt/project

ENTRYPOINT ["./mkdocs", "gore-and-ash-async", "1.1.1.0", "NCrashed"]
