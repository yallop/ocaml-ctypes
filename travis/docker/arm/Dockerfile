FROM lojzik/rpi-debian:jessie

COPY qemu-arm-static /usr/bin/qemu-arm-static

ENV OCAML_VERSION=4.02.3

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get -y update && \
  apt-get -y upgrade && \
  apt-get -y install aspcud git make curl gcc bzip2 sudo unzip m4 libffi-dev pkg-config libncurses-dev

RUN curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | env OCAMLBREW_FLAGS="-r -v ${OCAML_VERSION}" bash

RUN ln -s $HOME/ocamlbrew/ocaml-$OCAML_VERSION/bin/* /usr/local/bin

RUN opam init --auto-setup --yes
RUN opam update
RUN opam install -y ocamlfind conf-pkg-config lwt ctypes-foreign ounit

CMD [ "bash" ]
