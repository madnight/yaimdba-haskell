FROM pritunl/archlinux:2018-05-12

RUN pacman -S --noconfirm stack make

RUN mkdir -p /app/src
WORKDIR /app
COPY src/ /app/src
COPY app/ /app/app
COPY yaimdba.cabal stack.yaml package.yaml Setup.hs /app/

RUN stack build

CMD sleep 10 && stack exec yaimdba-exe
