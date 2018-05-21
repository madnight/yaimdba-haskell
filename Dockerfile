FROM pritunl/archlinux:2018-05-12

RUN pacman -S --noconfirm mongodb mongodb-tools nodejs npm wget
RUN npm install -g tsv-to-json2

COPY src/download.sh /

RUN bash download.sh

CMD bash provision.sh

