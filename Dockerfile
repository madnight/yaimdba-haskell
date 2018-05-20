FROM pritunl/archlinux:2018-05-12

RUN pacman -S --noconfirm mongodb-tools nodejs npm wget
RUN npm install -g tsv-to-json2

COPY src/provision.sh /

RUN bash provision.sh fetch_datasets

CMD bash provision.sh mongo_import

