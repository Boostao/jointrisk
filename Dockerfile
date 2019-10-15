FROM rocker/r-base
LABEL maintainer="Bruno Tremblay <bruno.tremblay@lacapitale.com>"

WORKDIR /src

RUN mkdir /usr/local/share/ca-certificates || exit 0
RUN /usr/sbin/update-ca-certificates

RUN apt-get update && apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev

RUN install2.r --error \
  data.table \
  plumber \
  sf

COPY ./api /etc
COPY soussnnn.csv soussnnn.csv
COPY lacapitale-root-ca.crt /usr/local/share/ca-certificates/lacapitale-root-ca.crt
COPY jointrisk_1.0.0.tar.gz jointrisk_1.0.0.tar.gz

RUN R CMD INSTALL jointrisk_1.0.0.tar.gz

EXPOSE 8004
ENTRYPOINT ["R", "-f", "/etc/startup.R", "--slave"]
