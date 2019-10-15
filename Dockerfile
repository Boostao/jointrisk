FROM rocker/r-base
LABEL maintainer="Bruno Tremblay <bruno.tremblay@lacapitale.com>"

WORKDIR /src

COPY ./api /etc
COPY soussnnn.csv soussnnn.csv
COPY lacapitale-root-ca.crt /usr/local/share/ca-certificates/lacapitale-root-ca.crt
COPY jointrisk_1.0.0.tar.gz jointrisk_1.0.0.tar.gz

RUN mkdir /usr/local/share/ca-certificates || exit 0
RUN /usr/sbin/update-ca-certificates

RUN apt update -qq && apt install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libudunits2-dev

RUN install2.r plumber data.table
RUN install2.r sf
RUN install2.r jointrisk_1.0.0.tar.gz

EXPOSE 8004
ENTRYPOINT ["R", "-f", "/etc/startup.R", "--slave"]