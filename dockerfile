FROM ghcr.io/rocker-org/shiny:4.4.1
LABEL Description="Docker image for shiny-doorsign"
LABEL authors="Roy Francis"
LABEL org.opencontainers.image.source="https://github.com/royfrancis/shiny-doorsign"
ARG QUARTO_VERSION="1.6.39"

RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get clean \
    && apt-get install -y libxml2-dev libssl-dev libcurl4-openssl-dev libudunits2-dev curl \
    && curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && apt-get install -y ./quarto-linux-amd64.deb \
    && rm -rf ./quarto-linux-amd64.deb \
    && rm -rf /var/lib/apt/lists/* \
    && install2.r --error --skipinstalled markdown remotes bsicons \
    && Rscript -e 'remotes::install_github("rstudio/bslib");remotes::install_github("quarto-dev/quarto-r")' \
    && rm -rf /tmp/downloaded_packages

COPY . /srv/shiny-server/app
COPY shiny-server.config /etc/shiny-server/shiny-server.conf
RUN sudo chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 8787

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app/', host = '0.0.0.0', port = 8787)"]

# docker build --platform=linux/amd64 -t royfrancis/shiny-doorsign:2.1.1 -t royfrancis/shiny-doorsign:latest .
# docker run --platform=linux/amd64 --rm -p 8787:8787 royfrancis/shiny-doorsign:latest
