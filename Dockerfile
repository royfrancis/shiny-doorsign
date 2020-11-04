FROM rocker/shiny
LABEL authors="Roy Francis"
ARG REPO="royfrancis/shiny-doorsign"

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && \
    apt-get install -y git libxml2-dev libmagick++-dev && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("Cairo","curl","ggplot2","ggtext","magick","png","shinythemes","shinyAce","showtext"),dependencies=c("Depends","Imports"))'

RUN cd /srv/shiny-server/ && \
    git clone https://github.com/${REPO}.git app && \
    sudo chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app/', host = '0.0.0.0', port = 3838)"]
