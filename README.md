# shiny-doorsign

[![gh-actions-build-status](https://github.com/royfrancis/shiny-doorsign/workflows/build/badge.svg)](https://github.com/royfrancis/shiny-doorsign/actions?workflow=build) [![Docker Image Size (latest by date)](https://img.shields.io/docker/image-size/royfrancis/shiny-doorsign?label=dockerhub)](https://hub.docker.com/repository/docker/royfrancis/shiny-doorsign)

This is an R shiny app to create door signage for offices.

![](preview.png)

## Running the app

### Run using docker

```
docker run --rm -p 8787:8787 royfrancis/shiny-doorsign:v1.2
```

The app should be available through a web browser at `http://0.0.0.0:8787`.

### Run in R

Install the following R packages:

```
install.packages(c(Cairo, curl, ggplot2, magick, png, shiny, shinyBS, shinythemes, showtext))
```

This repo is not an R package. In the root directory of this repo, run app using `shiny::runApp("app.R")`.

---

2022 • Roy Francis
