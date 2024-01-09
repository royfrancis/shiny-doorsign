# shiny-doorsign

[![gh-actions-build-status](https://github.com/royfrancis/shiny-doorsign/workflows/build/badge.svg)](https://github.com/royfrancis/shiny-doorsign/actions?workflow=build) [![Docker Image Size (latest by date)](https://img.shields.io/docker/image-size/royfrancis/shiny-doorsign?label=dockerhub)](https://hub.docker.com/repository/docker/royfrancis/shiny-doorsign)

A web app to create door signage for offices.

![](preview.jpg)

## Running the app

### Run online

Click [here](https://door-sign.serve.scilifelab.se/)

### Run in a docker container

```
docker run --platform=linux/amd64 --rm -p 8787:8787 royfrancis/shiny-doorsign
```

The app should be available through a web browser at `http://0.0.0.0:8787`.

---

2024 • Roy Francis
