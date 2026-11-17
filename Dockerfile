# Base image for reproducible R pipeline runs
ARG R_VERSION=4.5.2
FROM rocker/r-ver:${R_VERSION}

ENV CRAN_REPO=https://cran.r-project.org
ENV PE_REPO=https://predictiveecology.r-universe.dev

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       cmake \
       gdal-bin \
       git \
       libabsl-dev \
       libcurl4-openssl-dev \
       libgdal-dev \
       libgeos-dev \
       libproj-dev \
       libsqlite3-dev \
       libssl-dev \
       libtbb-dev \
       libudunits2-dev \
       libxml2-dev \
       pkg-config \
       proj-bin \
    && rm -rf /var/lib/apt/lists/*

RUN gdal-config --version \
    && geos-config --version \
    && projinfo -o PROJ >/dev/null

WORKDIR /workspace

COPY renv.lock /workspace/renv.lock
COPY renv/activate.R /workspace/renv/activate.R
COPY renv/settings.json /workspace/renv/settings.json

RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN_REPO"))); install.packages("renv")'
RUN Rscript -e 'options(repos = c(PE = Sys.getenv("PE_REPO"), CRAN = Sys.getenv("CRAN_REPO"))); renv::restore()'

COPY . /workspace

CMD ["bash"]
