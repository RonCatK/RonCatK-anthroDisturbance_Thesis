# Base image for reproducible R pipeline runs
FROM rocker/r-ver:4.3.2

# Install system dependencies (extend later as needed)
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       git \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy repository (populate via bind mount or build context)
WORKDIR /workspace
COPY . /workspace

# Placeholder: install R packages via renv/targets script
RUN echo "TODO: configure renv restore once lockfile is finalized"

CMD ["bash"]
