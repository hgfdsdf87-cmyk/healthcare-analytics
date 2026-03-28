# ╔══════════════════════════════════════════════════════════════════╗
# ║   UNIT V — Dockerfile for Healthcare R Shiny App               ║
# ║   Containerizes the Unit II R Shiny dashboard                  ║
# ╚══════════════════════════════════════════════════════════════════╝

FROM rocker/shiny:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', 'shinydashboard', 'ggplot2', 'dplyr', 'tidyr', \
    'lubridate', 'plotly', 'readr', 'DT', 'scales', 'forcats' \
    ), repos='https://cran.rstudio.com/')"

# Copy app files into container
COPY healthcare_dashboard.R /srv/shiny-server/app/app.R
COPY healthcare_dataset.csv /srv/shiny-server/app/healthcare_dataset.csv

WORKDIR /srv/shiny-server/app
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:3838/app/ || exit 1

CMD ["/usr/bin/shiny-server"]
