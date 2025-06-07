# Gunakan image R dengan shiny server
FROM rocker/shiny:latest

# Install dependencies untuk RPostgres
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpq-dev && \
    R -e "install.packages(c('shinydashboard', 'DBI', 'RPostgres', 'DT', 'ggplot2', 'dplyr'), repos='https://cloud.r-project.org')"

# Salin semua file ke dalam image
COPY . /srv/shiny-server/app

# Set permission
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port shiny
EXPOSE 3838

# Jalankan shiny-server
CMD ["/usr/bin/shiny-server"]