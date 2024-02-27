# Use the rocker/shiny:4 base image
FROM rocker/shiny:4

# Install necessary packages
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    iputils-ping \
    curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny','shinymanager'), repos='https://cran.rstudio.com/')"

# Set the working directory
WORKDIR /srv/shiny-server

# Copy your Shiny app code into the image
COPY ./MauriceOboya /srv/shiny-server/MauriceOboya

# Expose the Shiny port (as per Render convention)
EXPOSE 8080

# Set environment variable for Shiny server port
ENV PORT=8080

# Command to run Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/MauriceOboya', host='0.0.0.0', port=8080)"]
