FROM rocker/shiny:4

# Install necessary system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shinydashboard', 'shinymanager', 'readxl', 'dplyr', 'sf', 'ggplot2', 'viridis', 'stringr', 'plotly', 'tibble', 'DT'), repos='https://cran.rstudio.com/')"

# Install shinyauthr package from GitHub
RUN R -e "remotes::install_github('paulc91/shinyauthr')
# Copy your Shiny app code into the image
COPY . /usr/src/app

# Set the working directory
WORKDIR /usr/src/app

# Expose the port your Shiny app will run on
EXPOSE 3838

# Define the command to run your application
CMD ["R", "-e", "shiny::runApp('/usr/src/app/app.R', host='0.0.0.0', port=3838)"]

