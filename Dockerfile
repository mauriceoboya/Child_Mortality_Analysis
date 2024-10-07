FROM rocker/shiny:4

# Install necessary packages
RUN apt-get update &&  apt-get install -y \
   libssl-dev \
    libcurl4-openssl-dev \
    iputils-ping \
    curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyauthr', 'readxl', 'dplyr', 'sf', 'ggplot2', 'viridis', 'stringr', 'plotly', 'tibble'), repos='https://cran.rstudio.com/')"

# Copy your Shiny app code into the image
COPY ./  /srv/shiny-server/MauriceOboya


# Command to run Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/MauriceOboya')"]
