# Base R Shiny image
FROM rocker/shiny-verse:latest

# Install packages
RUN apt-get update
RUN apt-get install -y libudunits2-dev libproj-dev libgdal-dev
RUN ldconfig

# Install R dependencies
RUN R -e "install.packages(c('leaflet', 'leaflet.extras', 'terra', 'viridis', 'DT', 'scales', 'shinyjs', 'randomForest'))"

# Copy the Shiny app code
COPY ./ /home

# Need to copy .Renviron if running locally, but not deploying to DigitalOcean
# Environmental variables managed through DO website tools, not Dockerfile
# COPY .Renviron /root/.Renviron

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/home', host='0.0.0.0', port=8180)"]