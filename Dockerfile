# Base image for OpenShift / NERC 
FROM dukegcb/openshift-shiny-verse:4.1.2

# Install R packages not included in tidyverse
RUN install2.r \
    plotly \
    vipor \
    shinyWidgets \
    DT

# copy src folder to srv/code
ADD ./src /srv/code

# Set the working directory (optional)
# WORKDIR /srv/code

# Start the Shiny app automatically when the container runs (for local testing
CMD ["R", "-e", "shiny::runApp('/srv/code', host='0.0.0.0', port=3838)"]