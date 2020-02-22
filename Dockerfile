FROM rocker/shiny:3.6.1

RUN R -e "install.packages(c('ggplot2','data.table','tidyr'), repos='http://cran.rstudio.com/')"

COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/
