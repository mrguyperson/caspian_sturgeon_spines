FROM rocker/geospatial:4.1

RUN echo msttcorefonts msttcorefonts/accepted-mscorefonts-eula select true | sudo debconf-set-selections

RUN apt update && \
    apt install -y --no-install-recommends python3-pip \
    htop \
    ttf-mscorefonts-installer

RUN pip3 install radian

RUN install2.r languageserver \
    httpgd \
    jsonlite \
    here \
    tictoc \
    viridis \
    microbenchmark \
    targets \
    tarchetypes \
    ggmap \
    ggpubr \
    ggsignif \
    rstatix \
    tidymodels \
    tidytext \
    vegan \
    discrim \
    rnaturalearth \
    cowplot \
    ggspatial \
    dendextend \
    ggdendro \
    cluster \
    factoextra \
    tinytex \
    Hmisc \
    raster \
    rnaturalearthhires

# RUN R -e '''
#         install.packages("nanonext", repos = "https://shikokuchuo.r-universe.dev")
#         install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")
#         install.packages("crew", type = "source")
#         install.packages("targets", type = "source")
#         '''
