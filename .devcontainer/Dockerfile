FROM rocker/geospatial:4.2

RUN apt update && \
    apt install -y --no-install-recommends python3-pip \
    htop

RUN pip3 install radian

RUN install2.r languageserver \
    httpgd \
    jsonlite \
    here \
    tictoc \
    R.utils \
    tools \
    future \
    furrr \
    viridis \
    patchwork \
    microbenchmark \
    targets \
    tarchetypes \
    ggmap \
    ggpubr \
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
    factoextra


# RUN R -e '''
#         install.packages("nanonext", repos = "https://shikokuchuo.r-universe.dev")
#         install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")
#         install.packages("crew", type = "source")
#         install.packages("targets", type = "source")
#         '''