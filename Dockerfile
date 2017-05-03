FROM kuestennebel_base

WORKDIR /kuestennebel
ADD . /kuestennebel

# HACK to keep the timestamps of the data subdirectory fresh, since rebuilding these requires ida
RUN find /kuestennebel/data -exec touch {} \;

RUN apt-get update
RUN apt-get -y install git python-numpy python-sklearn r-base r-cran-ggplot2 r-cran-jsonlite r-cran-reshape
RUN apt-get -y install r-cran-data.table rapidjson-dev

RUN ( cd multialign; make )

CMD make




