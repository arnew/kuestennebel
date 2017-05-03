FROM ubuntu:16.10

WORKDIR /kuestennebel
ADD . /kuestennebel

RUN apt update
RUN apt -y install git python-numpy python-sklearn r-base r-cran-ggplot2 r-cran-jsonlite r-cran-reshape
RUN apt -y install r-cran-data.table 

CMD make




