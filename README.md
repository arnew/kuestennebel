
Using Docker
============

    git clone https://github.com/arnew/kuestennebel.git
    sudo apt-get install docker.io
    docker build -t kuestennebel_base dockerbase
    docker build -t kuestennebel .


Running With Docker
-------------------

    docker run -it kuestennebel make evaluation-putty
    docker run -it kuestennebel make
    docker run -it kuestennebel 
    docker run -it kuestennebel bash
	

Dependencies
============

Tested on ubuntu 16.10 :
(requires packages from universe)

    sudo apt install git python-numpy python-sklearn r-base r-cran-ggplot2 r-cran-jsonlite r-cran-reshape r-cran-data.table


