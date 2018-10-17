FROM rocker/shiny:3.4.4
MAINTAINER Simon J. Larsen "simonhffh@gmail.com"

EXPOSE 3838

ENV THEANO_FLAGS openmp=True,blas.ldflags="-L/usr/lib/ -lblas"
ENV OMP_NUM_THREADS 4

RUN apt-get install -y python2.7 python-pip sqlite3 git

COPY install_depends.R install_depends.R
RUN R -e "source('install_depends.R')"

RUN pwd

COPY setup.sh setup.sh
RUN /bin/bash setup.sh

COPY deepclip/*.py /srv/deepclip/code/
COPY . /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]
