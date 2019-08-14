FROM rocker/shiny-verse:3.4.4
MAINTAINER Simon J. Larsen "simonhffh@gmail.com"

EXPOSE 3838

RUN echo blabla
RUN apt-get update
RUN apt-get install -y python2.7 python-pip sqlite3 git libbz2-dev liblzma-dev

COPY install_depends.R install_depends.R
RUN R -e "source('install_depends.R')"

COPY setup.sh setup.sh
RUN /bin/bash setup.sh

RUN mkdir /srv/deepclip /srv/deepclip/code /srv/deepclip/results
COPY deepclip/*.py /srv/deepclip/code/
COPY data/results.tar.gz results.tar.gz
RUN tar xvfz results.tar.gz -C /srv/deepclip/results/
COPY data/models/ /srv/deepclip/models/
COPY deepclip.db /srv/deepclip/deepclip.db
RUN chown -R shiny:shiny /srv/deepclip && usermod -a -G shiny root && usermod -a -G shiny shiny && chmod -R g+rw /srv/deepclip

COPY *.R *.md pretrained_models.csv /srv/shiny-server/
COPY www/ /srv/shiny-server/www/
COPY data/example/ /srv/shiny-server/data/example/

COPY .theanorc /home/shiny/.theanorc
COPY .theanorc /root/.theanorc

COPY recompile_models.sh recompile_models.sh
RUN /bin/bash recompile_models.sh

CMD ["/usr/bin/shiny-server.sh"]
