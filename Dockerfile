FROM rocker/shiny-verse:3.6.1
MAINTAINER Simon J. Larsen "simonhffh@gmail.com"

EXPOSE 3838

RUN apt-get update && apt-get install -y python2.7 python-pip sqlite3 git libbz2-dev liblzma-dev cython && mkdir /srv/deepclip /srv/deepclip/code /srv/deepclip/results /srv/deepclip/data

COPY .theanorc /home/shiny/.theanorc
COPY .theanorc /root/.theanorc
COPY install_depends.R install_depends.R
COPY setup.sh setup.sh
RUN R -e "source('install_depends.R')" && /bin/bash setup.sh

COPY deepclip/*.py /srv/deepclip/code/
COPY *.R *.md /srv/shiny-server/
COPY www/ /srv/shiny-server/www/
COPY data/example/ /srv/shiny-server/data/example/

COPY pretrained_models.csv /srv/shiny-server/
COPY data/models/ /srv/deepclip/models/
COPY deepclip.db /srv/deepclip/deepclip.db
COPY data/results.tar.gz results.tar.gz
COPY recompile_models.sh recompile_models.sh

RUN tar xvfz results.tar.gz -C /srv/deepclip/results/ && chown -R shiny:shiny /srv/deepclip && usermod -a -G shiny root && usermod -a -G shiny shiny && chmod -R g+rw /srv/deepclip && /bin/bash recompile_models.sh

CMD ["/usr/bin/shiny-server.sh"]
