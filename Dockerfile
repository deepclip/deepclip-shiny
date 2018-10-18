FROM rocker/shiny-verse:3.4.4
MAINTAINER Simon J. Larsen "simonhffh@gmail.com"

EXPOSE 3838

RUN apt-get install -y python2.7 python-pip sqlite3 git

COPY install_depends.R install_depends.R
RUN R -e "source('install_depends.R')"

COPY setup.sh setup.sh
RUN /bin/bash setup.sh

COPY deepclip/*.py /srv/deepclip/code/
COPY data/results/ /srv/deepclip/results/
COPY data/models/ /srv/deepclip/models/
COPY deepclip.db /srv/deepclip/deepclip.db
COPY *.R *.md /srv/shiny-server/
COPY www/ /srv/shiny-server/www/

COPY .theanorc /home/shiny/.theanorc
COPY .theanorc /root/.theanorc

COPY recompile_models.sh recompile_models.sh
RUN /bin/bash recompile_models.sh


CMD ["/usr/bin/shiny-server.sh"]
