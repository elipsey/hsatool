# HSA server from Debian 8
FROM debian:8

LABEL maintainer "christophermartinberry@gmail.com"
LABEL version=".1"
LABEL description="Hydrologically Sensitive Areas Decision Support System"

RUN apt-get update && apt-get install --no-install-recommends -y \
#apache2 \
#dialog \
#less \
#libcurl4-openssl-dev \
#libssl-dev \
libxml2 \
#libxml2-dev \ # only needed for build, otherwise use libxml2
nginx \
r-base-core \
#ssh \
#vim \
#wget \
&& apt-get clean && rm -rf /var/lib/apt/lists/*

ADD site-library.tar.gz /
ADD hsadss-model.tar.gz /
ADD hsadss-webroot.tar.gz /
#ADD install-packages.r /tmp/
#RUN Rscript /tmp/install-packages.r
RUN service nginx start

#ENV APACHE_RUN_USER www-data
#ENV APACHE_RUN_GROUP www-data
#ENV APACHE_LOG_DIR /var/log/apache2

EXPOSE 80
#EXPOSE 22

#CMD ["bash"]
ENTRYPOINT /bin/bash 
