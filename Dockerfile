# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# aws
# RUN pip install awscli2

# get shiny server and R from the rocker project
FROM rocker/shiny-verse:latest
# Set env var for data path
ENV DOCKER_RUNNING=true
# And make this "root only" var visible to R files
RUN echo "DOCKER_RUNNING=$DOCKER_RUNNING" >> /usr/local/lib/R/etc/Renviron

# install awscli
RUN wget "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -O "awscliv2.zip"
RUN unzip awscliv2.zip
RUN sudo ./aws/install

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && \
 apt-get install -y libgeos-dev libproj-dev libgdal-dev libudunits2-dev git cron

# cron setup
RUN /etc/init.d/cron start
COPY get-s3-dynamics.sh /bin/
RUN chmod +x /bin/get-s3-dynamics.sh
COPY get-s3-dynamics.cron /etc/cron.d/get-s3-dynamics
RUN chmod 0644 /etc/cron.d/get-s3-dynamics
RUN crontab /etc/cron.d/get-s3-dynamics

# install packages
COPY dependencies.R ./
RUN Rscript dependencies.R 

# copy the app directory into the image
COPY ./shiny-app/* /srv/shiny-server/

# run app
#CMD ["/usr/bin/shiny-server"]

# get s3
RUN wget https://s3.amazonaws.com/ec2-downloads-windows/SSMAgent/latest/debian_amd64/amazon-ssm-agent.deb
RUN apt-get install ./amazon-ssm-agent.deb -y

# run app
CMD ["/bin/bash", "-c", "/usr/sbin/cron;/usr/bin/shiny-server"]