#!/bin/bash

# If $AWS_DEFAULT_REGION doesn't exist, then on local

if [[ -z "${DOCKER_RUNNING}" ]]; then
		    echo "on local"

    if [ ! -d "data/" ]
    then
        mkdir -p data/
    fi
    

#curl https://ppi-variant-dynamics.s3.amazonaws.com/latest_data/color_dict.csv --output data/color_dict.csv

curl https://rfds-ds-twitter.s3.amazonaws.com/ner_tagged_landmine_tweets.csv --output ner_tagged_landmine_tweets.csv

curl https://rfds-ds-twitter.s3.amazonaws.com/main.csv --output main.csv


else 

wget -N https://rfds-ds-twitter.s3.amazonaws.com/ner_tagged_landmine_tweets.csv -P /srv/shiny-server/

wget -N https://rfds-ds-twitter.s3.amazonaws.com/main.csv -P /srv/shiny-server/

fi