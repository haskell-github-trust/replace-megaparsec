# Dockerfile for usage examples hosted on mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t replace-megaparsec .
#     docker run --rm -p 8888:8888 -v $PWD:/home/jovyan/pwd --name replace-megaparsec --env JUPYTER_TOKEN=x replace-megaparsec:latest
#

FROM crosscompass/ihaskell-notebook:fb96a81230df

USER root

COPY notebook/ReplaceMegaparsecUsage.ipynb /home/$NB_USER/work/
COPY notebook/stack.yaml /home/$NB_USER/work/
RUN cat /opt/stack/global-project/stack.yaml /home/$NB_USER/work/stack.yaml >> /home/$NB_USER/work/stack.yaml.cat && mv /home/$NB_USER/work/stack.yaml.cat /home/$NB_USER/work/stack.yaml
RUN fix-permissions /home/$NB_USER/work

COPY src /home/$NB_USER/replace-megaparsec/src
COPY replace-megaparsec.cabal /home/$NB_USER/replace-megaparsec/
COPY LICENSE /home/$NB_USER/replace-megaparsec/
RUN fix-permissions /home/$NB_USER/replace-megaparsec

USER $NB_UID

RUN cd /home/$NB_USER/work && stack build replace-megaparsec

ENV JUPYTER_ENABLE_LAB=yes
