# Dockerfile for usage examples hosted on mybinder.org
# from https://hub.docker.com/r/crosscompass/ihaskell-notebook
#
# Test this Dockerfile:
#
#     docker build -t replace-megaparsec .
#     docker run --rm -p 8888:8888 -v $PWD:/home/jovyan/pwd --name replace-megaparsec replace-megaparsec:latest jupyter lab --LabApp.token=''
#

FROM crosscompass/ihaskell-notebook:62631e7176e8

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
