# Dockerfile for usage examples hosted on mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t replace-megaparsec .
#     docker run --rm -p 8888:8888 -v $PWD:/home/jovyan/pwd --name replace-megaparsec --env JUPYTER_TOKEN=x replace-megaparsec:latest
#
# Note that the replace-megaparsec built in the docker image will be the one
# from the IHaskell Stackage resolver, not from the local file tree.

FROM crosscompass/ihaskell-notebook:ebff081e2cef

USER $NB_UID

RUN stack build replace-megaparsec

USER root

COPY notebook/ReplaceMegaparsecUsage.ipynb /home/$NB_USER/work/
RUN chown $NB_UID:users /home/$NB_USER/work/ReplaceMegaparsecUsage.ipynb

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
