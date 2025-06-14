# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG PARENT_IMAGE="pecan/models"

# ----------------------------------------------------------------------
# BUILD MODEL BINARY
# ----------------------------------------------------------------------
FROM ${PARENT_IMAGE}:${IMAGE_VERSION} AS model-binary

# download, unzip and build ed2
WORKDIR /src
COPY namelist.patch .
RUN git -c http.sslVerify=false clone https://bitbucket.org/remkoduursma/maespa.git \
    && cd maespa \
    && patch -p1 < ../namelist.patch \
    && make

########################################################################

# ----------------------------------------------------------------------
# BUILD PECAN FOR MODEL
# ----------------------------------------------------------------------
FROM ${PARENT_IMAGE}:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# INSTALL MODEL SPECIFIC PIECES
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC MODEL
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git

# Setup model_info file
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/maespa.${MODEL_VERSION}#g" /work/model.json

# COPY model binary
COPY --from=model-binary /src/maespa/maespa.out /usr/local/bin/maespa.${MODEL_VERSION}
