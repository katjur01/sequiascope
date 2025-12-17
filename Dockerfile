# -----------------------------------------------------------
# STAGE 1 — Node.js pro instalaci JS knihoven (Cytoscape, IGV)
# -----------------------------------------------------------
FROM node:22 AS frontend-builder
WORKDIR /build

# Zkopíruj pouze package.json a nainstaluj závislosti
COPY ./package*.json ./
RUN npm ci

# Zkopíruj potřebné JS knihovny (Cytoscape, pluginy, IGV)
RUN mkdir -p /dist/js && \
    cp ./node_modules/cytoscape/dist/cytoscape.min.js /dist/js/ && \
    cp ./node_modules/cytoscape-cola/cytoscape-cola.js /dist/js/ && \
    cp ./node_modules/cytoscape-fcose/cytoscape-fcose.js /dist/js/ && \
    cp ./node_modules/cytoscape-panzoom/cytoscape-panzoom.js /dist/js/ && \
    cp ./node_modules/igv/dist/igv.min.js /dist/js/

# -----------------------------------------------------------
# STAGE 2 — Shiny aplikace (hlavní runtime)
# -----------------------------------------------------------
FROM rocker/shiny:latest


# Systémové závislosti pro R, IGV Desktop a snapshoty
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgeos-dev \
    libglpk-dev \
    libgit2-dev \
    libwebp-dev \
    libwebpdemux2 \
    libwebpmux3 \
    build-essential \
    ca-certificates \
    lsof \
    curl \
    poppler-utils \
    chromium \
    chromium-driver \
  && rm -rf /var/lib/apt/lists/*

# Nastavení proměnné prostředí pro Chromium (pro webshot2)
ENV CHROMOTE_CHROME=/usr/bin/chromium


# Vytvoř adresář a nastav pracovní prostředí
RUN mkdir -p /srv/shiny-server/sequiaScope
WORKDIR /srv/shiny-server/sequiaScope

# Zkopíruj Shiny aplikaci
COPY . /srv/shiny-server/sequiaScope

# Zkopíruj předem připravené JS knihovny z Node buildu
COPY --from=frontend-builder /dist/js ./app/static/js

# Instalace R balíčků
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org')"
RUN Rscript /srv/shiny-server/sequiaScope/dependencies.R

# Nastavení portu a spuštění
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=8080)"]
