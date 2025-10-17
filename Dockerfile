FROM rocker/shiny:latest

# System dependencies for R and compilation
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgeos-dev \
    libglpk-dev \
    libgit2-dev \
    build-essential \
    ca-certificates \
  && rm -rf /var/lib/apt/lists/*

# Node.js and NPM instalation (LTS 22)
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    node -v && npm -v

# Setting working directory and copying
RUN mkdir -p /srv/shiny-server/sequiaViz
WORKDIR /srv/shiny-server/sequiaViz
COPY . /srv/shiny-server/sequiaViz

# Install R dependencies
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN Rscript /srv/shiny-server/sequiaViz/Dependencies.R

# Install NPM dependencies
COPY package*.json ./
RUN npm ci || npm install
RUN node -v && npm -v #(check versions)


EXPOSE 8080

CMD ["R", "-e", "rhino::build_sass()"]
CMD ["R", "-e", "shiny::shinyAppDir('.', options = list(launch.browser = TRUE))"]