
# classic installation of required packages
install.packages(c(
  "rhino", "shiny", "reactable", "shinyjs", "bs4Dash", "htmltools", "BiocManager","devtools","plotly","shinyWidgets", "networkD3", "ggplot2", "dplyr",
  "data.table","openxlsx", "billboarder", "webshot", "shinycssloaders", "processx","future","shinyalert","shinyFiles","pheatmap","flextable","readxl",
  "vcfR"
))

# installation of specific versions which were used during development
# install.packages("remotes")
# remotes::install_version("shiny", version = "1.10.0")
# remotes::install_version("rhino", version = "1.11.0")
# remotes::install_version("reactable", version = "0.4.4")
# remotes::install_version("shinyjs", version = "2.1.0")
# remotes::install_version("bs4Dash", version = "2.3.4")
# remotes::install_version("htmltools", version = "0.5.8.1")
# remotes::install_version("shinyWidgets", version = "0.9.0")
# remotes::install_version("networkD3", version = "0.4.1")
# remotes::install_version("ggplot2", version = "3.5.2")
# remotes::install_version("dplyr", version = "1.1.4")
# remotes::install_version("data.table", version = "1.17.0")
# remotes::install_version("openxlsx", version = "4.2.8")
# remotes::install_version("billboarder", version = "0.5.0")
# remotes::install_version("webshot", version = "0.5.5")
# remotes::install_version("shinycssloaders", version = "1.1.0")
# remotes::install_version("processx", version = "3.8.6")

remotes::install_version("BiocManager", version = "")
remotes::install_version("devtools", version = "")
remotes::install_version("webshot2", version = "")
remotes::install_version("plotly", version = "")
remotes::install_version("tools", version = "")




# wget https://data.broadinstitute.org/igv/projects/downloads/2.19/IGV_Linux_2.19.5_WithJava.zip

#renv::snapshot()