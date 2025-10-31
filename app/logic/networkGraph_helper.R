
box::use(
  jsonlite[fromJSON, toJSON],
  data.table[fread,setnames],
  httr[GET, status_code, content],
  stats[setNames]
)
box::use(
  app/logic/load_data[get_inputs]
)

string_cache <- new.env(parent = emptyenv())

# Funkce pro získání interakcí mezi proteiny z STRING API
#' @export
get_string_interactions <- function(proteins, species = 9606, chunk_size = 100, delay = 0.2) {
  # Funkce pro odesílání jednotlivých požadavků
  
  cache_key <- paste(sort(proteins), collapse = "|")
  if (exists(cache_key, envir = string_cache)) {
    message("Using cached STRING interactions")
    return(get(cache_key, envir = string_cache))
  }
  
  
  fetch_interactions <- function(protein_chunk) {
    base_url <- "https://string-db.org/api/json/network?"
    query <- paste0("identifiers=", paste(protein_chunk, collapse = "%0D"), "&species=", species)
    url <- paste0(base_url, query)
    
    Sys.sleep(delay)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      content <- fromJSON(content(response, as = "text"))
      return(content)
    } else {
      stop("Request failed with status: ", status_code(response))
    }
  }
  
  # Rozdělení proteinů na bloky podle chunk_size (občas je proteinů moc)
  protein_chunks <- split(proteins, ceiling(seq_along(proteins) / chunk_size))
  all_interactions <- do.call(rbind, lapply(protein_chunks, fetch_interactions))
  
  assign(cache_key, all_interactions, envir = string_cache)
  
  return(all_interactions)
}

# library(data.table)
# library(httr)
# library(jsonlite)
# subTissue_dt <- fread("input_files/MOII_e117/RNAseq21_NEW/MR1507/Blood_all_genes_oneRow.tsv")
# synchronized_nodes <- c("CS","TP53")
# current_nodes <- c("CS","TP53")
# synchronized_nodes <- c("TP53")
# current_nodes <- c("TP53")
# interactions <- get_string_interactions(unique(subTissue_dt[feature_name %in% synchronized_nodes,feature_name]))
# proteins <- current_nodes
# fc_values <- unique(subTissue_dt[feature_name %in% current_nodes, log2FC])
# tab <- unique(subTissue_dt[feature_name %in% current_nodes, .(feature_name,log2FC)])


#' @export
prepare_cytoscape_network <- function(interactions, tab, proteins = NULL) {

    if(is.null(proteins)) proteins <- tab[,feature_name]

    interaction_nodes <- unique(c(interactions$preferredName_A, interactions$preferredName_B))
    all_nodes <- unique(c(interaction_nodes, proteins))

    log2FC_map <- setNames(tab[,log2FC], tab[,feature_name])
    log2FC_values <- sapply(all_nodes, function(node) {
      if (node %in% names(log2FC_map)) {
        log2FC_map[node]
      } else {
        NA  # Hodnota NA pro uzly mimo tabulku
      }
    }, USE.NAMES = FALSE)

    names(log2FC_values) <- all_nodes
    
    # Spočítání stupně (degree) pro každý uzel - singletony mají stupen 0
    degrees <- table(c(interactions$preferredName_A, interactions$preferredName_B))
    degree_values <- sapply(all_nodes, function(x) ifelse(x %in% names(degrees), degrees[x], 0))

    node_data <- data.frame(
      id = all_nodes,
      name = all_nodes,
      label = all_nodes,
      log2FC = log2FC_values,
      degree = degree_values,  # Přidání stupně (degree) uzlu
      stringsAsFactors = FALSE
    )

    if (is.null(interactions) || !is.data.frame(interactions) || nrow(interactions) == 0) {
      edges <- data.frame(
        source = character(0),
        target = character(0),
        interaction = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      edges <- data.frame(
        source = interactions$preferredName_A,
        target = interactions$preferredName_B,
        interaction = "interaction",
        stringsAsFactors = FALSE
      )
    }

  node_data <- node_data[match(proteins, node_data$id, nomatch = 0), ]
  edges <- edges[edges$source %in% proteins & edges$target %in% proteins, ]
  
  # json_data <- list(
  #   elements = list(
  #     nodes = lapply(seq_len(nrow(node_data)), function(i) {
  #       list(data = as.list(node_data[i, ]))
  #     }),
  #     edges = lapply(seq_len(nrow(edges)), function(i) {
  #       list(data = as.list(edges[i, ]))
  #     })
  #   )
  # )
  # 
  # network_json <- toJSON(json_data, auto_unbox = TRUE)
  # return(network_json)
  
  json_data <- list(
    elements = list(
      nodes = lapply(seq_len(nrow(node_data)), function(i) {
        list(data = as.list(node_data[i, ]))
      }),
      edges = lapply(seq_len(nrow(edges)), function(i) {
        list(data = as.list(edges[i, ]))
      })
    )
  )

  return(json_data)
}

#' @export
get_pathway_list <- function(expr_tag, goi_dt = NULL){
  if(expr_tag == "genes_of_interest") {
    if(!is.null(goi_dt$pathway)) return(sort(unique(goi_dt$pathway)))
  }
  tryCatch({
    dt <- fread("input_files/kegg_tab.tsv")
    return(sort(unique(dt$kegg_paths_name)))
    
  }, error = function(e) {
    return(message("Invalid expr_tag. Please use 'all_genes' or 'genes_of_interest'."))
  })

}
