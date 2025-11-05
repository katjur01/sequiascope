
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
get_string_interactions <- function(proteins, species = 9606, chunk_size = 100, delay = 0.2, 
                                    required_score = NULL, filter_sources = NULL) {
  # 🔑 VALIDACE: Zkontrolovat, že proteins je validní
  if (is.null(proteins) || length(proteins) == 0) {
    return(data.frame())
  }
  
  # Odstranit NA, NULL a prázdné stringy
  proteins <- proteins[!is.na(proteins) & proteins != ""]
  
  if (length(proteins) == 0) {
    return(data.frame())
  }
  
  # Funkce pro odesílání jednotlivých požadavků
  # 🔑 CACHE KEY: Zahrnout i parametry filtru
  cache_key <- paste(c(sort(proteins), species, required_score, sort(filter_sources)), collapse = "|")
  
  # 🔑 BEZPEČNÁ KONTROLA cache - zkontrolovat, že cache_key je validní string
  if (!is.character(cache_key) || length(cache_key) != 1 || cache_key == "") {
    # Invalid cache key, skip cache
  } else if (exists(cache_key, envir = string_cache)) {
    return(get(cache_key, envir = string_cache))
  }
  
  
  fetch_interactions <- function(protein_chunk) {
    base_url <- "https://string-db.org/api/json/network?"
    query <- paste0("identifiers=", paste(protein_chunk, collapse = "%0D"), "&species=", species)
    
    # 🔑 NEPOUŽÍVÁME required_score pokud filtrujeme podle sources
    # required_score filtruje combined_score, ale my chceme filtrovat podle individuálních skóre
    # Použijeme required_score jen když jsou vybrané ALL sources
    if (is.null(filter_sources) || length(filter_sources) == 0 || ("all" %in% filter_sources)) {
      if (!is.null(required_score)) {
        score_value <- as.numeric(required_score) * 1000
        query <- paste0(query, "&required_score=", score_value)
      }
    }
    
    url <- paste0(base_url, query)
    
    Sys.sleep(delay)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      content <- fromJSON(content(response, as = "text"))
      
      # 🔑 VALIDACE: Zkontrolovat že content je data.frame a má řádky
      if (!is.data.frame(content) || is.null(content) || nrow(content) == 0) {
        return(data.frame())  # Vrátit prázdný data.frame
      }
      
      # 🔑 FILTROVAT podle sources a required_score
      if (!is.null(filter_sources) && length(filter_sources) > 0 && !("all" %in% filter_sources)) {
        content <- filter_by_sources(content, filter_sources, required_score)
      }
      
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

# 🔑 NOVÁ FUNKCE: Filtrování interakcí podle zdrojů
filter_by_sources <- function(interactions, sources, required_score = NULL) {
  # Mapování UI hodnot na STRING score sloupce
  source_map <- list(
    "experiments" = "escore",
    "databases" = "dscore",
    "textmining" = "tscore",
    "coexpression" = "ascore",
    "neighborhood" = "nscore",
    "gene_fusion" = "fscore",
    "cooccurrence" = "pscore"
  )
  
  # Získat relevantní score sloupce
  score_cols <- unlist(source_map[sources])
  score_cols <- score_cols[!is.na(score_cols)]
  
  if (length(score_cols) == 0) {
    return(interactions)
  }
  
  # 🔑 FILTROVÁNÍ PODLE SOURCES A SCORE
  # Určit threshold podle required_score - STRING používá škálu 0-1000, my 0-1
  threshold <- if (!is.null(required_score)) as.numeric(required_score) else 0
  
  # Filtrovat: zachovat řádky kde alespoň jeden vybraný source má score > threshold
  keep <- rep(FALSE, nrow(interactions))
  for (col in score_cols) {
    if (col %in% names(interactions)) {
      keep <- keep | (interactions[[col]] >= threshold)
    }
  }
  
  filtered <- interactions[keep, ]
  
  return(filtered)
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
prepare_cytoscape_network <- function(interactions, tab, proteins = NULL, selected_sources = NULL, required_score = 0.4, edge_mode = "evidence") {

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
    
    # Spočítání stupně (degree) pro každý uzel
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
    } else if (edge_mode == "confidence") {
      # 🔑 CONFIDENCE MODE: Jedna černá hrana s tloušťkou podle combined score
      edges_list <- list()
      
      for (i in 1:nrow(interactions)) {
        row <- interactions[i, ]
        
        # Vytvořit jednu hranu pokud combined score >= threshold
        if (row$score >= required_score) {
          edges_list[[length(edges_list) + 1]] <- data.frame(
            source = row$preferredName_A,
            target = row$preferredName_B,
            interaction = "pp",
            score = row$score,  # combined score (pro tloušťku i filtrování)
            source_score = row$score,  # stejné jako combined score
            source_type = "confidence",  # speciální typ pro černou barvu
            # Všechny individual scores (pro popup)
            escore = if("escore" %in% names(row)) row$escore else 0,
            dscore = if("dscore" %in% names(row)) row$dscore else 0,
            tscore = if("tscore" %in% names(row)) row$tscore else 0,
            ascore = if("ascore" %in% names(row)) row$ascore else 0,
            nscore = if("nscore" %in% names(row)) row$nscore else 0,
            fscore = if("fscore" %in% names(row)) row$fscore else 0,
            pscore = if("pscore" %in% names(row)) row$pscore else 0,
            stringsAsFactors = FALSE
          )
        }
      }
      
      edges <- do.call(rbind, edges_list)
      if (is.null(edges)) edges <- data.frame()
      
    } else {
      # 🔑 EVIDENCE MODE: Více hran pro každou interakci - jedna hrana pro každý nenulový zdroj
      source_map <- list(
        escore = "experiments",
        dscore = "databases",
        tscore = "textmining",
        ascore = "coexpression",
        nscore = "neighborhood",
        fscore = "gene_fusion",
        pscore = "cooccurrence"
      )
      
      edges_list <- list()
      
      for (i in 1:nrow(interactions)) {
        row <- interactions[i, ]
        
        # Pro každý nenulový source vytvořit samostatnou hranu
        for (score_col in names(source_map)) {
          source_type <- source_map[[score_col]]
          
          # 🔑 FILTROVAT: Pouze vybrané sources (pokud jsou specifikované)
          if (!is.null(selected_sources) && length(selected_sources) > 0) {
            if (!source_type %in% selected_sources) {
              next  # Přeskočit tento zdroj
            }
          }
          
          # 🔑 KLÍČOVÁ OPRAVA: Kontrolovat individual score proti threshold!
          # Vytvořit hranu pouze pokud individual score >= required_score
          if (score_col %in% names(row) && row[[score_col]] >= required_score) {
            edges_list[[length(edges_list) + 1]] <- data.frame(
              source = row$preferredName_A,
              target = row$preferredName_B,
              interaction = "pp",
              score = row$score,  # combined score (pro tloušťku)
              source_score = row[[score_col]],  # score konkrétního zdroje
              source_type = source_type,  # typ zdroje (pro barvu)
              # Všechny individual scores (pro popup)
              escore = if("escore" %in% names(row)) row$escore else 0,
              dscore = if("dscore" %in% names(row)) row$dscore else 0,
              tscore = if("tscore" %in% names(row)) row$tscore else 0,
              ascore = if("ascore" %in% names(row)) row$ascore else 0,
              nscore = if("nscore" %in% names(row)) row$nscore else 0,
              fscore = if("fscore" %in% names(row)) row$fscore else 0,
              pscore = if("pscore" %in% names(row)) row$pscore else 0,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      edges <- if (length(edges_list) > 0) {
        do.call(rbind, edges_list)
      } else {
        data.frame(
          source = character(0),
          target = character(0),
          interaction = character(0),
          stringsAsFactors = FALSE
        )
      }
      
    }  # Konec else bloku (evidence vs confidence mode)

  node_data <- node_data[match(proteins, node_data$id, nomatch = 0), ]
  
  edges <- edges[edges$source %in% proteins & edges$target %in% proteins, ]
  
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
get_pathway_list <- function(expr_tag, goi_dt = NULL, run = NULL) {

  # Bezpečná kontrola parametru run
  if (!is.null(run) && length(run) > 0 && run == "docker") {
    path <- "/input_files/kegg_tab.tsv"
  } else {
    path <- paste0(getwd(), "/input_files/kegg_tab.tsv")
  }

  
  # Zpracování pro ALL GENES
  if (identical(expr_tag, "all_genes")) {
    if (!file.exists(path)) {
      stop("Soubor ", path, " nebyl nalezen.")
    }
    dt <- fread(path)
    return(sort(unique(dt$kegg_paths_name)))
  }
  
  # Zpracování pro GENES OF INTEREST
  if (identical(expr_tag, "genes_of_interest")) {
    # 1️⃣ goi_dt existuje a má sloupec "pathway"
    if (!is.null(goi_dt) && "pathway" %in% colnames(goi_dt)) {
      return(sort(unique(goi_dt$pathway)))
    }
    
    # 2️⃣ goi_dt neobsahuje pathway, použij KEGG tabulku
    if (file.exists(path)) {
      dt <- fread(path)
      return(sort(unique(dt$kegg_paths_name)))
    } else {
      stop("Soubor ", path, " nebyl nalezen.")
    }
  }
  
  # Pokud expr_tag neodpovídá známým hodnotám
  warning("Invalid expr_tag. Please use 'all_genes' or 'genes_of_interest'.")
  return(character(0))
}

