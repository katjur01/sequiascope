box::use(  
  shiny[tags,tagList, icon, actionButton, textInput,HTML],
  reactable,
  reactable[colDef,JS,colGroup],
  htmltools[div,tags,tagAppendAttributes],
  stats[na.omit,setNames],
  bs4Dash[actionButton],
  htmltools[HTML,span, div, tagList],
  data.table[uniqueN]
)

box::use(
  app/logic/prepare_table[colFilter]
)


# Select input filter with an "All" default option
#' @export
selectFilter <- function(tableId, style = "width: 100%; height: 100%;") {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf("
        const value = event.target.value
        Reactable.setFilter('%s', '%s', value === '__ALL__' ? undefined : value)
      ", tableId, name),
      # "All" has a special value to clear the filter, and is the default option
      tags$option(value = "__ALL__", "All"),
      lapply(unique(values), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = style
    )
  }
}

# Min range filter input that handles NaNs
#' @export
minRangeFilter <- function(tableId, style = "width: 100%;") {
  function(values, name) {
    values <- na.omit(values)
    oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, name)
    tags$input(
      type = "range",
      min = floor(min(values)),
      max = ceiling(max(values)),
      value = floor(min(values)),
      oninput = oninput,
      style = style,
      "aria-label" = sprintf("Filter by minimum %s", name)
    )
  }
}

# Min value filter method that handles NaNs
#' @export
filterMinValue <- JS("(rows, columnId, filterValue) => {
  return rows.filter(row => {
    const value = row.values[columnId]
    return !isNaN(value) && value >= filterValue
  })
}")


#' @export
custom_colGroup_setting <- function(tag, tissues) {
  if (tag == "expression") {
    
    custom_colGroup <- lapply(tissues, function(tissue) {
      group_name <- gsub("_", " ", tissue)
      colGroup(name = group_name, columns = c(
        paste0("log2FC_", tissue),
        paste0("p_value_", tissue),
        paste0("p_adj_", tissue)))})
    
    return(custom_colGroup)
  }
  
  return(NULL)
}


set_pathway_colors <- function(){
  pathway_colors <- list(
      "RTK Signaling" = "#98FB98",
      "Metabolic Signaling" = "#00CED1",
      "Epigenetics" = "#DA70D6",
      "PI3K/AKT/mTOR Signaling" = "#008000",
      "Apoptosis" = "#FF7F00",
      "MAPK Signaling" = "#800000",
      "WNT signaling" = "#6495ED",
      "Hormone Signaling" = "#DC143C",
      "DNA damage/repair" = "#87CEEB",
      "Cell cycle control" = "#FB9A99",
      "Immune Checkpoints" = "#A9A9A9",
      "TGF-B Signaling" = "#FFD700",
      "JAK/STAT Signaling" = "#BDB76B",
      "Hedgehog Signaling" = "#8B008B",
      "Non-receptor kinases" = "#6A5ACD",
      "Kinase Fusions" = "#D2691E",
      # "non-WNT/non-SHH medulloblastoma-related markers" = "khaki2",
      "Immune Response" = "#4682B4")

  return(pathway_colors)
}

#' @export
create_clinvar_filter <- function(data, selected_clinvar_sig) {  #data[is.na(clinvar_sig) | clinvar_sig == "Benign",]
  if ("missing_value" %in% selected_clinvar_sig) {
    if (length(selected_clinvar_sig) == 1) {
      return(data[is.na(clinvar_sig) | trimws(clinvar_sig) == ""])
    } else {
      # Pokud jsou vybrány i jiné hodnoty
      other_terms <- selected_clinvar_sig[selected_clinvar_sig != "missing_value"]
      
      if (length(other_terms) > 0) {
        conditions <- sapply(other_terms, function(term) {
          escaped_term <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", term)
          paste0("(^|[|/_])", escaped_term, "($|[|/_])")
        })
        final_pattern <- paste(conditions, collapse = "|")
        
        # Kombinace missing values a pattern matching
        return(data[is.na(clinvar_sig) | trimws(clinvar_sig) == "" | 
                      (!is.na(clinvar_sig) & grepl(final_pattern, clinvar_sig, ignore.case = TRUE))])
      }
    }
    
  } else {
  
    # Normální případ - žádné "missing_value"
    conditions <- sapply(selected_clinvar_sig, function(term) {
      escaped_term <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", term)
      paste0("(^|[|/_])", escaped_term, "($|[|/_])")
    })
    
    final_pattern <- paste(conditions, collapse = "|")
    
    filtered_data <- data[!is.na(clinvar_sig) & 
                            grepl(final_pattern, clinvar_sig, ignore.case = TRUE)]
    return(filtered_data)
  }
}


#' @export
create_consequence_filter <- function(data, selected_consequences, include_missing = FALSE) {

  if(!"consequence_trimws" %in% names(data)) {
    data$consequence_trimws <- data$consequence
  }
  
  # Vytvoříme logický vektor pro filtrování
  keep_rows <- sapply(data$consequence_trimws, function(x) {
    if(is.null(x) || length(x) == 0) return(FALSE)
    
    # Pokud obsahuje missing_value
    if(any(x == "missing_value")) {
      # Pokud chceme zahrnout missing values, vracíme TRUE
      if(include_missing) return(TRUE)
      # Jinak kontrolujeme, jestli jsou i jiné hodnoty než missing_value
      non_missing_values <- x[x != "missing_value"]
      if(length(non_missing_values) == 0) return(FALSE)
      return(any(non_missing_values %in% selected_consequences))
    }
    # Standardní kontrola
    any(x %in% selected_consequences)
  })
  
  filtered_data <- data[keep_rows]
  
  return(filtered_data)
}


#' @export
update_fusion_data <- function(dt, check_state, notes_state_current, fusion_data_to_render) {
  if (!is.null(dt)) {
    dt$Visual_Check <- NA
    dt$Notes <- NA
    
    if (!is.null(check_state) && nrow(check_state) > 0) {
      valid_rows <- check_state$row[check_state$row <= nrow(dt)]
      dt$Visual_Check[valid_rows] <- check_state$value[check_state$row <= nrow(dt)]
    }
    
    if (!is.null(notes_state_current) && nrow(notes_state_current) > 0) {
      valid_rows <- notes_state_current$row[notes_state_current$row <= nrow(dt)]
      dt$Notes[valid_rows] <- notes_state_current$value[notes_state_current$row <= nrow(dt)]
    }
    
    fusion_data_to_render(dt)
  }
}