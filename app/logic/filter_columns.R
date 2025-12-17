# app/logic/filter_columns.R

box::use(
  shiny[icon,textInput],
  bs4Dash[actionButton],
  htmltools[div,tags,tagList],
  reactable,
  reactable[colDef,JS,colGroup,colFormat],
  stats[setNames], #na.omit,
  shinyWidgets[radioGroupButtons],
  # data.table[uniqueN]
)

box::use(
  app/logic/prepare_table[colFilter]
)



#' Format column name for display (replace . and _ with space)
format_column_name <- function(col_name) {
  gsub("[._]", " ", col_name)
}

#' Expand wildcard column selections (for expression tissue columns)
#' @export
expand_expression_columns <- function(selected_columns, all_columns) {
  expanded <- character(0)
  
  for (col in selected_columns) {
    if (col == "log2fc_*") {
      # Expand to all log2fc_tissue columns
      expanded <- c(expanded, grep("^log2fc_", all_columns, value = TRUE))
    } else if (col == "p_value_*") {
      # Expand to all p_value_tissue columns
      expanded <- c(expanded, grep("^p_value_", all_columns, value = TRUE))
    } else if (col == "p_adj_*") {
      # Expand to all p_adj_tissue columns
      expanded <- c(expanded, grep("^p_adj_", all_columns, value = TRUE))
    } else {
      # Regular column, keep as is
      expanded <- c(expanded, col)
    }
  }
  
  return(unique(expanded))
}

#' Contract tissue-specific columns to wildcards (reverse of expand)
#' @export
contract_expression_columns <- function(columns) {
  contracted <- character(0)
  has_log2fc <- FALSE
  has_p_value <- FALSE
  has_p_adj <- FALSE
  
  for (col in columns) {
    if (grepl("^log2fc_", col)) {
      has_log2fc <- TRUE
    } else if (grepl("^p_value_", col)) {
      has_p_value <- TRUE
    } else if (grepl("^p_adj_", col)) {
      has_p_adj <- TRUE
    } else {
      # Non-tissue column, keep as is
      contracted <- c(contracted, col)
    }
  }
  
  # Add wildcards for tissue columns
  if (has_log2fc) contracted <- c(contracted, "log2fc_*")
  if (has_p_value) contracted <- c(contracted, "p_value_*")
  if (has_p_adj) contracted <- c(contracted, "p_adj_*")
  
  return(unique(contracted))
}

#' Find columns that exist in data but are not in map_list
#' @export
find_extra_columns <- function(actual_columns, map_list) {
  mapped_columns <- names(map_list)
  extra_cols <- setdiff(actual_columns, mapped_columns)
  return(extra_cols)
}

#' @export
map_checkbox_names <- function(map_list, all_actual_columns = NULL, is_expression = FALSE){
  
  # Special handling for expression: group tissue-specific columns
  if (is_expression && !is.null(all_actual_columns)) {
    
    # Static columns (non-tissue)
    static_cols <- c("feature_name", "geneid", "refseq_id", "type", "gene_definition", 
                     "all_kegg_gene_names", "pathway", "num_of_paths", "mean_log2fc")
    existing_static <- intersect(static_cols, all_actual_columns)
    
    # Get static column display names from map_list
    static_display_names <- character(0)
    for (col in existing_static) {
      if (!is.null(map_list[[col]])) {
        name <- map_list[[col]]$name %||% map_list[[col]]$header %||% format_column_name(col)
        static_display_names <- c(static_display_names, setNames(col, name))
      }
    }
    
    # Tissue-specific columns: create grouped entries
    # Find all tissue columns in data
    has_log2fc <- any(grepl("^log2fc_", all_actual_columns))
    has_p_value <- any(grepl("^p_value_", all_actual_columns))
    has_p_adj <- any(grepl("^p_adj_", all_actual_columns))
    
    tissue_display_names <- character(0)
    
    # Create single entries that represent ALL tissues for each metric
    if (has_log2fc) {
      tissue_display_names <- c(tissue_display_names, setNames("log2fc_*", "log2FC (all tissues)"))
    }
    if (has_p_value) {
      tissue_display_names <- c(tissue_display_names, setNames("p_value_*", "p-value (all tissues)"))
    }
    if (has_p_adj) {
      tissue_display_names <- c(tissue_display_names, setNames("p_adj_*", "p-adj (all tissues)"))
    }
    
    # Combine static and grouped tissue columns
    choices <- c(static_display_names, tissue_display_names)
    return(choices)
  }
  
  # Standard handling for non-expression datasets
  # Získej display names z map_list
  map_display_names <- sapply(map_list, function(x) {
    if (!is.null(x$name)) {
      x$name
    } else if (!is.null(x$header)) {
      x$header
    } else {
      NA_character_
    }
  })
  
  map_display_names <- map_display_names[!is.na(map_display_names)]
  
  # Pokud jsou poskytnuty skutečné sloupce, filtruj mapované a přidej extra sloupce
  if (!is.null(all_actual_columns)) {
    mapped_columns <- names(map_display_names)
    
    # Filtruj mapované sloupce - ponechej jen ty, co existují v datech
    existing_mapped <- intersect(mapped_columns, all_actual_columns)
    filtered_map_display_names <- map_display_names[existing_mapped]
    
    # Najdi extra sloupce (v datech, ale nejsou v map_list)
    # all_actual_columns UŽ jsou vyfiltrované (bez hidden columns) z colFilter
    extra_columns <- setdiff(all_actual_columns, mapped_columns)
    
    if (length(extra_columns) > 0) {
      # Pro extra sloupce: formátuj název (. a _ → mezera)
      extra_display_names <- sapply(extra_columns, format_column_name)
      names(extra_display_names) <- extra_columns
      
      # Zkombinuj mapované (filtrované) a extra sloupce
      all_display_names <- c(filtered_map_display_names, extra_display_names)
    } else {
      all_display_names <- filtered_map_display_names
    }
  } else {
    all_display_names <- map_display_names
  }
  
  # Vytvoř choices: setNames(internal_name, display_name)
  choices <- setNames(names(all_display_names), all_display_names)
  
  return(choices)
}


#' @export
generate_columnsDef <- function(column_names, selected_columns, tag, map_list) {
  
  # Definuj permanentně skryté sloupce podle tagu
  hide <- switch(tag,
                 "fusion" = c("sample", "png_path", "svg_path", "has_png", "has_svg","arriba.confidence"),
                 "germline" = c("sample"),
                 "somatic" = c("sample"),
                 "expression" = c("sample"),
                 character(0))
  
  if (length(hide) == 0) {
    message("No column has been selected for permanent hiding")
  }
  
  column_defs <- lapply(column_names, function(col) {
    
    # 1️⃣ Permanentně skryté sloupce
    if (col %in% hide) {
      return(colDef(show = FALSE))
    }
    
    # 2️⃣ Pokud je sloupec vybrán uživatelem
    if (col %in% selected_columns) {
      
      # Získej definici z map_list (může být NULL pro extra sloupce)
      map_def <- map_list[[col]]
      
      # Nastavit header
      if (!is.null(map_def)) {
        # Sloupec je v map_list - použij jeho definici
        header_name <- if (!is.null(map_def$name)) {
          map_def$name
        } else if (!is.null(map_def$header)) {
          map_def$header
        } else {
          format_column_name(col)  # fallback
        }
        
        map_def$header <- header_name
        return(do.call(colDef, map_def))
        
      } else {
        # Extra sloupec (není v map_list)
        header_name <- format_column_name(col)
        return(colDef(show = TRUE, header = header_name))
      }
    }
    
    # 3️⃣ Pokud není vybrán uživatelem, skryj
    colDef(show = FALSE)
  })
  
  names(column_defs) <- column_names
  return(column_defs)
}

#' @export
colnames_map_list <- function(tag, all_columns = NULL, session = NULL, tissues = NULL) {
  if (tag == "somatic"){
    map_list <- list(
      var_name = colDef(sticky='left', minWidth=140,filterable = TRUE, name = 'Variant name'),
      in_library = colDef(sticky='left',header = "In library"),
      alarm = colDef(name="Alarm"),
      full_annot_name = colDef(name="Annotated name",minWidth = 240,),
      var_gen_coord = colDef(name="Variant coordinates",minWidth = 240,filterable=TRUE),
      variant_type = colDef(filterable = TRUE, name = 'Variant type',minWidth=110),
      gene_symbol = colDef(sticky = 'left',filterable = TRUE, minWidth=110,name = 'Gene symbol'),
      hgvsp = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
      hgvsc = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
      tumor_variant_freq = colDef(name="Tumor variant frequency",minWidth=200, filterable = TRUE),
      tumor_depth = colDef(name = "Tumor depth",minWidth=110),
      normal_variant_freq = colDef(name="Normal variant frequency",minWidth=200),
      normal_depth = colDef(name = "Normal depth",minWidth=120),
      called_by	= colDef(name="Called by"),
      `1000g_eur_af` = colDef(name="1000G EUR AF",minWidth=130),
      gnomad_nfe = colDef(name = "GnomAD NFE",filterable=FALSE,minWidth=110),
      snpdb = colDef(name="SnpDB",filterable=TRUE),
      cosmic = colDef(name="COSMIC",filterable=TRUE),
      nhlbi_esp = colDef(name="NHLBI ESP",minWidth=110),
      clinvar_sig = colDef(name = "ClinVar significance", filterable = TRUE,minWidth=180),
      clinvar_dbn = colDef(show=TRUE,name="ClinVar DBN",filterable = TRUE,minWidth=110),
      `md-anderson` = colDef(name="MD Anderson",minWidth=110),
      trusight_genes = colDef(name="TruSight genes",minWidth=130),
      cgc_somatic = colDef(name="CGC Somatic",minWidth=120, html = TRUE,
                           cell = JS("function(rowInfo) {
                                  if (rowInfo.value == 'yes') {
                                    var cls = 'db-' + rowInfo.value.toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
      fone = colDef(width = 100, name = "fOne", html = TRUE,
                    cell = JS("function(rowInfo) {
                                  if (rowInfo.value == 'yes') {
                                    var cls = 'db-' + rowInfo.value.toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
      cgc_tumour_somatic = colDef(name="CGC Tumour",minWidth=110),
      polyphen = colDef(name="PolyPhen",minWidth=190),
      sift = colDef(name="SIFT",minWidth=180),
      gene_region = colDef(name="Gene region",filterable = TRUE, minWidth=110),
      hgmd = colDef(name="HGMD"),
      impact = colDef(name="Impact",filterable=TRUE),
      consequence = colDef(name = "Consequence",minWidth=140,filterable=TRUE),
      exon = colDef(name = "Exon"),
      intron = colDef(name = "Intron"),
      feature = colDef(name = "Feature",minWidth=180,filterable=TRUE),
      feature_type = colDef(name = "Feature type",filterable=TRUE,minWidth=110),
      `annotation source` = colDef(name = "Annotation source",minWidth=160),
      gene = colDef(name = "Gene"),
      all_full_annot_name = colDef(name = "Full annotated name", minWidth = 240,show=TRUE)
    )
  } else if (tag == "germline"){
    map_list <- list(
      # original germline #
      var_name = colDef(sticky='left', minWidth=140,filterable = TRUE, name = 'Variant name'),
      in_library = colDef(sticky='left', maxWidth = 100, header = "In library"),
      variant_freq = colDef(filterable = TRUE, minWidth = 100, name = "Variant frequency"),
      gene_symbol = colDef(sticky = 'left',filterable = TRUE, minWidth=110,name = 'Gene symbol'),
      coverage_depth = colDef(maxWidth = 100,filterable = TRUE, name = "Coverage depth"),
      gene_region = colDef(filterable = TRUE, minWidth=110, name="Gene region"),
      gnomad_nfe = colDef(minWidth = 140,maxWidth = 150,filterable = TRUE,name = "GnomAD NFE"),
      clinvar_sig = colDef(minWidth = 180,filterable = TRUE, name = "ClinVar significance", html = TRUE,
                           cell = JS("function(rowInfo) {
                                if (rowInfo.value == null || rowInfo.value == '' || rowInfo.value == 'NA') {
                                  return '';  // Do not render anything for NA/null values
                                }
                                
                                // Split by '/' and process each part
                                var parts = rowInfo.value.split('/');
                                var spans = '';
                                
                                for (var i = 0; i < parts.length; i++) {
                                  var v_trimmed = parts[i].trim();
                                  var class_name = 'clinvar-tag clinvar-' + v_trimmed.toLowerCase().replace(/ /g, '_');
                                  spans += '<span class=\"' + class_name + '\">' + v_trimmed + '</span>';
                                }
                                return '<div>' + spans + '</div>'; }")),
      consequence = colDef(minWidth = 170,filterable = TRUE,name = "Consequence"),
      hgsvp = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
      hgsvc = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
      all_full_annot_name = colDef(name = "Full annotated name", minWidth = 160),
      snpdb = colDef(maxWidth = 120,filterable = TRUE,name="SnpDB",
                     # header = function(value) {
                     #   tagList(value, tags$a(
                     #     href = "https://www.ncbi.nlm.nih.gov/clinvar/",
                     #     target = "_blank",
                     #     icon("external-link-alt", lib = "font-awesome"),
                     #     style = "margin-left: 6px; color: #007bff; text-decoration: none;"
                     #     ))}
      ),
      cgc_germline = colDef(width = 130,name="CGC Germline", html = TRUE,
                            cell = JS("function(rowInfo) {
                                  if (rowInfo.value == 'yes') {
                                    var cls = 'db-' + rowInfo.value.toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
      trusight_genes = colDef(width = 140,name="TruSight genes",
                              html = TRUE,
                              cell = JS("function(rowInfo) {
                                  if (rowInfo.value == 'yes') {
                                    var cls = 'db-' + rowInfo.value.toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
      fone = colDef(width = 100, name = "fOne", html = TRUE,
                        cell = JS("function(rowInfo) {
                                  if (rowInfo.value == 'yes') {
                                    var cls = 'db-' + rowInfo.value.toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
      occurance_in_cohort = colDef(width = 170,name = "Occurence in cohort"),
      in_samples = colDef(minWidth = 120,name = "In samples"),
      alarm = colDef(minWidth = 120,name="Alarm"),
      var_gen_coord = colDef(minWidth = 170,name="Variant coordinates",filterable=TRUE),
      variant_type = colDef(minWidth = 120,filterable = TRUE, name = "Variant type"),
      genotype = colDef(minWidth = 100,filterable = TRUE,name = "Genotype"),
      called_by = colDef(minWidth = 120, name="Called by"),
      `1000g_eur_af` = colDef(width = 130, name="1000G EUR AF"),
      cosmic = colDef(minWidth = 120, name="COSMIC",filterable=TRUE),
      hgmd = colDef(minWidth = 120, name="HGMD"),
      nhlbi_esp = colDef(minWidth = 120, name="NHLBI ESP"),
      clinvar_dbn = colDef(name="ClinVar DBN",filterable = TRUE,minWidth=110),
      bronco = colDef(width = 100,name="BRONCO"),
      `md-anderson` = colDef(width = 130,name="MD Anderson"),
      cgc_tumour_germline = colDef(minWidth = 180,filterable = TRUE,name="CGC tumour germline"),
      polyphen = colDef(minWidth = 120,name="PolyPhen"),
      feature = colDef(minWidth = 140,filterable = TRUE,name = "Feature"),
      feature_type = colDef(minWidth = 130,name = "Feature type"),
      `annotation source` = colDef(minWidth = 170,name = "Annotation source"),
      gene = colDef(minWidth = 150,filterable = TRUE, name = "Gene"),
      sift = colDef(minWidth = 120,name="SIFT"),
      cadd_raw = colDef(width = 100,name="CADD raw"),
      cad_phred = colDef(width = 120,name="CADD phred"),
      impact = colDef(minWidth = 110,filterable = TRUE,name="Impact"),
      somatic = colDef(minWidth = 110,name="Somatic"),
      pheno = colDef(minWidth = 110,name="Phenotype"),
      gene_pheno = colDef(width = 150,name="Gene phenotype"),
      pubmed = colDef(minWidth = 150,name="PubMed"),
      exon = colDef(width = 110,name="Exon"),
      intron = colDef(width = 110,name="Intron")
      )
    } else if (tag == "fusion"){
      map_list <- list(
        gene1 = colDef(minWidth = 120,filterable = TRUE,sticky = "left",name="Gene 1"),
        gene2 = colDef(minWidth = 120,filterable = TRUE,sticky = "left",name="Gene 2"),
        arriba.called = colDef(width = 110,name="Arriba called", html = TRUE,
                               cell = JS("function(rowInfo) {
                                  if (rowInfo.value == true || rowInfo.value == false) {
                                    var cls = 'tag called-' + String(rowInfo.value).toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                  }
                                  return rowInfo.value }")),
        starfus.called = colDef(width = 110,name="StarFusion called", html = TRUE,
                                cell = JS("function(rowInfo) {
                                   if (rowInfo.value == true || rowInfo.value == false) {
                                    var cls = 'tag called-' + String(rowInfo.value).toLowerCase()
                                    return '<div class=\"' + cls + '\">' + rowInfo.value + '</div>'
                                   }
                                  return rowInfo.value }")),
        arriba.confidence = colDef(show = FALSE),
        arriba.confidence_sort = colDef(width = 140,filterable = TRUE,name = "Arriba confidence",html = TRUE,
                                        cell = JS("function(rowInfo) {
                                  // Získáme původní textovou hodnotu z arriba.confidence
                                  var confidence = rowInfo.row['arriba.confidence'];
                                  
                                  if (confidence == null || confidence == '' || confidence == 'NA') {
                                    return '';
                                  }
                                  var cls = 'tag confidence-' + confidence.toLowerCase();
                                  return '<div class=\"' + cls + '\">' + confidence + '</div>'; }")),
        overall_support = colDef(width = 100,name="Overall support"),
        arriba.reading_frame = colDef(width = 110,name="Reading frame"),
        visual_check = colDef(width = 110,name="Visual check",html = TRUE,
                              cell = JS(paste0("function(cellInfo) {
                                                const rowIndex = cellInfo.index;
                                                    const value = cellInfo.value || '';
                                                    const inputId = '", session$ns("visual_check"), "';
                                                    return `
                                                      <div class='fusion-radio-group' data-row='${rowIndex}'>
                                                        <label class='fusion-radio-label'>
                                                          <input type='radio' name='${inputId}_${rowIndex}' value='yes' ${value === 'yes' ? 'checked' : ''}
                                                          onclick='Shiny.setInputValue(\"", session$ns("visual_check_changed"), "\", {row: ${rowIndex}, value: \"yes\"}, {priority: \"event\"})'>
                                                          <span class='fusion-radio-btn'><i class='fa fa-check'></i></span>
                                                        </label>
                                                        <label class='fusion-radio-label'>
                                                          <input type='radio' name='${inputId}_${rowIndex}' value='no' ${value === 'no' ? 'checked' : ''}
                                                           onclick='Shiny.setInputValue(\"", session$ns("visual_check_changed"), "\", {row: ${rowIndex}, value: \"no\"}, {priority: \"event\"})'>
                                                          <span class='fusion-radio-btn'><i class='fa fa-times'></i></span>
                                                        </label>
                                                      </div>
                                                `;}"))),
        notes = colDef(minWidth = 120,name="Notes", html = TRUE,
                       cell = JS(paste0("
                          function(cellInfo) {
                            const rowIndex = cellInfo.index;
                            const value = cellInfo.value || '';
                            const inputId = '", session$ns("notes_input"), "';
                            return `
                              <input type='text' value='${value}' 
                                     onblur='Shiny.setInputValue(\"", session$ns("notes_input_changed"),"\", {row: ${rowIndex}, value: this.value}, {priority: \"event\"})' 
                                     style='width: 100%; box-sizing: border-box;' />`;}"))),
        position1 = colDef(minWidth = 150,name="Position 1"),
        position2 = colDef(minWidth = 150,name="Position 2"),
        strand1 = colDef(width = 100,name="Strand 1"),
        strand2 = colDef(width = 100,name="Strand 2"),
        arriba.site1 = colDef(minWidth = 120,filterable = TRUE,name="Arriba site 1"),
        arriba.site2 = colDef(minWidth = 120,filterable = TRUE,name="Arriba site 2"),
        starfus.splice_type = colDef(minWidth = 140,name="StarFus splice type"),
        db_count = colDef(maxWidth = 100,name="DB count"),
        db_list = colDef(minWidth = 100,name="DB list"),
        arriba.split_reads = colDef(width = 110,name="Arriba split reads"),
        arriba.discordant_mates = colDef(width = 160,name="Arriba discordant mates"),
        arriba.break_coverage = colDef(width = 120,name="Arriba break coverage"),
        arriba.break2_coverage = colDef(width = 120,name="Arriba break coverage 2"),
        starfus.split_reads = colDef(width = 120,name="StarFusion split reads"),
        starfus.discordant_mates = colDef(width = 160,name="StarFus discordant mates"),
        starfus.counter_fusion1 = colDef(width = 150,name="StarFusion counter fusion 1"),
        starfus.counter_fusion2 = colDef(width = 150,name="StarFusion counter fusion 2"),
        arriba.break_seq = colDef(minWidth = 120,name="Arriba break sequence"),
        starfus.break_seq = colDef(minWidth = 130,name="StarFusion break sequence")
      )
      
  } else if (tag == "expression"){
    
    # Static columns (non-tissue specific)
    static_columns <- list(
      feature_name = colDef(name = "Gene name", sticky = "left", minWidth = 140, filterable = TRUE),
      geneid = colDef(name = "Gene ID", minWidth = 110, filterable = TRUE),
      refseq_id = colDef(name = "RefSeq ID", minWidth = 110),
      type = colDef(name = "Type", minWidth = 100),
      gene_definition = colDef(name = "Gene definition", minWidth = 200),
      all_kegg_gene_names = colDef(name = "KEGG gene names", minWidth = 180),
      pathway = colDef(name = "Pathway", minWidth = 150),
      num_of_paths = colDef(name = "Pathway (n)", minWidth = 100),
      mean_log2fc = colDef(name = "Mean log2FC", minWidth = 120, format = colFormat(digits = 3))
    )
    
    # Dynamic tissue-specific columns
    dynamic_columns <- list()
    
    if (!is.null(tissues) && length(tissues) > 0) {
      tissue_list <- tissues
      log2fc_cols <- paste0("log2fc_", tissue_list)
      p_value_cols <- paste0("p_value_", tissue_list)
      p_adj_cols <- paste0("p_adj_", tissue_list)
      
      # Create colDef for each tissue column with simplified names (just metric type)
      for (tissue in tissue_list) {
        # Determine if this is the first tissue (for border styling)
        is_first <- tissue == tissue_list[1]
        
        # log2fc column
        col_log2fc <- paste0("log2fc_", tissue)
        dynamic_columns[[col_log2fc]] <- colDef(
          name = "log2FC",  # Simple name without tissue
          minWidth = 100,
          format = colFormat(digits = 3),  # Format as number with 3 decimals
          style = JS(sprintf("function(rowInfo, colInfo) {
            var value = rowInfo.values[colInfo.id];
            var color = value > 1 ? '#FFE9E9' : (value < -1 ? '#E6F7FF' : '#FFFFFF');
            return { backgroundColor: color, borderLeft: '%s' };
          }", ifelse(is_first, "2px solid black", "")))
        )
        
        # p_value column
        col_p_value <- paste0("p_value_", tissue)
        dynamic_columns[[col_p_value]] <- colDef(
          name = "p-value",  # Simple name without tissue
          minWidth = 100,
          cell = JS("function(cellInfo) {
            var val = cellInfo.value;
            if (val === null || val === undefined || val === '') return '';
            var num = typeof val === 'number' ? val : parseFloat(val);
            if (isNaN(num)) return val;
            return num.toExponential(3);
          }"),
          style = JS("function(rowInfo, colInfo) {
            var value = rowInfo.values[colInfo.id];
            var num = typeof value === 'number' ? value : parseFloat(value);
            var color = (!isNaN(num) && num <= 0.05) ? '#FFEFDE' : '#FFFFFF';
            return { backgroundColor: color };
          }")
        )
        
        # p_adj column
        col_p_adj <- paste0("p_adj_", tissue)
        dynamic_columns[[col_p_adj]] <- colDef(
          name = "p-adj",  # Simple name without tissue
          minWidth = 100,
          cell = JS("function(cellInfo) {
            var val = cellInfo.value;
            if (val === null || val === undefined || val === '') return '';
            var num = typeof val === 'number' ? val : parseFloat(val);
            if (isNaN(num)) return val;
            return num.toExponential(3);
          }"),
          style = JS("function(rowInfo, colInfo) {
            var value = rowInfo.values[colInfo.id];
            var num = typeof value === 'number' ? value : parseFloat(value);
            var color = (!isNaN(num) && num <= 0.05) ? '#E7FAEF' : '#FFFFFF';
            return { backgroundColor: color, borderRight: '1px dashed rgba(0,0,0,0.3)' };
          }")
        )
      }
    }
    
    map_list <- c(static_columns, dynamic_columns)
   

  } else {
    print("NOT germline, expression or fusion")
  }
  return(map_list)
}

