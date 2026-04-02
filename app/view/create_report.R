box::use(
  shiny[req,tagList,fileInput,conditionalPanel,reactive,reactiveValues,reactiveVal,downloadButton,icon,moduleServer,NS,downloadHandler,div,observe,observeEvent,reactiveValuesToList],
  flextable[flextable,theme_vanilla,bg,fontsize,border_remove,set_header_labels,set_table_properties,body_add_flextable,color,align,width,bold,nrow_part,italic,border],
  officer[cursor_reach,body_add_par,body_add_fpar,body_remove,fp_border,fp_text,fpar,ftext,read_docx,hyperlink_ftext],
  htmltools[tags,HTML],
  shinyWidgets[dropdown,prettyRadioButtons],
  shinyjs[useShinyjs,enable,disable,hide,show],
  data.table[data.table,as.data.table],
  bs4Dash[box],
  utils[str],
  openxlsx[read.xlsx]
)

myReport_theme <- function(ft) {
  ft |>
    theme_vanilla() |>
    color(part = "header", color = "white") |>
    bg(part = "header", bg = "#294779") |>
    bg(i = seq_len(nrow_part(ft, part = "body")), bg = "white", part = "body") |>
    bg(i = seq(1, nrow_part(ft, part = "body"), by = 2), bg = "#cfe2f3", part = "body") |>
    fontsize(size = 8, part = "all") |>
    align(align = "left", part = "header") |>
    align(align = "left", part = "body") |>
    border_remove()
}


ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    div(style = "font-size: 16px !important; font-weight: normal !important;",
      dropdown(right = TRUE, size = "sm", icon = icon("download"), style = "material-flat", width = "300px",
         prettyRadioButtons(
              inputId = ns("template_choice"),
              label = "Choose template:",
              choices = c("Use default template" = "default", "Upload custom template" = "custom"),
              selected = "default"
            ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("template_choice")),
            fileInput(ns("custom_template"), "Upload your template (.docx)", accept = ".docx")
         ),
        tags$div(
          id = ns("template_warning"),
          style = "display: none; color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; margin-bottom: 10px; font-size: 13px;",
          icon("exclamation-triangle"),
          " Default template not available. Upload template in Upload Data or select custom template."
        ),
        downloadButton(ns("download_report"), "Generate Report")
      )
    )
  )
}


server <- function(id, patient, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    noNA_text <- function(x) ifelse(is.na(x) | x == "", "-", x)

    mutation_load <- reactive({
      overview_som <- shared_data$somatic.overview[[ patient ]]
      if (!is.null(overview_som$TMB)){
        mut_load_str <- overview_som$TMB
      } else {
        mut_load_str <- NA
      }
      return(as.character(mut_load_str))
    })
    
    somatic_dt <- reactive({
      som_vars <- as.data.table(shared_data$somatic.variants())

      if (is.null(som_vars) || nrow(som_vars) == 0) {
      } else {
        patient_id <- patient  # Store in local variable to avoid column name collision
        som_vars <- som_vars[sample == patient_id]

        if (nrow(som_vars) == 0) {
          dt <- data.table(
            Gene        = character(),
            Transcript  = character(),
            variant     = character(),
            VAF         = character(),
            consequence = character(),
            Class       = character()
          )
        } else {
          dt <- data.table(
            Gene        = som_vars$gene_symbol,
            Transcript  = som_vars$feature,
            HGVSc       = noNA_text(som_vars$hgvsc),
            HGVSp       = noNA_text(som_vars$hgvsp),
            variant     = sprintf("%s\n(%s)", noNA_text(som_vars$hgvsc), noNA_text(som_vars$hgvsp)),
            VAF         = sprintf("%.1f%%", as.numeric(som_vars$tumor_variant_freq) * 100),
            consequence = som_vars$consequence,
            Class       = ""
          )
        }
       return(dt)
      }
    })
    preprare_somatic_dt <- function(somatic_dt) {
      ft <- flextable(somatic_dt, col_keys = c("Gene","Transcript","variant","VAF","consequence","Class"))
      ft <- set_header_labels(ft,
                              Gene = "Gene",
                              Transcript = "Transcript",
                              variant = "Variant",
                              VAF = "VAF",
                              consequence = "Variant effect",
                              Class = "Class")
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      ft <- width(ft, j = ~ VAF, width = 0.6)
      ft <- width(ft, j =  ~ variant, width = 0.8)
      ft <- width(ft, j =  ~ Class, width = 1.5)
      ft <- bold(ft, j = ~ Gene, bold = TRUE)
      ft
    }

    germline_dt <- reactive({
      germ_vars <- as.data.table(shared_data$germline.variants())
      
      if (is.null(germ_vars) || nrow(germ_vars) == 0) {
      } else {
        patient_id <- patient  # Store in local variable to avoid column name collision
        germ_vars <- germ_vars[sample == patient_id]
        if (nrow(germ_vars) == 0) {
          dt <- data.table(
            Gene        = character(),
            Transcript  = character(),
            variant     = character(),
            MAF         = numeric(),
            consequence = character(),
            Phenotype   = character(),
            Zygozity    = character(),
            Inheritance = character(),
            Class       = character()
          )
        } else {
          dt <- data.table(
            Gene        = germ_vars$gene_symbol,
            Transcript  = germ_vars$feature,
            HGVSc       = noNA_text(germ_vars$hgvsc),
            HGVSp       = noNA_text(germ_vars$hgvsp),
            variant     = sprintf("%s\n(%s)", noNA_text(germ_vars$hgvsc), noNA_text(germ_vars$hgvsp)),
            MAF         = germ_vars$gnomad_nfe,
            consequence = germ_vars$consequence,
            Phenotype   = "",
            Zygozity    = "",
            Inheritance = "",
            Class       = germ_vars$clinvar_sig
          )
        }
        return(dt)
      }
    })
    preprare_germline_dt <- function(germline_dt) {
      ft <- flextable(germline_dt, col_keys = c("Gene","Transcript","variant","MAF","consequence","Phenotype","Zygozity","Inheritance","Class"))
      ft <- set_header_labels(ft,
                              Transcript = "Transcript",
                              variant = "Variant",
                              consequence = "Variant effect")
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      ft <- width(ft, j = ~ MAF, width = 0.6)
      ft <- width(ft, j =  ~ Class, width = 0.8)
      ft <- width(ft, j =  ~ Phenotype, width = 1.5)
      ft <- bold(ft, j = ~ Gene, bold = TRUE)
      ft
    }
#       dt[,MAF := sprintf("%.5f%%", gnomAD_NFE * 100)]
#       # dt[,phenotype := c("Xeroderma pigmentosum, group D (AR), Trichothiodystrophy type 1, photosensitive (AR)",
#       #                   "Cerebrooculofacioskeletal syndrome type 2 (AR)")]

    
    
    fusion_dt <- reactive({
      fusion <- as.data.table(shared_data$fusion.variants())
      
      if (is.null(fusion) || nrow(fusion) == 0) {
      } else {
        patient_id <- patient  # Store in local variable to avoid column name collision
        fusion <- fusion[sample == patient_id]
        if (nrow(fusion) == 0) {
          dt <- data.table(
            gene1           = character(),
            transcript5     = character(),
            gene2           = character(),
            transcript3     = character(),
            overall_support = numeric(),
            phasing         = character()
          )
        } else {
          dt <- data.table(
            gene1           = fusion$gene1,
            transcript5     = "",
            gene2           = fusion$gene2,
            transcript3     = "",
            overall_support = fusion$overall_support,
            phasing         = noNA_text(fusion$arriba.reading_frame)
          )
        }
        return(dt)
      }
    })

    preprare_fusion_dt <- function(fusion_dt) {
      ft <- flextable(fusion_dt, col_keys = c("gene1","transcript5","gene2","transcript3","overall_support","phasing"))
      ft <- set_header_labels(ft,
                              gene1 = "Gene 5'",
                              transcript5 = "Transcript 5'",
                              gene2 ="Gene 3'",
                              transcript3 = "Transcript 3'",
                              overall_support = "Overall support",
                              phasing = "Phasing")

      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      # ft <- width(ft, j = c("gene1", "gene2"), width = 1)
      # ft <- width(ft, j = c("transcript5", "transcript3"), width = 1)
      ft <- width(ft, j = c("overall_support", "phasing"), width = 0.9)
      ft <- bold(ft, j = ~ gene1 + gene2, bold = TRUE)
      ft
    }
    
    
    expression_dt <- reactive({
      exp_goi <- as.data.table(shared_data$expression.variants.goi())
      exp_all <- as.data.table(shared_data$expression.variants.all())
      exp_genes <- unique(rbind(exp_goi, exp_all, use.names = TRUE, fill = TRUE))
      
      if (is.null(exp_genes) || nrow(exp_genes) == 0) {
      } else {
        patient_id <- patient  # Store in local variable to avoid column name collision
        exp_genes <- exp_genes[sample == patient_id]
        if (nrow(exp_genes) == 0) {
          dt <- data.table(
            Gene              = character(),
            Transcript        = character(),
            Expression_level  = character(),
            log2FC            = character(),
            pathway           = character(),
            treatment         = character()
          )
        } else {
          dt <- data.table(
            Gene              = exp_genes$feature_name,
            Transcript        = exp_genes$geneid,
            Expression_level  = "",
            log2FC            = exp_genes$log2FC,
            pathway           = noNA_text(exp_genes$pathway),
            treatment         = noNA_text("")
          )
        }
        return(dt)
      }
    })
    
    preprare_expression_dt <- function(expression_dt) {
      ft <- flextable(expression_dt, col_keys = c("Gene","Transcript","Expression_level","log2FC","treatment"))
      ft <- set_header_labels(ft,
                              Gene = "Gene",
                              Transcript = "Transcript",
                              Expression_level = "Expression level",
                              log2FC = "log2FC",
                              treatment = "Therapeutic option")
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      ft <- width(ft, j = ~ Expression_level, width = 0.8)
      ft <- width(ft, j =  ~ log2FC, width = 0.8)
      ft <- width(ft, j =  ~ treatment, width = 1.5)
      ft <- bold(ft, j = ~ Gene, bold = TRUE)
      ft
    }

#     
#     expression_dt <- reactive({
#       dt <- data.table(
#         gene = c("ALK", "DDR2", "EPHA3", "EPHA5", "FGF14", "KIT","CD79B", "LAG3", "PDCD1 (PD1)","DNMT3A", "EZH2", "MSH2", "MSH6", "PBRM1", "TERT", "TET2", "Valproic acid","BID"),
#         pathway = c(rep("Receptor Tyrosine Kinase/Growth Factor Signaling", 6),rep("Immune Checkpoints", 3),rep("Chromatin Remodeling/DNA Methylation", 8),"Apoptosis"),
#         expression_level = c("++++", "+", "+++", "++++++", "++++", "+++++","+", "++", "++++++","+++", "++++", "++++", "+++", "++", "++++", "++", "","+"),
#         FC = c(4.5, 2.0, 3.5, 8.44, 4.5, 5.0, 2.0, 2.5, 7.87, 3.5, 4.5, 4.5, 3.5, 2.5, 4.5, 2.5, 1.0, 2.0),
#         therapeutic_option = c("Crizotinib, Ceritinib, Alectinib, Lorlatinib","Regorafenib","","Ponatinib, Vandetanib","","Ponatinib, Sunitinib, Regorafenib, Imatinib, Ripretinib","","","","","","","","","","","Valproic acid","")
#       )
#       dt
#     })
#     
#     preprare_expression_dt <- function(expression_dt) {
#       ft <- flextable(expression_dt)
#       ft <- myReport_theme(ft)
#       ft <- set_table_properties(ft, width = 1, layout = "autofit")
#       ft <- width(ft, j = "therapeutic_option", width = 3)
#       ft <- bold(ft, j = ~ gene, bold = TRUE)
#       ft
#     }
#     

    
    details_dt <- reactive({
      # Sample data table (for future reference):
      # dt <- data.table(
      #   attribute = c("Specimen type", "Date of collection", "Number of biopsy",
      #                 "Cancer cell content", "Method used", "Library preparation",
      #                 "Sequencing device", "Date of sequencing"),
      #   germline = c("Peripheral blood", "7.9.2023", NA, NA, "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "11.9.2023"),
      #   somatic = c("FFPE tissue", "5.9.2023", "1391/23/1", "NA", "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "9.10.2023"),
      #   fusion = c("Peripheral blood", "7.9.2023", NA, NA, "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "11.9.2023"),
      #   expression = c("Frozen tissue", "5.9.2023", "1391/23", "NA", "Whole-transcriptome sequencing", "NEBNext Ultra II Directional RNA Library Prep Kit", "NextSeq 500", "31.10.2023")
      # )
      
      # Create empty table with attributes only
      dt <- data.table(
        attribute = c("Specimen type", "Date of collection", "Number of biopsy",
                      "Cancer cell content", "Method used", "Library preparation",
                      "Sequencing device", "Date of sequencing")
      )
      
      active_cols <- c()
    
      # Check which datasets were loaded (by checking if patient lists are not empty)
      if (length(shared_data$germline.patients()) > 0) {
        dt$germline <- ""
        active_cols <- c(active_cols, "germline")
      }
      if (length(shared_data$somatic.patients()) > 0) {
        dt$somatic <- ""
        active_cols <- c(active_cols, "somatic")
      }
      if (length(shared_data$fusion.patients()) > 0) {
        dt$fusion <- ""
        active_cols <- c(active_cols, "fusion")
      }
      if (length(shared_data$expression.patients()) > 0) {
        dt$expression <- ""
        active_cols <- c(active_cols, "expression")
      }
    
      return(dt)
    })
    
    preprare_details_dt <- function(details_dt) {
      # Define which columns will be active (without empty gaps)
      active_cols <- c("attribute",
                       if ("germline" %in% names(details_dt)) "germline" else NULL,
                       if ("somatic" %in% names(details_dt)) "somatic" else NULL,
                       if ("fusion" %in% names(details_dt)) "fusion" else NULL,
                       if ("expression" %in% names(details_dt)) "expression" else NULL)
      
      # Define borders
      top_bottom_border <- fp_border(color = "#294779", width = 1)
      
      # First create the flextable object
      ft <- flextable(details_dt, col_keys = active_cols)
      
      # Now we can safely get the last row
      last_row <- nrow_part(ft, part = "body")
      
      # Apply formatting - white header, alternating white-blue
      ft <- ft |>
        theme_vanilla() |>
        fontsize(size = 8, part = "all") |>
        align(align = "center", part = "header") |>
        bg(part = "header", bg = "white") |>
        bg(i = seq_len(last_row), bg = "white", part = "body") |>
        bg(i = seq(1, last_row, by = 2), bg = "#cfe2f3", part = "body") |>
        border(part = "header",
               border.top = top_bottom_border,
               border.bottom = top_bottom_border) |>
        border(part = "body",
               i = last_row,
               j = active_cols,
               border.bottom = top_bottom_border) |>
        bold(part = "header", bold = TRUE) |>
        width(j = "attribute", width = 1.5) |>
        set_table_properties(width = 1, layout = "autofit") |>
        set_header_labels(
          attribute = "",
          germline = "Germline variant calling",
          somatic = "Somatic variant calling",
          fusion = "Fusion gene detection",
          expression = "RNA expression profile"
        ) |>
        bold(j = "attribute", bold = TRUE) |>
        italic(j = "attribute", italic = TRUE)
      
      ft
    }

    # Path to the default template (reactive value)
    default_template_path <- shared_data$report_template_path
    
    # Check if default template is available (reactive)
    is_default_template_available <- reactive({
      path <- default_template_path()
      
      result <- !is.null(path) && nzchar(path) && file.exists(path)
      return(result)
    })
    
    # Enable/disable download button based on template availability
    # Use observe() to react to both template_choice AND template availability changes
    observe({
      # Wait for template_choice to be initialized
      if (is.null(input$template_choice) || length(input$template_choice) == 0) {
        return()
      }
      
      # Small delay to ensure DOM is ready (especially for dropdown content)
      shinyjs::delay(100, {
        if (input$template_choice == "default") {
          if (is_default_template_available()) {
            shinyjs::enable("download_report")
            shinyjs::hide("template_warning")
          } else {
            shinyjs::disable("download_report")
            shinyjs::show("template_warning")
          }
        } else {
          shinyjs::enable("download_report")
          shinyjs::hide("template_warning")
        }
      })
    })

    # Reactive expression to get the template path
    template_path <- reactive({
      if (input$template_choice == "default") {
        return(default_template_path())
      } else {
        # Check if a custom file was uploaded
        req(input$custom_template)
        return(input$custom_template$datapath)
      }
    })

    summary_somatic <- reactive({
      if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
        sum_som <- NA
      } else {
        sum_som <- sprintf("%s (%s/%s)", somatic_dt()$Gene, somatic_dt()$HGVSc, somatic_dt()$HGVSp)
      }
      return(sum_som)
    })
    
    summary_germline <- reactive({
      if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
        sum_germ <- NA
      } else {
        sum_germ <- sprintf("%s (%s/%s)", germline_dt()$Gene, germline_dt()$HGVSc, germline_dt()$HGVSp)
      }
      return(sum_germ)
    })

    summary_fusion <- reactive({
      if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
        sum_fus <- NA
      } else {
        sum_fus <- sprintf("%s::%s gene fusion", fusion_dt()$gene1, fusion_dt()$gene2)
      }
      return(sum_fus)
    })

    somatic_interpretation <- reactive({
      if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
        return(NULL)
      } else {
        # Create formatted paragraph with hyperlink in parentheses
        fpar_list <- lapply(1:nrow(somatic_dt()), function(i) {
          variant_text <- sprintf("%s/%s variant was found in the %s gene. ", 
                                  somatic_dt()$HGVSc[i], somatic_dt()$HGVSp[i], somatic_dt()$Gene[i])
          gene_url <- paste0("https://www.oncokb.org/gene/", somatic_dt()$Gene[i])
          
          fpar(
            ftext(variant_text),
            ftext("("),
            hyperlink_ftext(href = gene_url, text = gene_url),
            ftext(")")
          )
        })
        return(fpar_list)
      }
    })
    
    germline_interpretation <- reactive({
      if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
        return(NULL)
      } else {
        # Create formatted paragraph with hyperlink in parentheses
        fpar_list <- lapply(1:nrow(germline_dt()), function(i) {
          variant_text <- sprintf("%s/%s variant was found in the %s gene. ", 
                                  germline_dt()$HGVSc[i], germline_dt()$HGVSp[i], germline_dt()$Gene[i])
          gene_url <- paste0("https://www.oncokb.org/gene/", germline_dt()$Gene[i])
          
          fpar(
            ftext(variant_text),
            ftext("("),
            hyperlink_ftext(href = gene_url, text = gene_url),
            ftext(")")
          )
        })
        return(fpar_list)
      }
    })

  observe({

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report_",patient,"_",Sys.Date(), ".docx")
      },
      content = function(file) {
        print("Download handler triggered")
        print(template_path())

        doc <- read_docx(path = template_path())   # Load template
        note_style <- fp_text(font.size = 7, font.family = "Helvetica") # font for comments

        tryCatch({
          doc <- cursor_reach(doc, "<<patient_info>>")
          # Normaly here would be body_remove(doc), but also it would add empty row between titul in template
          # and text/table from placeholder which I dont want to. Solution is to add text before and after placeholder
          # and remove placeholder at last.
          #
          # Move the cursor to the placeholder <<patient_info>> in the document.
          # After all content (headings and text) has been inserted before this placeholder,
          # the placeholder itself is now removed to finalize the layout.
          doc <- body_add_par(doc, paste0("Patient ID: ", patient), pos = "before")
          doc <- body_add_par(doc, paste0("Diagnosis: ", ""), pos = "after")
          doc <- body_add_par(doc, paste0("Report date: ", format(Sys.Date(), "%d. %m. %Y")), pos = "after")

          doc <- cursor_reach(doc, "<<patient_info>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<patient_info>> was not found.")
        })

        tryCatch({
          doc <- cursor_reach(doc, "<<summary_somatic>>")
          if (!is.null(somatic_dt()) && nrow(somatic_dt()) != 0) {
            for (variant_text in rev(summary_somatic())) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_somatic>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<summary_somatic>> was not found. No pathogenic varints will not be added.")
        })

        tryCatch({
          doc <- cursor_reach(doc, "<<summary_germline>>")
          if (!is.null(germline_dt()) && nrow(germline_dt()) != 0) {
            for (variant_text in rev(summary_germline())) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_germline>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<<summary_germline>> was not found. No pathogenic varints will not be added.")
        })

        tryCatch({
          doc <- cursor_reach(doc, "<<summary_fusion>>")
          if (!is.null(fusion_dt()) && nrow(fusion_dt()) != 0) {
            for (variant_text in rev(summary_fusion())) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_fusion>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<summary_fusion>> was not found. No gene fusion will not be added.")
        })
        
        
        tryCatch({
          doc <- cursor_reach(doc, "<<details_table>>")
          
          message("📊 [DETAILS TABLE] Checking details_dt...")
          message("📊 [DETAILS TABLE] ncol: ", ncol(details_dt()))
          message("📊 [DETAILS TABLE] Column names: ", paste(names(details_dt()), collapse = ", "))
          message("📊 [DETAILS TABLE] nrow: ", nrow(details_dt()))
          
          # Tabulka se vytvoří vždy, protože aplikace musí mít alespoň jeden dataset
          message("📊 [DETAILS TABLE] Creating flextable with preprare_details_dt...")
          ft <- preprare_details_dt(details_dt())
          message("📊 [DETAILS TABLE] Flextable created, adding to document")
          doc <- body_add_flextable(doc, ft, pos = "before")
          message("📊 [DETAILS TABLE] Flextable added successfully")
          
          doc <- cursor_reach(doc, "<<details_table>>")
          doc <- body_remove(doc)
          message("📊 [DETAILS TABLE] Placeholder removed successfully")
        }, error = function(e) {
          message("❌ [DETAILS TABLE] ERROR: ", e$message)
          message("Placeholder <<details_table>> was not found or error occurred.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<somatic_table>>")
          if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
            doc <- body_add_par(doc, "No variants with known or potential clinical significance were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_somatic_dt(somatic_dt()[,-c("HGVSc","HGVSp")]), pos = "before")
          }
          doc <- cursor_reach(doc, "<<somatic_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {   # No placeholder in template
          message("Placeholder <<somatic_table>> was not found. Somatic table will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<germline_table>>")

          if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
            doc <- body_add_par(doc, "No variants with known or potential clinical significance in genes associated with hereditary cancer-predisposing syndromes were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_germline_dt(germline_dt()[,-c("HGVSc","HGVSp")]), pos = "before")
            # Vysvětlivka pod tabulkou
            doc <- body_add_fpar(doc, fpar(ftext("MAF – minor allele frequency – Non-Finnish European population (gnomAD database)", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("AD – autosomal dominant inheritance", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("AR – autosomal recessive inheritance", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("XLR – X-linked recessive", prop = note_style)), pos = "after")
          }
          doc <- cursor_reach(doc, "<<germline_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {   # No placeholder in template
            message("Placeholder <<germline_table>> was not found. Germline table will not be added.")
        })

        tryCatch({
          doc <- cursor_reach(doc, "<<fusion_table>>")

          if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
            doc <- body_add_par(doc, "No clinically relevant fusion genes were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_fusion_dt(fusion_dt()), pos = "before")
          }
          doc <- cursor_reach(doc, "<<fusion_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<fusion_table>> was not found. Fusion table will not be added.")
        })
        
        tryCatch({
          message("=== EXPRESSION TABLE DEBUG ===")
          message("expression_dt rows: ", if(!is.null(expression_dt())) nrow(expression_dt()) else "NULL")
          
          doc <- cursor_reach(doc, "<<expression_table>>")
          if (is.null(expression_dt()) || nrow(expression_dt()) == 0) {
            doc <- body_add_par(doc, "No deregulated genes were selected for this report.")
          } else {
            message("expression_dt columns: ", paste(colnames(expression_dt()), collapse=", "))
            if ("pathway" %in% colnames(expression_dt())) {
              message("Pathway values: ", paste(expression_dt()$pathway, collapse=", "))
            }
            pathways <- rev(unique(expression_dt()$pathway)) # Musím převrátit, jinak bude seznam pathways v opačném pořadí
            message("Unique pathways: ", paste(pathways, collapse=", "))
            message("Number of pathways: ", length(pathways))
            
            for (p in pathways) {
              message("Processing pathway: ", p)
              subset_dt <- expression_dt()[pathway == p,]
              message("  Subset rows: ", nrow(subset_dt))
              if (nrow(subset_dt) > 0) {
                message("  Genes in pathway: ", paste(subset_dt$Gene, collapse=", "))
                doc <- body_add_flextable(doc, preprare_expression_dt(subset_dt[,-c("pathway")]), pos = "before")
                doc <- body_add_par(doc, p, style = "Heading 5", pos = "before") # Přidej nadpis (podsekce) s názvem pathway
              } else {
                message("  WARNING: Subset is empty for pathway: ", p)
              }
            }
          }
          doc <- cursor_reach(doc, "<<expression_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("ERROR in expression table: ", e$message)
          message("Placeholder <<expression_table>> was not found. Expression profile table will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<mutation_burden>>")
          if (is.null(mutation_load()) || is.na(mutation_load())) {
            doc <- body_add_par(doc, "No data about mutational load were found.")
          } else {
            formatted_text <- fpar(
              ftext("Tumor mutation burden (load): ", prop = fp_text(bold = TRUE)),
              ftext(mutation_load(), prop = fp_text()),
              ftext(" mutations/Mb", prop = fp_text())
            )
            doc <- body_add_fpar(doc, formatted_text)
          }
          doc <- cursor_reach(doc, "<<mutation_burden>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<mutation_burden>> was not found. Mutational signatures table will not be added.")
        })
      
        tryCatch({
          doc <- cursor_reach(doc, "<<somatic_interpretation>>")

          if (!is.null(somatic_interpretation())) {
            for (fpar_obj in rev(somatic_interpretation())) {
              doc <- body_add_fpar(doc, fpar_obj, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<somatic_interpretation>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<somatic_interpretation>> was not found. No pathogenic varints will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<germline_interpretation>>")

          if (!is.null(germline_interpretation())) {
            for (fpar_obj in rev(germline_interpretation())) {
              doc <- body_add_fpar(doc, fpar_obj, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<germline_interpretation>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<germline_interpretation>> was not found. No pathogenic variants will be added.")
        })
        
        
        print(doc, target = file)
      }
    )
  })
  })
}

# shinyApp(ui, server, options = list(launch.browser = TRUE))
