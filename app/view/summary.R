box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,span,textOutput,renderText,req,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
  reactable,
  reactable[colDef],
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite, uniqueN],
  openxlsx[read.xlsx,getSheetNames],
  htmltools[hr,strong,h5],
  stats[na.omit]
  # shiny.gosling
  # plotly[renderPlotly, plot_ly,plotlyOutput]
  # plotly[plot_ly,plotlyOutput,renderPlotly],
  # magrittr,
  # data.table,htmltools,
  # billboarder[bb_donutchart,billboarderOutput,renderBillboarder]
)




ui <- function(id, dataset_availability = NULL){
  ns <- NS(id)
  
  # Helper function to get icon based on availability
  get_icon_html <- function(dataset_name) {
    if (is.null(dataset_availability) || isTRUE(dataset_availability[[dataset_name]])) {
      HTML('<span class="icon icon-available" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>')
    } else {
      HTML('<span class="icon icon-unavailable" title="Analysis not available"><i class="fa-solid fa-circle-xmark"></i></span>')
    }
  }
  
  # Helper function to get box class based on availability
  get_box_class <- function(dataset_name, base_class) {
    if (is.null(dataset_availability) || isTRUE(dataset_availability[[dataset_name]])) {
      base_class
    } else {
      paste(base_class, "no-data")
    }
  }
  
  tagList(
    tags$head(
      tags$style(HTML("
        .somatic-infobox .category {
          color: #7ac143;
          font-size: 23px;
          font-weight: bold;
        }
        .germline-infobox .category {
          color: #45818e;
          font-size: 23px;
          font-weight: bold;
        }
        .fusion-infobox .category {
          color: #f1c232;
          font-size: 23px;
          font-weight: bold;
        }
        .expression-infobox .category {
          color: #3c78d8;
          font-size: 23px;
          font-weight: bold;
        }
        .summary-box.no-data .elevation-2 {
          background-color: #f5f5f5 !important;
          border: 2px solid #d0d0d0 !important;
        }
        .summary-box.no-data .category {
          color: #6c757d !important;
        }
      "))
    ),
    fluidRow(
      column(12,
         fluidRow(
      # div(class = "container-of-summary-boxes",
          div(class = get_box_class("somatic", "summary-box somatic-infobox"),
              box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
                title = span("Somatic var call", class = "category"),
                tags$div(textOutput(ns("TMB"))),
                tags$div(textOutput(ns("for_review_som"))),
                style = "height:140px; overflow:auto;")
          ),
          div(class = get_box_class("germline", "summary-box germline-infobox"),
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
              title = span("Germline var call", class = "category"),
              tags$div(textOutput(ns("clinvar_N_germ"))),
              tags$div(textOutput(ns("for_review_germ"))),
              style = "height:140px; overflow:auto;")
            ),
          div(class = get_box_class("fusion", "summary-box fusion-infobox"),
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
              title = span("Fusion genes", class = "category"),
              tags$div(textOutput(ns("high_confidence"))),
              tags$div(textOutput(ns("potencially_fused"))),
              style = "height:140px; overflow:auto;")
          ),
          div(class = get_box_class("expression", "summary-box expression-infobox"),
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE, color = "teal",width = 12,
              title = span("Expression profile", class = "category"),
              tags$div(textOutput(ns("tissues"))),
              tags$div(textOutput(ns("for_review_expr"))),
              tags$div(textOutput(ns("altered_pathways"))),
              style = "height:140px; overflow:auto;")
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        hr(),
        uiOutput(ns("somatic_boxes")),
        hr(),
        uiOutput(ns("germline_boxes")),
        hr(),
        uiOutput(ns("fusion_boxes")),
        hr(),
        uiOutput(ns("expression_box"))
      )
    )
    
  )
}

server <- function(id, patient, shared_data, dataset_availability = NULL){ #,active_tab
  moduleServer(id, function(input, output, session){

#     #####################
#     ### Card overview ###
#     #####################
  
    output$tissues <- renderText({
      if (is.null(patient)) return("Tissue comparison: Not available")
      overview_exp  <- shared_data$expression.overview[[ patient ]]
      if (is.null(overview_exp$tissues) || !nzchar(overview_exp$tissues)) return("Tissue comparison: NA")
      
      # Rozdělit tkáně čárkou a spočítat
      tissue_list <- trimws(unlist(strsplit(overview_exp$tissues, ",")))
      tissue_list <- tissue_list[tissue_list != "none" & nzchar(tissue_list)]
      
      if (!length(tissue_list)) return("Tissue comparison: 0")
      paste0("Tissue comparison: ", length(tissue_list))
    })
    
    output$for_review_expr <- renderText({
      if (is.null(patient)) return("Deregulated genes: Not available")
      overview_expr  <- shared_data$expression.overview[[ patient ]]
      if (is.null(overview_expr$for_review_expr)) {
        return("Deregulated genes: NA")
      } else {
        paste("Deregulated genes:", overview_expr$for_review_expr)
      }
    })
    
    output$altered_pathways <- renderText({
      if (is.null(patient)) return("Pathways for review: Not available")
      overview_expr  <- shared_data$expression.overview[[ patient ]]
      if (is.null(overview_expr$altered_pathways)) {
        return("Pathways for review: NA")
      } else {
        paste("Pathways for review:", overview_expr$altered_pathways)
      }
    })
    
    output$for_review_som <- renderText({
      if (is.null(patient)) return("Variants for review: Not available")
      overview_som  <- shared_data$somatic.overview[[ patient ]]
      if (is.null(overview_som$for_review)) {
        return("Variants for review: NA")
      } else {
        paste("Variants for review: ", overview_som$for_review)
      }
    })
    
    output$TMB <- renderText({
      if (is.null(patient)) return("Tumor mutation burden: Not available")
      overview_som  <- shared_data$somatic.overview[[ patient ]]
      if (!is.null(overview_som$TMB)){
        paste("Tumor mutation burden (load):", overview_som$TMB)
      } else {
        "Tumor mutation burden: N/A"
      }
    })
    
    output$for_review_germ <- renderText({
      if (is.null(patient)) return("Variants for review: Not available")
      overview_germ <- shared_data$germline.overview[[ patient ]]
      if (is.null(overview_germ$for_review)) {
        return("Variants for review: NA")
      } else {
        paste("Variants for review: ", overview_germ$for_review)
      }
    })

    output$clinvar_N_germ <- renderText({
      if (is.null(patient)) return("Pathogenic and likely-pathogenic variants: Not available")
      overview_germ <- shared_data$germline.overview[[ patient ]]
      if (is.null(overview_germ$clinvar_N)) {
        return("Pathogenic and likely-pathogenic variants: NA")
      } else {
        paste("Pathogenic and likely-pathogenic variants: ", overview_germ$clinvar_N)
      }
    })


    output$high_confidence <- renderText({
      if (is.null(patient)) return("Fused genes with high confidence: Not available")
      overview_fus  <- shared_data$fusion.overview[[ patient ]]
      if (is.null(overview_fus$high_confidence)) {
        return("Fused genes with high confidence: NA")
      } else {
        paste("Fused genes with high confidence: ", overview_fus$high_confidence)
      }
    })

    output$potencially_fused <- renderText({
      if (is.null(patient)) return("Potencially fused genes: Not available")
      overview_fus  <- shared_data$fusion.overview[[ patient ]]
      if (is.null(overview_fus$potencially_fused)) {
        return("Potencially fused genes: NA")
      } else {
        paste("Potencially fused genes: ", overview_fus$potencially_fused)
      }
    })


    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################

    noNA_text <- function(x) ifelse(is.na(x) | x == "", "-", x)

      output$somatic_boxes <- renderUI({
        som_vars <- as.data.table(shared_data$somatic.variants())
  
        if (is.null(som_vars) || nrow(som_vars) == 0) {
          tags$div("No somatic variants selected")
        } else {
          patient_id <- patient  # Store in local variable to avoid column name collision
          som_vars <- som_vars[sample == patient_id]
  
          if (nrow(som_vars) == 0) {
            return(tags$div("No somatic variants selected"))
          } else {
            boxes <- lapply(1:nrow(som_vars), function(i) {
              variant <- som_vars[i, ]
              var_name_split <- unlist(strsplit(variant$var_name, "_"))
              allele_change <- unlist(strsplit(var_name_split[3],"/"))
              
              div(class = "somatic-box",
                  box(solidHeader = TRUE, collapsed = TRUE, width = 12,
                      title = HTML(sprintf(
                          '<span style="font-size:16px; font-weight:bold;">%s</span>
                           <span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>
                           <span style="font-size:14px; font-weight:normal;">%s</span>',
                          noNA_text(variant$gene_symbol),
                          noNA_text(variant$consequence))),
                    fluidRow(
                      column(3,
                             tags$p(strong("Variant info: ")),
                             tags$p(sprintf("Position: %s", sprintf("chr%s:%s", var_name_split[1], var_name_split[2]))),
                             fluidRow(
                               column(2, tags$p(sprintf("Ref: %s", allele_change[1]))),
                               column(2, tags$p(sprintf("Alt: %s", allele_change[2]))))),
                      column(3,
                             fluidRow(
                               column(6,
                                      tags$p(""),
                                      tags$p(sprintf("HGVSc: %s", variant$hgvsc)),
                                      tags$p(sprintf("HGVSp: %s", variant$hgvsp)),
                                      tags$p(sprintf("Variant type: %s", variant$variant_type))))),
                      column(3,
                             tags$p(strong("Frequency: ")),
                             tags$p(sprintf("Allelic: %s", variant$tumor_variant_freq)),
                             tags$p(sprintf("GnomAD: %s", variant$gnomad_nfe))),
                      column(3)
                      )))})
            tagList(boxes)
          }
        }
      })

      output$germline_boxes <- renderUI({
        germ_vars <- as.data.table(shared_data$germline.variants())
  
        if (is.null(germ_vars) || nrow(germ_vars) == 0) {
          tags$div("No germline variants selected")
        } else {
          patient_id <- patient  # Store in local variable to avoid column name collision
          germ_vars <- germ_vars[sample == patient_id]
  
        if (nrow(germ_vars) == 0) {
          return(tags$div("No germline variants selected"))
        } else {
          boxes <- lapply(1:nrow(germ_vars), function(i) {
            variant <- germ_vars[i, ]
            var_name_split <- unlist(strsplit(variant$var_name, "_"))
            allele_change <- unlist(strsplit(var_name_split[3],"/"))
            div(class = "germline-box",
                box(solidHeader = TRUE, collapsed = TRUE, width = 12,
                    title = HTML(sprintf(
                      '<span style="font-size:16px; font-weight:bold;"> %s </span>
                       <span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>
                       <span style="font-size:14px; font-weight:normal;"> %s </span>
                       <span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>
                       <span style="font-size:14px; font-weight:normal;"> %s </span>',
                       noNA_text(variant$gene_symbol),
                       noNA_text(variant$consequence),
                      noNA_text(variant$clinvar_sig)
                    )),
                    fluidRow(
                      column(3,
                             tags$p(strong("Variant info: ")),
                             tags$p(sprintf("Position: %s", sprintf("chr%s:%s", var_name_split[1], var_name_split[2]))),
                             fluidRow(
                               column(2, tags$p(sprintf("Ref: %s", allele_change[1]))),
                               column(2, tags$p(sprintf("Alt: %s", allele_change[2]))))),
                      column(3,
                             fluidRow(
                               column(6,
                                      tags$p(),
                                      tags$p(sprintf("HGVSc: %s", variant$hgvsc)),
                                      tags$p(sprintf("HGVSp: %s", variant$hgvsp)),
                                      tags$p(sprintf("Variant type: %s", variant$variant_type))))),
                      column(3,
                             tags$p(strong("Frequency: ")),
                             tags$p(sprintf("Allelic: %s", variant$variant_freq)),
                             tags$p(sprintf("GnomAD: %s", variant$gnomad_nfe))),
                      column(3)
                )))})
          tagList(boxes) # Vrátíme seznam boxů jako tagList
          }
        }
      })
    # })

      output$fusion_boxes <- renderUI({
        fusion_vars <- as.data.table(shared_data$fusion.variants())
  
        if (is.null(fusion_vars) || nrow(fusion_vars) == 0) {
          tags$div("No fusion genes selected")
        } else {
          patient_id <- patient  # Store in local variable to avoid column name collision
          fusion_vars <- fusion_vars[sample == patient_id]
  
        if (nrow(fusion_vars) == 0) {
          return(tags$div("No fusion genes selected"))
        } else {
          boxes <- lapply(1:nrow(fusion_vars), function(i) {
            fusion <- fusion_vars[i, ]
            # Convert ordered factor to character to avoid numeric conversion
            confidence_text <- as.character(fusion$arriba.confidence)
            div(class = "fusion-box",
                box(solidHeader = TRUE, collapsed = TRUE, width = 12,
                    title = HTML(sprintf(
                        '<span style="font-size:16px; font-weight:bold;">%s</span>
                         <span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>
                         <span style="font-size:14px; font-weight:normal;">%s</span>',
                         paste0(fusion$gene1," - ", fusion$gene2),
                         noNA_text(confidence_text))),
                    fluidRow(
                      column(3,
                             tags$p(strong(sprintf("%s: ", fusion$gene1))),
                             tags$p(sprintf("Position: %s", fusion$position1)),
                             tags$p(sprintf("Arriba site: %s", fusion$arriba.site1))),
                      column(3,
                             tags$p(strong(sprintf("%s: ", fusion$gene2))),
                             tags$p(sprintf("Position: %s", fusion$position2)),
                             tags$p(sprintf("Arriba site: %s", fusion$arriba.site2))),
                      column(3,
                             tags$p(),
                             tags$p(sprintf("Frame: %s", noNA_text(fusion$arriba.reading_frame))),
                             tags$p(sprintf("Coverage: %s", fusion$overall_support))),
                      column(3)
                    )))})
          tagList(boxes)
         }
        }
      })

      output$expression_box <- renderUI({
        exp_goi <- as.data.table(shared_data$expression.variants.goi())
        exp_all <- as.data.table(shared_data$expression.variants.all())
        deregulated_genes <- unique(rbind(exp_goi, exp_all, use.names = TRUE, fill = TRUE))
        
        if (is.null(deregulated_genes) || nrow(deregulated_genes) == 0) {
          tags$div("None of the deregulated genes will be reported.")
        } else {
          patient_id <- patient  # Store in local variable to avoid column name collision
          deregulated_genes <- deregulated_genes[sample == patient_id]
          
          if (nrow(deregulated_genes) == 0) {
            return(tags$div("No deregulated genes have been selected"))
          } else {
            div(class = "expression-box",
                box(solidHeader = TRUE,  collapsible = FALSE,  width = 12,
                    title = HTML(sprintf('<span style="font-size:16px; font-weight:normal;">In total, </span>
                                          <span style="font-size:16px; font-weight:bold;">%s</span>
                                          <span style="font-size:16px; font-weight:normal;"> deregulated genes have been selected for report.</span>',
                                          uniqueN(deregulated_genes$geneid)))
                    ))
          }
        }
      })
      
  })
}

# ui <- fluidPage(
#   UI("xx")
# )
# server <- function(input, output, session){
#   SERVER("xx","DZ1601")
# }
# shinyApp(ui,server,options = list(launch.browser = TRUE))