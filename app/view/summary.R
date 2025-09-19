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

ui <- function(id){
  ns <- NS(id)
  hasData <- TRUE
  tagList(
    fluidRow(
      column(12,
         fluidRow(
      # div(class = "container-of-summary-boxes",
          div(class = "summary-box somatic-infobox",
              box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
                title = span("Somatic var call", class = "category"),
                tags$div(textOutput(ns("TMB"))),
                tags$div(textOutput(ns("for_review_som"))),
                icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
                style = "height:136px; overflow:auto;")
          ),
          div(class = "summary-box germline-infobox", #class = paste("summary-box germline-infobox", if(!hasData) "no_data" else ""),
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
              title = span("Germline var call", class = "category"),
              tags$div(textOutput(ns("clinvar_N_germ"))),
              tags$div(textOutput(ns("for_review_germ"))),
              icon = HTML('<span class="icon icon-gray" title="Analysis not available"><i class="fa-solid fa-circle-check"></i></span>'), #fa-circle-xmark
              style = "height:140px; overflow:auto;")
            ),
          div(class = "summary-box fusion-infobox",
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE,width = 12,
              title = span("Fusion genes", class = "category"),
              tags$div(textOutput(ns("high_confidence"))),
              tags$div(textOutput(ns("potencially_fused"))),
              icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
              style = "height:136px; overflow:auto;")
          ),
          div(class = "summary-box expression-infobox",
            box(elevation = 2, collapsible = FALSE, headerBorder = FALSE, color = "teal",width = 12,
              title = span("Expression profile", class = "category"),
              tags$div(textOutput(ns("tissues"))),
              tags$div(class = "item", "Over-expressed genes: "),
              tags$div(class = "item", "Under-expressed genes: "),
              tags$div(class = "item", "Altered pathways: "),
              icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
              style = "height:136px; overflow:auto;")
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

server <- function(id, patient, shared_data){ #,active_tab
  moduleServer(id, function(input, output, session){

#     #####################
#     ### Card overview ###
#     #####################
  
    output$tissues <- renderText({
      if (is.null(patient)) return("Tissue comparison: Not available")
      overview_exp  <- shared_data$expression.overview[[ patient ]]
      tissue_N <- unique(na.omit(trimws(overview_exp$tissues)))
      if (!length(tissue_N)) return("Tissue comparison: NA")
      if (length(tissue_N) == 1 && tissue_N[1] == "none") return("Tissue comparison: 0")
      paste0("Tissue comparison: ", sum(tissue_N != "none"))
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
          som_vars <- som_vars[grepl(patient, sample)]
  
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
                          noNA_text(variant$Gene_symbol),
                          noNA_text(variant$Consequence))),
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
                                      tags$p(sprintf("HGVSc: %s", variant$HGVSc)),
                                      tags$p(sprintf("HGVSp: %s", variant$HGVSp)),
                                      tags$p(sprintf("Variant type: %s", variant$variant_type))))),
                      column(3,
                             tags$p(strong("Frequency: ")),
                             tags$p(sprintf("Allelic: %s", variant$tumor_variant_freq)),
                             tags$p(sprintf("GnomAD: %s", variant$gnomAD_NFE))),
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
          germ_vars <- germ_vars[grepl(patient, sample)]
  
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
                       noNA_text(variant$Gene_symbol),
                       noNA_text(variant$Consequence),
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
                                      tags$p(sprintf("HGVSc: %s", variant$HGVSc)),
                                      tags$p(sprintf("HGVSp: %s", variant$HGVSp)),
                                      tags$p(sprintf("Variant type: %s", variant$variant_type))))),
                      column(3,
                             tags$p(strong("Frequency: ")),
                             tags$p(sprintf("Allelic: %s", variant$variant_freq)),
                             tags$p(sprintf("GnomAD: %s", variant$gnomAD_NFE))),
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
          fusion_vars <- fusion_vars[grepl(patient, sample)]
  
        if (nrow(fusion_vars) == 0) {
          return(tags$div("No fusion genes selected"))
        } else {
          boxes <- lapply(1:nrow(fusion_vars), function(i) {
            fusion <- fusion_vars[i, ]
            div(class = "fusion-box",
                box(solidHeader = TRUE, collapsed = TRUE, width = 12,
                    title = HTML(sprintf(
                        '<span style="font-size:16px; font-weight:bold;">%s</span>
                         <span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>
                         <span style="font-size:14px; font-weight:normal;">%s</span>',
                         paste0(fusion$gene1," - ", fusion$gene2),
                         noNA_text(fusion$arriba.confidence))),
                    fluidRow(
                      column(3,
                             tags$p(strong(sprintf("%s: ", fusion$gene1))),
                             tags$p(sprintf("ID: %s", "ENS")),
                             tags$p(sprintf("Position: %s", fusion$position1)),
                             tags$p(sprintf("Arriba site: %s", fusion$arriba.site1))),
                      column(3,
                             tags$p(strong(sprintf("%s: ", fusion$gene2))),
                             tags$p(sprintf("ID: %s", "ENS")),
                             tags$p(sprintf("Position: %s", fusion$position2)),
                             tags$p(sprintf("Arriba site: %s", fusion$arriba.site2))),
                      column(3,
                             tags$p(),
                             tags$p(sprintf("Frame: %s", "inframe or out-of-frame")),
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
          deregulated_genes <- deregulated_genes[grepl(patient, sample)]
          
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
    # })
      
    # })
  
  })
}

# ui <- fluidPage(
#   UI("xx")
# )
# server <- function(input, output, session){
#   SERVER("xx","DZ1601")
# }
# shinyApp(ui,server,options = list(launch.browser = TRUE))