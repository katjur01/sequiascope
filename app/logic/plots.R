box::use(
  data.table[setnames,data.table],
  plotly[plot_ly,layout,add_segments,add_trace,add_text,add_annotations],
  magrittr[`%>%`],
  ggplot2[ggplot,scale_color_manual,geom_hline,geom_vline,ggtitle,theme_bw,ggsave,aes,geom_point,facet_wrap,theme,element_text,scale_x_continuous,unit],
  # ggiraph[geom_point_interactive,geom_text_repel_interactive,girafe,opts_hover,opts_tooltip,opts_sizing,opts_zoom],
  # promises[`%...>%`,catch],
  # future[future],

)


#############################
##   create volcano plot   ##
#############################
#' @export
prepare_volcano <- function(dt, tissue) {
  # Dynamically select relevant columns for the given tissue
  
  fc_col <- paste0("log2fc_", tissue)
  pval_col <- paste0("p_value_", tissue)
  padj_col <- paste0("p_adj_", tissue)
  
  # Check if columns exist (in case a tissue is missing)
  if (!(fc_col %in% colnames(dt)) || !(pval_col %in% colnames(dt)) || !(padj_col %in% colnames(dt))) {
    stop(paste("Missing columns for tissue:", tissue))
  }
  # Create new data table with universal column names
  dt_tissue <- dt[, .(feature_name, geneid,#pathway,
                      log2FC = as.numeric(as.character(get(fc_col))),
                      pval = as.numeric(as.character(get(pval_col))),
                      padj = as.numeric(as.character(get(padj_col))))]
  
  # Remove invalid values
  dt_tissue <- dt_tissue[!is.na(log2FC) & !is.infinite(log2FC) & !is.nan(log2FC)]
  
  # Add tissue name as new column
  dt_tissue[, tissue := tissue]

  return(dt_tissue)
}
#' @export
classify_volcano_genes <- function(dt, padj_cutoff = 0.05, logfc_cutoff = 1) {
  

  dt[, abs.logfc := abs(log2FC)]
  dt[, sig := "na"]  # Default color

  dt[is.na(padj), sig := "na"]
  dt[padj >= padj_cutoff, sig := "nsig"]  # Not significant
  dt[padj < padj_cutoff & log2FC > -logfc_cutoff & log2FC < logfc_cutoff, sig := "sig"]  # Significant
  dt[padj < padj_cutoff & log2FC <= -logfc_cutoff, sig := "down"]  # Downregulated
  dt[padj < padj_cutoff & log2FC >= logfc_cutoff, sig := "up"]  # Upregulated

  # dt[, abs_logfc := abs(log2FC)]
  # setorder(dt, padj, pvalue, -abs_logfc, na.last = T)
  # 
  # dt[, `:=`(sig="na", significant="NA")] #set default as grey and not significant
  # dt[padj>=padj_cutoff, `:=`(sig="nsig", significant="Non significant")]
  # dt[padj<padj_cutoff & log2FC>-(logfc_cutoff) & log2FC<logfc_cutoff, `:=`(sig="sig", significant="Significant")]
  # dt[padj<padj_cutoff & log2FC<=-(logfc_cutoff) , `:=`(sig="down", significant="Down regulated")] #down regulated DE genes
  # dt[padj<padj_cutoff & log2FC>=logfc_cutoff , `:=`(sig="up", significant="Up regulated")] #up regulated DE genes
  # dt[, significant := factor(significant,levels = c("Significant","Down regulated","Up regulated","Non significant","NA"))]

  return(dt)
}



#' @export
volcanoPlot <- function(dt, tissue, top_n = 10, padj_cutoff = 0.05, logfc_cutoff = 1) {
  dt <- dt[!is.na(log2FC) & !is.na(padj)]
  dt <- classify_volcano_genes(dt, padj_cutoff = padj_cutoff, logfc_cutoff = logfc_cutoff)  # Gene classification with custom cutoffs
  dt[,neg_log10_padj := -log10(padj)]
  
  # Dynamically replace Inf values with a large finite value
  max_y <- max(dt$neg_log10_padj[is.finite(dt$neg_log10_padj)], na.rm = TRUE)
  
  dt$neg_log10_padj[!is.finite(dt$neg_log10_padj)] <- 
    sign(dt$neg_log10_padj[!is.finite(dt$neg_log10_padj)]) * (abs(max_y) + 10)
  
  
  # Convert factor variable to colors
  color_map <- c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")
  
  # Determine axis limits - ensure correct order
  x_min <- min(dt$log2FC, na.rm = TRUE)
  x_max <- max(dt$log2FC, na.rm = TRUE)
  y_min <- min(dt$neg_log10_padj, na.rm = TRUE)
  y_max <- max(dt$neg_log10_padj, na.rm = TRUE)
  
  # Add margin for better display
  x_range <- x_max - x_min
  y_range <- y_max - y_min
  x_min <- x_min - 0.1 * abs(x_range)
  x_max <- x_max + 0.1 * abs(x_range)
  y_min <- y_min - 0.1 * abs(y_range)
  y_max <- y_max + 0.1 * abs(y_range)
  
  # Select top N genes by p-adj (if top_n > 0)
  top_genes <- data.table()
  if (!is.null(top_n) && !is.na(top_n) && top_n > 0 && nrow(dt) > 0) {
    top_genes <- dt[order(padj)][1:min(top_n, nrow(dt))]
  }
  
  
  # Create plot
  plot <- plot_ly() %>%
    add_trace(
      data = dt,
      x = ~log2FC,
      y = ~neg_log10_padj,
      type = 'scatter',
      mode = 'markers',
      color = ~sig,
      colors = color_map,
      marker = list(opacity = 0.7, size = 5),
      text = ~paste(feature_name," \n",padj),
      hoverinfo = "text",
      inherit = FALSE
    )
  
  # Add vertical threshold lines only if within data range
  if (!is.null(logfc_cutoff) && !is.na(logfc_cutoff) && abs(logfc_cutoff) <= abs(x_max)) {
    plot <- plot %>%
      add_segments(
        x = -logfc_cutoff, xend = -logfc_cutoff,
        y = y_min, yend = y_max,
        line = list(dash = "dash", color = "black", width = 1),
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      add_segments(
        x = logfc_cutoff, xend = logfc_cutoff,
        y = y_min, yend = y_max,
        line = list(dash = "dash", color = "black", width = 1),
        showlegend = FALSE,
        inherit = FALSE
      )
  }
  
  # Add horizontal threshold line
  if (!is.null(padj_cutoff) && !is.na(padj_cutoff) && padj_cutoff > 0) {
    padj_threshold <- -log10(padj_cutoff)
    if (!is.na(padj_threshold) && is.finite(padj_threshold) && padj_threshold <= y_max) {
      plot <- plot %>%
        add_segments(
          x = x_min, xend = x_max,
          y = padj_threshold, yend = padj_threshold,
          line = list(dash = "dash", color = "black", width = 1), 
          showlegend = FALSE,
          inherit = FALSE
        )
    }
  }
  
  # Add labels for top N genes (if they exist)
  if (!is.null(top_genes) && is.data.frame(top_genes) && nrow(top_genes) > 0) {
    plot <- plot %>%
      add_text(
        data = top_genes,
        x = ~log2FC,
        y = ~neg_log10_padj,
        text = ~feature_name,
        textposition = "top center",
        showlegend = FALSE,
        textfont = list(size = 10, color = "black"),
        inherit = FALSE
      )
  }
  
  plot <- plot %>%
    layout(
      title = paste("Volcano Plot -", tissue),
      xaxis = list(title = "log2FC", range = c(x_min, x_max),zeroline = FALSE,showline = TRUE),
      yaxis = list(title = "-log10(p-adj)", range = c(y_min, y_max), zeroline = FALSE, showline = TRUE),
      plot_bgcolor = "white"
    )
  ###################
  ## ggplotly plot ##
  ###################
  # y_max <- max(-log10(data[padj != 0]$padj))+10
  # plot <- ggplot(data, aes(x = log2FC, y = neg_log10_padj, color = sig, text = feature_name)) +
  #   geom_point(alpha = 0.7, size = 1) +
  #   scale_color_manual(values = c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")) +
  #   geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +  # Threshold p-adj
  #   geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") +  # Threshold logFC
  #   #geom_text_repel(data = top_genes, aes(label = feature_name), size = 3, max.overlaps = 10) +
  #   ggtitle(paste("Volcano Plot -", tissue)) +
  #   ylim(c(min(data$neg_log10_padj, na.rm = TRUE) - 10, y_max)) + 
  #   #theme_bw()
  #   theme_minimal()
  # 
  # ggplotly(plot,tooltip = "text")
  ################
  ## giraf plot ##
  ################
  # plot <- ggplot(data, aes(x = log2FC, y = -log10(padj), color = sig)) +
  #   geom_point_interactive(aes(tooltip = feature_name, data_id = feature_name), alpha = 0.7, size = 1) +
  #   scale_color_manual(values = c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")) +
  #   geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +  # Threshold p-adj
  #   geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") +  # Threshold logFC
  #   #geom_text_repel_interactive(data = top_genes, aes(label = feature_name, tooltip = feature_name, data_id = feature_name), size = 3, max.overlaps = 10) +
  #   ggtitle(paste("Volcano Plot -", tissue)) +
  #   theme_bw()
  # 
  # iplot <- girafe(
  #   ggobj = plot,  # Convert ggplot to interactive girafe
  #   options = list(
  #   # opts_sizing(width = .7),
  #   opts_zoom(max = 5),
  #   opts_hover(css = "fill:red;")  # Hover efekt na body
  #   # opts_tooltip(css = "background-color:white; color:black; border:1px solid gray; padding:5px;")  # Styl tooltipu
  #   )
  # )
  return(plot)
}
#' @export
ggvolcanoPlot <- function(dt, top_n = 10) {
  # dt <- dt[!is.na(log2FC) & !is.na(padj)]
  # dt <- classify_volcano_genes(dt)  # Gene classification
  dt[,neg_log10_padj := -log10(padj)]
  dt[, tissue := factor(tissue, levels = unique(dt$tissue))]
  
  x_min <- floor(min(dt$log2FC, na.rm = TRUE) / 5) * 5
  x_max <- ceiling(max(dt$log2FC, na.rm = TRUE) / 5) * 5
  tick_marks <- seq(x_min, x_max, by = 5)  # Create regular intervals on the axis
  
  plot <- ggplot(dt, aes(x = log2FC, y = neg_log10_padj, color = sig, text = feature_name)) +
    geom_point(alpha = 0.7, size = 1) +
    scale_color_manual(values = c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
    geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") +
    facet_wrap(~ tissue, scales = "free") +
    scale_x_continuous(breaks = tick_marks) +
    theme_bw() +
    theme(strip.text = element_text(size = 12, face = "bold"),panel.spacing = unit(1, "cm"))

  return(plot)
}
## parallel plot rendering
# plot_volcano <- function(dt, tissue) {
#   future(seed = TRUE,{
#     set.seed(42)
#     volcanoPlot(dt, tissue)
#   }) %...>%
#     (function(plot) {
#        plot  # Returns ggplot object
#     }) %>%
#     catch(function(err) {
#       message("❌ Error generating plot:", tissue, " - ", err$message)
#       NULL
#     })
# }





#########################
##   create bar plot   ##
#########################

#' @export
prepare_barPlot_data <- function(tissue,data){
  
  selected_columns <- c("Sample", "Gene", "Pathway", grep(tissue, names(data), value = TRUE))
  filtered_data <- data[, ..selected_columns]
  setnames(filtered_data, c("Sample", "Gene", "Pathway", "Tissue", "Scale", "FC"))
  
  merged_data <- filtered_data[, .(
    Pathway = paste(Pathway, collapse = ", "),
    Scale = unique(Scale),
    FC = unique(FC)
  ), by = .(Sample, Gene, Tissue)]
  
}  

#' @export
create_barPlot <- function(df,patient,tissue){ # df = copy(merged_data)
  
  y_max <- ceiling(max(abs(df$FC)))
  y_min <- -y_max
  
  p <- plot_ly(df, x = ~Gene, y = ~FC, type = 'bar', 
               marker = list(color = ~FC, 
                             colorscale = list(c(0, 0.2, 0.5, 0.8, 1),
                                               c("darkblue", "blue", "white", "red", "darkred")),
                             cmin = y_min, cmax = y_max,
                             colorbar = list(title = "FC", 
                                             len = 1, 
                                             tickvals = c( -7.5, -5, -2.5, 0, 2.5, 5, 7.5), 
                                             ticktext = c("-7.5", "-5", "-2.5", "0", "2.5", "5", "7.5"))),
               text = "",
               hovertext =  ~paste("Gene:", Gene, "<br>Pathway:", sapply(Pathway, paste, collapse=", "), "<br>FC:", FC),
               hoverinfo = 'text'
  ) %>%
    layout(title = paste0("Tissue: ",tissue," - Gene Fold Change Distribution in Pathways"), 
           xaxis = list(title = "Genes"), # ,showticklabels = FALSE
           yaxis = list(title = "FC", range = c(y_min, y_max)),
           margin = list(t = 100))
  
  return(p)
}