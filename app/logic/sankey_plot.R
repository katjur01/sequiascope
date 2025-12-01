# app/logic/sankey_plot.R

box::use(
  data.table[fread,setcolorder],
  reactable[colDef],
  networkD3[sankeyNetwork],
  dplyr[count,group_by,summarise,n,left_join,select]
)

# Create Sankey plot
#' @export
sankey_plot <- function(data, run){
  # kegg_path <- paste0(getwd(),"/input_files/kegg_tab.tsv")
  if (run == "docker") {
    kegg_path <- paste0(getwd(),"/kegg_tab.tsv")
  } else {
    kegg_path <- paste0(getwd(),"/input_files/kegg_tab.tsv")
  }
  
  df <- fread(kegg_path)
  data_sub <- data[,list(var_name,gene_symbol,`annotation source`,gene)]
  data_sub2_refseq <- df[, list(refseq_id,kegg_paths_name)]
  data_sub2_refseq[,refseq_id := as.character(refseq_id)]
  data_sankey_refseq <- merge(data_sub,data_sub2_refseq,by.x = "gene", by.y = "refseq_id")
  data_sub2_ensemble <- df[, list(ensembl_id,kegg_paths_name)]
  data_sankey_ensemble <- merge(data_sub,data_sub2_ensemble,by.x = "gene", by.y = "ensembl_id")
  data_sankey <- rbind(data_sankey_ensemble,data_sankey_refseq)
  links <- data.frame(
    source = c(data_sankey[, var_name], data_sankey[, gene_symbol]),
    target = c(data_sankey[, gene_symbol], data_sankey[, kegg_paths_name]),
    value = c(rep(3, length(data_sankey[, var_name])), rep(1, length(data_sankey[, gene_symbol])))
  )
  links <- unique(links)
  outgoing_counts <- group_by(links, source)
  outgoing_counts <- summarise(outgoing_counts, value = n(), .groups = "drop")
  links <- left_join(links, outgoing_counts, by = c("target" = "source"))
  links$value <- ifelse(is.na(links$value.y), links$value.x, links$value.y)
  links <- select(links, source, target, value)
  colnames(links)[colnames(links) == "value.x"] <- "value"
  nodes <- data.frame(
    name = unique(c(as.character(links$source), as.character(links$target)))
  )
  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1
  plot_height <- nrow(nodes) * 15
  return(list(links = links, nodes = nodes, plot_height = plot_height))
}