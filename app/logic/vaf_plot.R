# app/logic/vaf_plot.R

box::use(
  ggplot2[ggsave, ggplot, geom_density, aes, labs, theme, element_text, scale_x_continuous, scale_y_continuous, 
          geom_histogram,expansion,margin,element_rect,element_line,scale_color_manual,unit,geom_vline,annotate]
)

# Create VAF diagram
#' @export
generate_vaf <- function(data, selected_variants){
  data <- data[,"tumor_variant_freq", drop = FALSE]
  ggplot(data, aes(x=tumor_variant_freq)) +
    geom_histogram(binwidth = 0.01,fill="#A7C6ED", color="#e9ecef", alpha=0.9)+
    geom_density(aes(color = "Distribution curve"), linewidth = 0.5,, key_glyph = "path") +
    scale_color_manual(values = c("Distribution curve" = "#333333"), name = "") +
    labs(x="Tumor variant frequency",y="Number of found variants")+
    geom_vline(xintercept = selected_variants[["tumor_variant_freq"]], color = "blue", linetype = "dashed", linewidth = 1) +
    annotate("text", x = selected_variants[["tumor_variant_freq"]],
             y = rep(Inf, length(selected_variants[["tumor_variant_freq"]])),  
             label = paste0(selected_variants[["var_name"]], "                                        "),
             vjust = -0.5, size = 5, angle = 90, color = "blue")+
    scale_x_continuous(breaks = seq(0,1,by=0.05),minor_breaks = seq(0, 1, by = 0.01))+
    scale_y_continuous(expand=expansion(mult = c(0, 0.01)),breaks = seq(0,100,by=1),minor_breaks = seq(0,100,by=1))+
    theme(
      axis.title.x = element_text(size=15,face="bold"),
      axis.title.y = element_text(size=15,face="bold"),
      axis.text.x = element_text(size=15,margin = margin(t=10)),
      axis.text.y = element_text(size=15,margin = margin(r=10)),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey80"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 13),
      legend.key.size = unit(0.5,"cm")
    )
}