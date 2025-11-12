# app/logic/prepare_main_table_and_filters.R

box::use(data.table[fread,setcolorder],
         reactable[colDef],
         dplyr[count,group_by,summarise,n,left_join,select]
)


# During automatic loading of possible columns override the names of expected columns with more appropriate versions
#' @export
map_column_names <- function(column_names) {
  name_mapping <- list(
    var_name = "Variant name",
    library = "In library",
    gene_symbol = "Gene symbol",
    HGVSp = "HGVSp",
    HGVSc = "HGVSc",
    tumor_variant_freq = "Tumor variant frequency",
    tumor_depth = "Tumor depth",
    gnomAD_NFE = "GnomAD NFE",
    snpDB = "SnpDB",
    COSMIC = "COSMIC",
    HGMD = "HGMD",
    clinvar_sig = "ClinVar significance",
    clinvar_DBN = "ClinVar DBN",
    fOne = "fOne",
    CGC_Somatic = "CGC Somatic",
    gene_region = "Gene region",
    consequence = "Consequence",
    all_full_annot_name = "Full annotated name",
    alarm = "Alarm",
    full_annot_name = "Annotated name",
    var_gen_coord = "Variant coordinates",
    variant_type = "Variant type",
    normal_variant_freq = "Normal variant frequency",
    normal_depth = "Normal depth",
    Called_by = "Called by",
    "1000g_EUR_AF" = "1000G EUR AF",
    NHLBI_ESP = "NHLBI ESP",
    "md-anderson" = "MD Anderson",
    trusight_genes = "TruSight genes",
    CGC_Tumour_Somatic = "CGC Tumour",
    PolyPhen = "PolyPhen",
    SIFT = "SIFT",
    IMPACT = "Impact",
    EXON = "Exon",
    INTRON = "Intron",
    Feature = "Feature",
    Feature_type = "Feature type",
    "Annotation source" = "Annotation source",
    Gene = "Gene"
  )
  
  unname(sapply(column_names, function(col) {
    if (col %in% names(name_mapping)) {
      name_mapping[[col]]
    } else {
      col 
    }
  }))
}

# During automatic loading of possible gene regions override the names of expected columns with more appropriate versions
#' @export
map_gene_region_names <- function(gene_region_names){
  gene_region_mapping <- list(
    "exon" = "Exon",
    "intron" = "Intron",
    "downstream" = "Downstream",
    "splice" = "Splice",
    "5_prime_UTR" = "5 prime UTR",
    "3_prime_UTR" = "3 prime UTR",
    "upstream" = "Upstream",
    "non_coding" = "Non-coding",
    "regulatory_region" = "Regulatory region"
  )
  
  unname(sapply(gene_region_names, function(reg) {
    if (as.character(reg) %in% names(gene_region_mapping)) {
      gene_region_mapping[[reg]]
    } else {
      reg 
    }
  }))
} 

# During automatic loading of possible clinvar_sig override the names of expected columns with more appropriate versions
#' @export
map_clin_sig_names <- function(clin_sig_names){
  clin_sig_mapping <- list(
    "Likely_benign"="Likely benign",
    "Pathogenic"="Pathogenic",
    "Uncertain_significance"="Uncertain significance",
    "Benign"="Benign",
    "Conflicting_classifications_of_pathogenicity"="Conflicting classifications of pathogenicity",
    "." = "Unknown"
  )
  
  unname(sapply(clin_sig_names, function(sig) {
    if (as.character(sig) %in% names(clin_sig_mapping)) {
      clin_sig_mapping[[sig]]
    } else {
      sig 
    }
  }))
}

