# Reference Files / Podpůrné soubory

Tento adresář obsahuje podpůrné soubory pro aplikaci SequiaViz, které **nejsou pacientská data**.

## Konfigurace

Cesty k souborům jsou definované v **`config.json`** v kořeni projektu:

```json
{
  "reference_files": {
    "docker": {
      "kegg_tab": "/app/references/kegg_tab.tsv",
      "report_template": "/app/references/report_template.docx",
      "genes_of_interest": "/app/references/genes_of_interest.txt"
    },
    "local": {
      "kegg_tab": "./app/references/kegg_tab.tsv",
      "report_template": "./app/references/report_template.docx",
      "genes_of_interest": "./app/references/genes_of_interest.txt"
    }
  }
}
```

## Požadované soubory

### 1. `kegg_tab.tsv`
- **Účel**: Mapování genů na KEGG pathways pro funkční analýzu
- **Formát**: TSV soubor s kolonkami `refseq_id`, `ensembl_id`, `kegg_paths_name`
- **Použití**: 
  - Expression profile - filtrování podle pathway
  - Network graph - vizualizace pathway
  - Sankey diagram - zobrazení vztahů variant-geny-pathway

### 2. `report_template.docx`
- **Účel**: Šablona pro generování Word reportů
- **Formát**: Microsoft Word dokument (.docx)
- **Obsah**: Předdefinované styly (Heading 1-5, Normal text)
- **Použití**: Export pacientských reportů s výsledky analýzy

### 3. `genes_of_interest.txt` (volitelné)
- **Účel**: Výchozí seznam genů pro analýzu
- **Formát**: Textový soubor s kolonkou `gene` (povinná) a `pathway` (volitelná)
- **Použití**: Pokud uživatel nenahraje vlastní GOI, použije se tento seznam

## Jak to funguje

### V kódu (main.R):
```r
# Config je načten při startu
config <- fromJSON("config.json")
env_type <- if (in_docker) "docker" else "local"

# Vytvoří se reactive hodnoty s celými cestami
shared_data$kegg_tab_path <- reactiveVal(config$reference_files[[env_type]]$kegg_tab)
shared_data$report_template_path <- reactiveVal(config$reference_files[[env_type]]$report_template)
shared_data$genes_of_interest_path <- reactiveVal(config$reference_files[[env_type]]$genes_of_interest)
```

### Ve funkcích:
```r
# Funkce dostávají celou cestu včetně názvu souboru
get_pathway_list <- function(expr_tag, goi_dt = NULL, kegg_tab_path = NULL) {
  dt <- fread(kegg_tab_path)  # Přímo použije celou cestu
  ...
}

# Volání
pathway_list <- get_pathway_list("all_genes", kegg_tab_path = shared_data$kegg_tab_path())
```

## Získání souborů

### kegg_tab.tsv
Tento soubor lze získat:
1. Vygenerovat z KEGG databáze
2. Použít stávající z demo dat (`input_files/MOII_e117/...`)
3. Obdržet od správce projektu

### report_template.docx
Lze vytvořit v Microsoft Word s následujícími styly:
- Heading 1, Heading 2, ..., Heading 5
- Normal text
- Table Grid

### genes_of_interest.txt
Příklad struktury (TSV format):
```
gene	pathway
TP53	Cell cycle
BRCA1	DNA repair
EGFR	EGFR signaling pathway
```

## Příprava souborů

### Pro lokální vývoj:
```bash
# Zkopírujte soubory do app/references/
mkdir -p app/references
cp input_files/MOII_e117/kegg_tab.tsv app/references/
cp input_files/MOII_e117/report_template.docx app/references/
```

### Pro Docker:
```bash
# Soubory musí být v app/references/ před buildem
# Docker je zkopíruje do /app/references/ podle Dockerfile

docker-compose build
docker-compose up -d

# Ověření
docker exec -it sequiaviz-shiny-1 ls -la /app/references/
```

## Změna cest nebo názvů souborů

### Změna umístění:
Edituj `config.json`:
```json
{
  "reference_files": {
    "docker": {
      "kegg_tab": "/custom/path/my_kegg.tsv",
      ...
    }
  }
}
```

### Změna názvu:
Stačí přejmenovat soubor a upravit config:
```json
{
  "reference_files": {
    "local": {
      "kegg_tab": "./app/references/my_custom_kegg_table.tsv",
      ...
    }
  }
}
```

## Výhody tohoto řešení

✅ **Žádné hardcoded cesty** - vše v config.json  
✅ **Flexibilní** - lze snadno změnit názvy/cesty souborů  
✅ **Oddělení dat** - aplikační soubory ≠ pacientská data  
✅ **Čitelné** - každá funkce dostává explicitní cestu k souboru  
✅ **Testovatelné** - lze snadno použít různé soubory pro testy  

## Ovlivněné soubory

Soubory, které používají reference files:
- `app/main.R` - načítá config a vytváří reactive hodnoty
- `app/logic/helper_networkGraph.R` - `get_pathway_list(kegg_tab_path)`
- `app/logic/sankey_plot.R` - `sankey_plot(data, kegg_tab_path)`
- `app/view/create_report.R` - používá `report_template_path()`
- `app/view/expression_profile_table.R` - volá `get_pathway_list()`
- `app/view/networkGraph_cytoscape.R` - volá `get_pathway_list()`
- `app/view/somatic_var_call_table.R` - volá `sankey_plot()`
