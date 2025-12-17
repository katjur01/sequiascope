# Reference Files / Podpůrné soubory

Tento adresář obsahuje podpůrné soubory pro aplikaci SequiaViz, které **nejsou pacientská data**.

## Požadované soubory

### 1. `kegg_tab.tsv`
- **Účel**: Mapování genů na KEGG pathways pro funkční analýzu
- **Formát**: TSV soubor s kolonkami `refseq_id`, `kegg_paths_name`
- **Použití**: 
  - Expression profile - filtrování podle pathway
  - Network graph - vizualizace pathway
  - Sankey diagram - zobrazení vztahů variant-geny-pathway

### 2. `report_template.docx`
- **Účel**: Šablona pro generování Word reportů
- **Formát**: Microsoft Word dokument (.docx)
- **Obsah**: Předdefinované styly (Heading 1-5, Normal text)
- **Použití**: Export pacientských reportů s výsledky analýzy

### 3. `genes_of_interest.tsv` (volitelné)
- **Účel**: Výchozí seznam genů pro analýzu
- **Formát**: TSV soubor s kolonkou `gene` (povinná) a `pathway` (volitelná)
- **Použití**: Pokud uživatel nenahraje vlastní GOI, použije se tento seznam

## Umístění v různých prostředích

### Docker prostředí
Soubory jsou součástí Docker image:
```
/app/reference_files/
├── kegg_tab.tsv
├── report_template.docx
└── genes_of_interest.tsv
```

Cesta je nastavena v `app/main.R`:
```r
shared_data$reference_path <- reactiveVal("/app/reference_files")
```

### Lokální vývoj
Pro lokální vývoj se zatím používá `./input_files`:
```r
shared_data$reference_path <- reactiveVal("./input_files")
```

**DŮLEŽITÉ**: V budoucnu bude cesta konfigurovatelná přes `config.yml`.

## Získání souborů

### kegg_tab.tsv
Tento soubor lze vygenerovat z KEGG databáze nebo obdržet od správce projektu.

### report_template.docx
Lze vytvořit v Microsoft Word s následujícími styly:
- Heading 1, Heading 2, ..., Heading 5
- Normal text
- Table Grid

### genes_of_interest.tsv
Příklad struktury:
```
gene	pathway
TP53	Cell cycle
BRCA1	DNA repair
EGFR	EGFR signaling pathway
```

## Poznámky pro vývojáře

- Tyto soubory **NESMÍ** být v `/input_files` (kde jsou pacientská data)
- V Docker image jsou součástí aplikace (baked-in)
- Pro lokální vývoj: zkopírujte soubory z `input_files/MOII_e117` nebo jiného demo projektu
- V budoucnu: cesta bude konfigurovatelná v `config.yml`
