# Migrace podpůrných souborů do reference_files

## Kroky migrace

### 1. Zkopírujte soubory z demo dat

Pokud máte demo data v `input_files/MOII_e117/`, zkopírujte soubory:

```bash
# Najděte soubory v demo datech
find input_files -name "kegg_tab.tsv"
find input_files -name "report_template.docx"
find input_files -name "genes_of_interest.tsv"

# Příklad kopírování (upravte cesty podle vašeho projektu)
cp input_files/MOII_e117/kegg_tab.tsv reference_files/
cp input_files/MOII_e117/report_template.docx reference_files/
cp input_files/MOII_e117/genes_of_interest.tsv reference_files/  # pokud existuje
```

### 2. Pro lokální vývoj (dočasné řešení)

Dokud nebudete mít `config.yml`, ponechte kopie v `input_files/`:
- `./input_files/kegg_tab.tsv`
- `./input_files/report_template.docx`

Protože v `main.R` pro lokální vývoj je:
```r
shared_data$reference_path <- reactiveVal("./input_files")
```

### 3. Ověření v Dockeru

Po build Docker image ověřte, že soubory jsou na správném místě:

```bash
# Build image
docker-compose build

# Spusť kontejner a zkontroluj soubory
docker-compose up -d
docker exec -it sequiaviz-shiny-1 ls -la /app/reference_files/

# Měli byste vidět:
# kegg_tab.tsv
# report_template.docx
# genes_of_interest.tsv (pokud existuje)
```

### 4. Budoucí konfigurace přes config.yml

Plánované nastavení v `config.yml`:

```yaml
reference_files:
  path: "/app/reference_files"  # Docker
  # path: "./reference_files"   # Lokální
  
  kegg_tab: "kegg_tab.tsv"
  report_template: "report_template.docx"
  genes_of_interest: "genes_of_interest.tsv"
```

## Proč tato změna?

### Před změnou ❌
- Podpůrné soubory byly v `input_files/` (kde jsou pacientská data)
- Uživatel musel mít tyto soubory ve svých datech
- Nejasné oddělení aplikačních souborů od dat

### Po změně ✅
- Podpůrné soubory jsou v `reference_files/` (součást aplikace)
- V Dockeru jsou součástí image (`/app/reference_files/`)
- Jasné oddělení: `/input_files` = data pacienta, `/app/reference_files` = soubory aplikace
- Uživatel nepotřebuje tyto soubory ve svých datech

## Ovlivněné soubory

### Změněné soubory:
1. `app/main.R` - přidána `shared_data$reference_path`
2. `app/logic/helper_networkGraph.R` - `get_pathway_list()` používá `reference_path`
3. `app/logic/sankey_plot.R` - `sankey_plot()` používá `reference_path`
4. `app/view/create_report.R` - šablona z `reference_path`
5. `app/view/expression_profile_table.R` - volání `get_pathway_list()` s `reference_path`
6. `app/view/networkGraph_cytoscape.R` - volání `get_pathway_list()` s `reference_path`
7. `app/view/somatic_var_call_table.R` - volání `sankey_plot()` s `reference_path`
8. `Dockerfile` - vytvoření `/app/reference_files/` a kopírování souborů
9. `.dockerignore` - includování `reference_files/`

### Nově vytvořené:
- `reference_files/` - adresář pro podpůrné soubory
- `reference_files/README.md` - dokumentace
- `reference_files/MIGRATION.md` - tento soubor

## Testování

### Lokální prostředí:
```bash
# Zkopírujte soubory do ./input_files/ pro lokální vývoj
cp input_files/MOII_e117/kegg_tab.tsv input_files/
cp input_files/MOII_e117/report_template.docx input_files/

# Spusťte aplikaci
R -e "shiny::runApp('.')"
```

### Docker prostředí:
```bash
# Zkopírujte soubory do reference_files/
cp input_files/MOII_e117/kegg_tab.tsv reference_files/
cp input_files/MOII_e117/report_template.docx reference_files/

# Build a spuštění
docker-compose build
docker-compose up -d

# Otevřete http://localhost:8080
```

## Známé problémy

1. **Lokální vývoj**: Zatím je třeba mít kopie v `./input_files/`
   - **Řešení**: V budoucnu změnit v `main.R` na `"./reference_files"`

2. **genes_of_interest.tsv**: Není jasné zda jde o referenční soubor nebo user data
   - **Status quo**: Ponecháváme v `/input_files/` (user data)
   - **Budoucnost**: Možnost mít default v `reference_files/` a override z user data

## Další kroky

1. ✅ Implementovat `reference_path` v `main.R`
2. ✅ Aktualizovat všechny funkce používající reference soubory
3. ⏳ Zkopírovat soubory do `reference_files/`
4. ⏳ Otestovat lokálně
5. ⏳ Otestovat v Dockeru
6. ⏳ Implementovat konfiguraci přes `config.yml`
