# Migrace na config.json systém

## ✅ Co bylo implementováno

### 1. Vytvořen `config.json`
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

### 2. Aktualizován `app/main.R`
- Přidán import `jsonlite[fromJSON]`
- Config se načítá při startu
- Vytvořeny reactive hodnoty:
  - `shared_data$kegg_tab_path()`
  - `shared_data$report_template_path()`
  - `shared_data$genes_of_interest_path()`

### 3. Aktualizovány funkce
- **`get_pathway_list(expr_tag, goi_dt, kegg_tab_path)`** - přijímá celou cestu k kegg_tab.tsv
- **`sankey_plot(data, kegg_tab_path)`** - přijímá celou cestu k kegg_tab.tsv
- Žádné `file.path()` sestavování cest v těchto funkcích

### 4. Aktualizována všechna volání
- `app/view/expression_profile_table.R` - 2x volání `get_pathway_list()`
- `app/view/networkGraph_cytoscape.R` - 1x volání `get_pathway_list()`
- `app/view/somatic_var_call_table.R` - 1x volání `sankey_plot()`
- `app/view/create_report.R` - používá `shared_data$report_template_path()`

### 5. Aktualizován Docker
- `Dockerfile` - kopíruje `app/references/` do `/app/references/`
- `.dockerignore` - includuje `config.json` a `app/references/`

### 6. Vytvořena dokumentace
- `app/references/README.md` - kompletní dokumentace
- `app/references/MIGRATION.md` - tento soubor

## 📋 Co je třeba udělat

### Krok 1: Příprava souborů pro lokální vývoj

```bash
# Najděte soubory ve stávajících demo datech
cd /home/katka/BioRoots/sequiaViz

# Příklad - najděte kde jsou soubory
find input_files -name "kegg_tab.tsv" -type f
find input_files -name "report_template.docx" -type f

# Zkopírujte do app/references/
cp input_files/MOII_e117/kegg_tab.tsv app/references/
cp input_files/MOII_e117/report_template.docx app/references/

# Volitelně genes_of_interest
# cp input_files/...path.../genes_of_interest.txt app/references/
```

### Krok 2: Test lokálně

```bash
# Spusťte aplikaci
R -e "shiny::runApp('.')"

# V konzoli by měly být zprávy:
# 💻 Running locally - using relative paths
# 📁 Reference files:
#   kegg_tab: ./app/references/kegg_tab.tsv
#   report_template: ./app/references/report_template.docx
#   genes_of_interest: ./app/references/genes_of_interest.txt
```

### Krok 3: Test funkcionalita

Otestujte v aplikaci:
- ✅ Expression profile - filtrování podle pathway
- ✅ Network graph - pathway selector
- ✅ Sankey diagram - zobrazení pathways
- ✅ Create report - export do Word

### Krok 4: Build Docker

```bash
# Build image
docker-compose build

# Spustit
docker-compose up -d

# Zkontrolovat soubory v kontejneru
docker exec -it sequiaviz-shiny-1 ls -la /app/references/
# Měly by tam být:
# - kegg_tab.tsv
# - report_template.docx
# - genes_of_interest.txt (pokud jste přidali)

# Zkontrolovat config
docker exec -it sequiaviz-shiny-1 cat /srv/shiny-server/sequiaViz/config.json

# Zkontrolovat logy
docker logs sequiaviz-shiny-1
# Měli byste vidět:
# 🐳 Running in Docker - using absolute paths
# 📁 Reference files:
#   kegg_tab: /app/references/kegg_tab.tsv
#   ...
```

### Krok 5: Test v Dockeru

Otevřete http://localhost:8080 a otestujte stejné funkce jako lokálně.

## 🔧 Řešení problémů

### ❌ Error: "kegg_tab_path is NULL or empty"

**Příčina**: Config se nenačetl nebo má špatnou strukturu.

**Řešení**:
```bash
# Zkontrolujte že config.json existuje
ls -la config.json

# Zkontrolujte syntaxi JSON
cat config.json | python -m json.tool

# V R konzoli:
library(jsonlite)
config <- fromJSON("config.json")
print(config)
```

### ❌ Error: "Soubor ...kegg_tab.tsv nebyl nalezen"

**Příčina**: Soubor není v app/references/ nebo config má špatnou cestu.

**Řešení**:
```bash
# Lokálně
ls -la app/references/kegg_tab.tsv

# V Dockeru
docker exec -it sequiaviz-shiny-1 ls -la /app/references/kegg_tab.tsv
```

### ❌ Docker build - soubory nejsou zkopírovány

**Příčina**: `.dockerignore` blokuje soubory.

**Řešení**:
```bash
# Zkontrolujte .dockerignore
cat .dockerignore

# Mělo by tam být:
# !config.json
# !app/references/
# !app/references/*.tsv
# !app/references/*.docx
# !app/references/*.txt
```

## 📝 Poznámky

### Stará vs. nová struktura

**Před:**
```r
# V main.R
shared_data$reference_path <- reactiveVal("/app/references")

# Ve funkci
get_pathway_list <- function(..., reference_path) {
  path <- file.path(reference_path, "kegg_tab.tsv")  # Hardcoded název
  ...
}
```

**Po:**
```r
# V main.R (z config.json)
shared_data$kegg_tab_path <- reactiveVal("/app/references/kegg_tab.tsv")

# Ve funkci
get_pathway_list <- function(..., kegg_tab_path) {
  dt <- fread(kegg_tab_path)  # Žádné hardcoded názvy
  ...
}
```

### Výhody nového řešení

✅ **Flexibilní názvy souborů** - lze změnit v config.json  
✅ **Přehledné** - každý soubor má explicitní cestu  
✅ **Testovatelné** - lze snadno přepnout na testovací soubory  
✅ **Žádné hardcoded hodnoty** - vše v jednom místě (config.json)  
✅ **Environment-aware** - automaticky detekuje Docker vs. local  

### Budoucí rozšíření

Můžete snadno přidat další konfigurační položky do `config.json`:
```json
{
  "reference_files": {
    "docker": { ... },
    "local": { ... }
  },
  "igv_settings": {
    "docker": {
      "host": "igv-static",
      "port": 8081
    },
    "local": {
      "host": "localhost",
      "port": 60151
    }
  },
  "max_file_size_mb": 500,
  "parallel_workers": 2
}
```

## ✅ Checklist

- [ ] Zkopírovány soubory do `app/references/`
- [ ] Otestováno lokálně (R shiny::runApp)
- [ ] Otestována funkcionalita (pathway filtering, sankey, reports)
- [ ] Docker build úspěšný
- [ ] Soubory jsou v Docker kontejneru (`/app/references/`)
- [ ] Aplikace běží v Dockeru bez errors
- [ ] Otestována funkcionalita v Dockeru

## 🎯 Shrnutí změn

| Soubor | Změna |
|--------|-------|
| `config.json` | ✨ NOVÝ - konfigurace cest |
| `app/main.R` | ✏️ Načítá config, vytváří reactive hodnoty |
| `app/logic/helper_networkGraph.R` | ✏️ `kegg_tab_path` parametr |
| `app/logic/sankey_plot.R` | ✏️ `kegg_tab_path` parametr |
| `app/view/create_report.R` | ✏️ Používá `report_template_path()` |
| `app/view/expression_profile_table.R` | ✏️ Volání s `kegg_tab_path` |
| `app/view/networkGraph_cytoscape.R` | ✏️ Volání s `kegg_tab_path` |
| `app/view/somatic_var_call_table.R` | ✏️ Volání s `kegg_tab_path` |
| `Dockerfile` | ✏️ Kopíruje do `/app/references/` |
| `.dockerignore` | ✏️ Includuje config a references |
| `app/references/` | ✨ NOVÝ adresář |
| `app/references/README.md` | ✨ NOVÝ - dokumentace |
