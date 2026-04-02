# SeqUIaSCOPE

SeqUIaSCOPE is a web application designed for routine clinical oncology diagnostics through patient-centric integration and visualization of genomic variants, fusion events, and expression profiles. It includes embedded genome browsing, pathway-level interpretation, and customizable reporting.

---
 
## Quick start
 
- 🚀 **[Live demo](https://sequiascope.dyn.cloud.e-infra.cz)** — try the application with simulated sample data, no setup required
- 📖 **[Documentation](https://bioit-ceitec.github.io/sequiascope)** — detailed user guide, supported formats, and workflow description
 
---

## Deployment options

SeqUIaSCOPE can be deployed in two ways:

- **On a cluster** using Kubernetes + Helm — suitable for shared or production environments
- **Locally** using Docker Compose — suitable for single-machine use

Pre-built images for both scenarios are available on Docker Hub — no need to build anything yourself:

| Image | Docker Hub | Description |
|---|---|---|
| App | [`juraskovakaterina/sequiascope-app`](https://hub.docker.com/r/juraskovakaterina/sequiascope-app) | The main Shiny application (port 8080) |
| IGV | [`juraskovakaterina/sequiascope-igv`](https://hub.docker.com/r/juraskovakaterina/sequiascope-igv) | IGV static server (port 8081) |

---

## Option A — Cluster deployment (Kubernetes + Helm)

For cluster environments, SeqUIaSCOPE can be deployed using the provided Helm chart. Full instructions and configuration options are maintained in a separate repository:

👉 **[sequiascope-helm](https://bioit-ceitec.github.io/sequiascope-helm)**

The Helm chart uses the same pre-built Docker Hub images listed above.

---

## Option B — Local deployment (Docker Compose)

### Prerequisites

- [Docker](https://docs.docker.com/engine/install/) ≥ 24
- [Docker Compose](https://docs.docker.com/compose/install/) (if not already installed)

```bash
docker --version
docker compose version
```

### Installation

#### 1. Prepare your working directory

Linux / macOS:

```bash
mkdir -p sequiascope/input_files sequiascope/output_files
cd sequiascope
```

Windows (Command Prompt):

```cmd
mkdir sequiascope\input_files
mkdir sequiascope\output_files
cd sequiascope
```

---

#### 2. Download the compose file and configure your data path

Linux / macOS / Windows:

```bash
curl -O https://raw.githubusercontent.com/katjur01/seqUIaSCOPE/main/docker-compose.yaml
```

---

The application uses two volumes defined in the compose file:

- `input_files` — read-only mount for your patient data
- `output_files` — writable mount for sessions, IGV snapshots, and reports

By default, the compose file expects your data to be placed inside the `input_files` subfolder you just created. If your data lives elsewhere, open `docker-compose.yaml` in a text editor and update the `input_files` volume to point to your actual data directory. **You must update this path in both the `app` and `igv` service sections.**

```yaml
volumes:
  - /path/to/your/data:/input_files:ro   # ← change this path in BOTH services
  - ./output_files:/output_files
```

On Windows, use forward slashes or a drive letter, for example:

```yaml
volumes:
  - C:/Users/yourname/mydata:/input_files:ro
  - ./output_files:/output_files
```

> ⚠️ **Do not change the `output_files` path.** Both containers must share the same output directory to communicate correctly.

---

#### 3. Pull

```bash
docker compose pull
```

#### 4. Start

```bash
docker compose up -d
```

#### 5. Open in browser

```
http://localhost:8080
```

The app may take 10–20 seconds to initialize on first launch.

#### 6. Stop

```bash
docker compose down
```

---

## IGV snapshots

The `sequiascope-igv` container generates IGV screenshots for fusion gene visualizations when BAM files are provided. It watches `output_files/sessions/` and processes batch files written by the app.

---

## Configuration

The app reads a `reference_paths.json` from the working directory. The default config is embedded in the image. To override it, mount your own:

```yaml
    volumes:
      - ./input_files:/input_files:ro
      - ./output_files:/output_files
      - ./reference_paths.json:/srv/shiny-server/sequiaScope/app/reference_paths.json:ro
```

---

## Troubleshooting

**App doesn't start**
```bash
docker compose logs app
```

**No data visible after upload**
- Check that file names match expected patterns (see Data requirements)
- Verify the mount: `docker exec sequiascope-app ls /input_files`

**IGV snapshots not generated**
- Check the IGV container is running: `docker ps | grep igv`
- Check IGV logs: `docker compose logs igv`

## License
MIT © Kateřina Jurásková
