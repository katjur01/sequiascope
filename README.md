# sequiaScope

SeqUIaSCOPE is a web application designed for routine clinical oncology diagnostics through patient-centric integration and visualization of genomic variants, fusion events, and expression profiles. It includes embedded genome browsing, pathway-level interpretation, and customizable reporting.

---
 
## Quick start
 
- 🚀 **[Live demo](https://sequiascope.dyn.cloud.e-infra.cz)** — try the application with simulated sample data, no setup required
- 📖 **[Documentation](https://katjur01.github.io/seqUIaSCOPE)** — detailed user guide, supported formats, and workflow description
 
---

## Deployment options

sequiaScope can be deployed in two ways:

- **On a cluster** using Kubernetes + Helm — suitable for shared or production environments
- **Locally** using Docker Compose — suitable for single-machine use

Pre-built images for both scenarios are available on Docker Hub — no need to build anything yourself:

| Image | Docker Hub | Description |
|---|---|---|
| App | [`juraskovakaterina/sequiascope-app`](https://hub.docker.com/r/juraskovakaterina/sequiascope-app) | The main Shiny application (port 8080) |
| IGV | [`juraskovakaterina/sequiascope-igv`](https://hub.docker.com/r/juraskovakaterina/sequiascope-igv) | IGV static server (port 8081) |

---

## Option A — Cluster deployment (Kubernetes + Helm)

For cluster environments, sequiaScope can be deployed using the provided Helm chart. Full instructions and configuration options are maintained in a separate repository:

👉 **[sequiascope-helm](https://github.com/KrKOo/sequiascope-helm)**

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

#### 1. Prepare your working directory:

```bash
mkdir -p sequiascope/input_files sequiascope/output_files
cd sequiascope
```

#### 2. Download the compose file:

```bash
curl -O https://raw.githubusercontent.com/katjur01/sequiaScope/main/docker-compose.hub.yml
```

The application uses two separate volumes:

- input_files: Read-only bind mount for input data
- output_files: Writable bind mount for session and output data


#### 3. Pull and start

```bash
docker compose -f docker-compose.hub.yml pull
docker compose -f docker-compose.hub.yml up -d
```

#### 4. Open in browser

```
http://localhost:8080
```

The app may take 10–20 seconds to initialize on first launch.

#### 5. Stop

```bash
docker compose -f docker-compose.hub.yml down
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

## Build from source

Building from source is not required for standard use. If you need to modify the application itself:

```bash
git clone https://github.com/katjur01/seqUIaSCOPE.git
cd seqUIaSCOPE
docker compose build
docker compose up -d
```

---

## Documentation

Full docs are in the [`docs/`](docs/index.html) folder.

---

## Troubleshooting

**App doesn't start**
```bash
docker compose -f docker-compose.hub.yml logs app
```

**No data visible after upload**
- Check that file names match expected patterns (see Data requirements)
- Verify the mount: `docker exec sequiascope-app ls /input_files`

**IGV snapshots not generated**
- Check the IGV container is running: `docker ps | grep igv`
- Check IGV logs: `docker compose -f docker-compose.hub.yml logs igv`

## License
MIT © Kateřina Jurásková