#!/bin/bash
# Wrapper skript pro IGV Desktop batch snapshoty
# Použití: igv_wrapper.sh <batch_file>
# Po dokončení vytvoří <batch_file>.done

set -e

BATCH_FILE="$1"

if [ -z "$BATCH_FILE" ]; then
  echo "ERROR: No batch file specified"
  exit 1
fi

if [ ! -f "$BATCH_FILE" ]; then
  echo "ERROR: Batch file does not exist: $BATCH_FILE"
  exit 1
fi

echo "Starting IGV Desktop with batch file: $BATCH_FILE"

# Spustit IGV Desktop (Xvfb je už spuštěný v kontejneru)
java -Djava.awt.headless=false -cp "/opt/IGV_2.16.2/lib/*" org.broad.igv.ui.Main -b "$BATCH_FILE"

# Po dokončení vytvořit .done soubor
DONE_FILE="${BATCH_FILE%.txt}.done"
touch "$DONE_FILE"

echo "IGV snapshots completed. Created: $DONE_FILE"
exit 0
