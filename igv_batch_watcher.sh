#!/bin/bash
# IGV Batch File Watcher
# Monitors for new batch files and executes them

# Use environment variables with defaults
OUTPUT_BASE_DIR="${OUTPUT_BASE_DIR:-/output_files}"
WATCH_DIR="${WATCH_DIR:-${OUTPUT_BASE_DIR}/igv_snapshots}"
PROCESSED_DIR="${PROCESSED_DIR:-${OUTPUT_BASE_DIR}/.processed_batches}"
IGV_SCRIPT="/opt/IGV_2.16.2/igv.sh"
XVFB_DISPLAY_START=100
XVFB_DISPLAY_END=150
CURRENT_DISPLAY=$XVFB_DISPLAY_START

# Create processed directory if it doesn't exist
mkdir -p "$PROCESSED_DIR"

echo "[WATCHER] Starting IGV batch file watcher..."
echo "[WATCHER] Watching directory: $WATCH_DIR"
echo "[WATCHER] Using DISPLAY range: :${XVFB_DISPLAY_START} - :${XVFB_DISPLAY_END}"

# Clean up old batch files and status files from previous runs
echo "[WATCHER] Cleaning up old batch files and status files..."
if [ -d "$WATCH_DIR" ]; then
    find "$WATCH_DIR" -type f \( -name "*.done" -o -name "*.error" -o -name "*.log" -o -name "*_batch.txt" \) -delete
    echo "[WATCHER] Old batch files and status files removed"
else
    echo "[WATCHER] Watch directory does not exist yet, creating..."
    mkdir -p "$WATCH_DIR"
fi

# Function to get next available display number
get_next_display() {
    CURRENT_DISPLAY=$((CURRENT_DISPLAY + 1))
    if [ $CURRENT_DISPLAY -gt $XVFB_DISPLAY_END ]; then
        CURRENT_DISPLAY=$XVFB_DISPLAY_START
    fi
    echo $CURRENT_DISPLAY
}

# Function to process a batch file
process_batch_file() {
    local batch_file="$1"
    local batch_name=$(basename "$batch_file")
    local patient_dir=$(dirname "$batch_file")
    local patient_id=$(basename "$patient_dir")
    local done_file="${batch_file}.done"
    local error_file="${batch_file}.error"
    
    echo "[WATCHER] =========================================="
    echo "[WATCHER] Processing batch file: $batch_name"
    echo "[WATCHER] Patient: $patient_id"
    echo "[WATCHER] Batch path: $batch_file"
    
    # Check if already processed
    if [ -f "$done_file" ] || [ -f "$error_file" ]; then
        echo "[WATCHER] Already processed, skipping..."
        return
    fi
    
    # Get unique display number for this process
    local display_num=$(get_next_display)
    export DISPLAY=":${display_num}"
    
    echo "[WATCHER] Using DISPLAY=${DISPLAY}"
    echo "[WATCHER] Starting Xvfb on ${DISPLAY}..."
    
    # Start Xvfb for this display (with -nolisten tcp for security)
    Xvfb ${DISPLAY} -screen 0 1280x1024x24 -nolisten tcp 2>&1 | sed 's/^/[XVFB] /' &
    local xvfb_pid=$!
    sleep 2
    
    # Check if Xvfb started successfully
    if ! kill -0 $xvfb_pid 2>/dev/null; then
        echo "[WATCHER] ❌ ERROR: Xvfb failed to start"
        echo "Failed to start Xvfb" > "$error_file"
        return 1
    fi
    
    echo "[WATCHER] Xvfb started (PID: $xvfb_pid)"
    echo "[WATCHER] Executing IGV batch script..."
    
    # Execute IGV using igv.sh script (which sets up classpath correctly)
    # Don't use java -jar directly - it won't find htsjdk and other dependencies
    /opt/IGV_2.16.2/igv.sh -b "$batch_file" \
        > "${batch_file}.log" 2>&1
    
    local igv_exit_code=$?
    
    echo "[WATCHER] IGV execution completed with exit code: $igv_exit_code"
    
    # Show relevant parts of IGV log if there was an error
    if [ $igv_exit_code -ne 0 ] && [ -f "${batch_file}.log" ]; then
        echo "[WATCHER] IGV error log (last 20 lines):"
        tail -20 "${batch_file}.log" | sed 's/^/[IGV LOG] /'
    fi
    
    # Cleanup Xvfb
    kill $xvfb_pid 2>/dev/null
    wait $xvfb_pid 2>/dev/null
    echo "[WATCHER] Xvfb stopped"
    
    # Create done or error file based on exit code
    if [ $igv_exit_code -eq 0 ]; then
        echo "success" > "$done_file"
        echo "[WATCHER] ✅ SUCCESS: Batch file processed successfully"
        echo "[WATCHER] Done file created: $done_file"
    else
        # Write detailed error info to error file
        {
            echo "IGV exit code: $igv_exit_code"
            echo "---"
            echo "Last 10 lines of IGV log:"
            if [ -f "${batch_file}.log" ]; then
                tail -10 "${batch_file}.log"
            else
                echo "No log file found"
            fi
        } > "$error_file"
        echo "[WATCHER] ❌ ERROR: IGV failed with exit code $igv_exit_code"
        echo "[WATCHER] Error file created: $error_file"
    fi
    
    # Move to processed directory
    mkdir -p "$PROCESSED_DIR/$patient_id"
    cp "$batch_file" "$PROCESSED_DIR/$patient_id/"
    echo "[WATCHER] Batch file backed up to: $PROCESSED_DIR/$patient_id/"
    
    echo "[WATCHER] =========================================="
}

# Main watch loop
echo "[WATCHER] Starting monitoring loop (checking every 2 seconds)..."
echo "[WATCHER] Press Ctrl+C to stop"

while true; do
    # Find all batch files that don't have .done or .error files
    if [ -d "$WATCH_DIR" ]; then
        find "$WATCH_DIR" -type f -name "*_batch.txt" | while read -r batch_file; do
            done_file="${batch_file}.done"
            error_file="${batch_file}.error"
            
            # Only process if not already done and not currently being processed
            if [ ! -f "$done_file" ] && [ ! -f "$error_file" ]; then
                # Check if file is complete (not being written)
                # Wait for file to be stable for 1 second
                size1=$(stat -c%s "$batch_file" 2>/dev/null || echo 0)
                sleep 1
                size2=$(stat -c%s "$batch_file" 2>/dev/null || echo 0)
                
                if [ "$size1" -eq "$size2" ] && [ "$size1" -gt 0 ]; then
                    process_batch_file "$batch_file"
                fi
            fi
        done
    fi
    
    sleep 2
done
