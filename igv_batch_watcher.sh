#!/bin/bash
# IGV Batch File Watcher
# Monitors for new batch files and executes them

# Use environment variables with defaults
OUTPUT_BASE_DIR="${OUTPUT_BASE_DIR:-/output_files}"
# Sessions are stored under ${OUTPUT_BASE_DIR}/sessions — batch files live inside
# each session's igv_snapshots subfolder.  The old default (/output_files/igv_snapshots)
# was a relic; the correct subtree to watch is /output_files/sessions.
WATCH_DIR="${WATCH_DIR:-${OUTPUT_BASE_DIR}/sessions}"
PROCESSED_DIR="${PROCESSED_DIR:-${OUTPUT_BASE_DIR}/.processed_batches}"
IGV_SCRIPT="/opt/IGV_2.16.2/igv.sh"
XVFB_DISPLAY_START=100
XVFB_DISPLAY_END=199

# Create processed directory if it doesn't exist
mkdir -p "$PROCESSED_DIR"

echo "[WATCHER] Starting IGV batch file watcher..."
echo "[WATCHER] Watching directory: $WATCH_DIR"
echo "[WATCHER] Parallel processing enabled (one IGV instance per batch file)"
echo "[WATCHER] Using DISPLAY range: :${XVFB_DISPLAY_START} - :${XVFB_DISPLAY_END}"

# Clean up old batch files and status files from previous runs
echo "[WATCHER] Cleaning up old batch files and status files..."
if [ -d "$WATCH_DIR" ]; then
    find "$WATCH_DIR" -type f \( -name "*.done" -o -name "*.error" -o -name "*.log" -o -name "*_batch.txt" -o -name "*.inprogress" -o -name "*.xvfb.log" \) -delete
    echo "[WATCHER] Old batch files and status files removed"
else
    echo "[WATCHER] Watch directory does not exist yet, creating..."
    mkdir -p "$WATCH_DIR"
fi

# Function to process a batch file
process_batch_file() {
    local batch_file="$1"
    local display_num="$2"  # display number passed from main loop
    local batch_name=$(basename "$batch_file")
    local patient_dir=$(dirname "$batch_file")
    local patient_id=$(basename "$patient_dir")
    local done_file="${batch_file}.done"
    local error_file="${batch_file}.error"
    local igv_log_file="${batch_file}.igv.log"   # shared log with R side

    # Helper: append timestamped line to .igv.log
    wlog() {
        local level="$1"; shift
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] [${level}] [WATCHER] $*" >> "$igv_log_file"
    }
    
    echo "[WATCHER] =========================================="
    echo "[WATCHER] Processing batch file: $batch_name"
    echo "[WATCHER] Patient: $patient_id"
    echo "[WATCHER] Batch path: $batch_file"
    wlog INFO "=== Watcher zpracování zahájeno ==="
    wlog INFO "Batch: $batch_file"
    wlog INFO "Patient: $patient_id | Display: :$display_num"
    
    # Check if already processed
    if [ -f "$done_file" ] || [ -f "$error_file" ]; then
        echo "[WATCHER] Already processed, skipping..."
        return
    fi
    
    # Use display number passed as parameter (assigned sequentially in main loop)
    export DISPLAY=":${display_num}"
    
    echo "[WATCHER] Using DISPLAY=${DISPLAY}"
    echo "[WATCHER] Starting Xvfb on ${DISPLAY}..."
    
    # Start Xvfb for this display (with -nolisten tcp for security)
    # NOTE: Do NOT pipe output through sed — bash's $! captures the last command
    # in a pipeline (sed), not Xvfb, so kill $xvfb_pid would never kill Xvfb.
    local xvfb_log="${batch_file}.xvfb.log"
    Xvfb ${DISPLAY} -screen 0 1280x1024x24 -nolisten tcp >"$xvfb_log" 2>&1 &
    local xvfb_pid=$!
    sleep 2

    # Check if Xvfb started successfully
    if ! kill -0 $xvfb_pid 2>/dev/null; then
        echo "[WATCHER] ❌ ERROR: Xvfb failed to start (check $xvfb_log)"
        wlog ERROR "Xvfb se nepodařilo spustit na DISPLAY :$display_num"
        wlog ERROR "=== Watcher zpracování ukončeno (XVFB FAIL) ==="
        echo "Failed to start Xvfb" > "$error_file"
        return 1
    fi

    echo "[WATCHER] Xvfb started (PID: $xvfb_pid)"
    echo "[WATCHER] Executing IGV batch script..."

    # Parse snapshot output directory and expected count from batch file
    local snapshot_dir
    snapshot_dir=$(grep -m1 "^snapshotDirectory" "$batch_file" | awk '{print $2}')
    local expected_count
    expected_count=$(grep -c "^snapshot " "$batch_file" 2>/dev/null || echo 0)
    echo "[WATCHER] Expected snapshots: $expected_count in $snapshot_dir"
    wlog INFO "Očekávaných PNG: $expected_count | Výstupní složka: $snapshot_dir"

    # ── Pre-IGV diagnostics ──────────────────────────────────────────────────
    echo "[WATCHER] --- Pre-IGV diagnostics ---"
    echo "[WATCHER] whoami    : $(whoami 2>/dev/null || id)"
    echo "[WATCHER] id        : $(id)"
    echo "[WATCHER] HOME      : $HOME"
    echo "[WATCHER] DISPLAY   : $DISPLAY"
    echo "[WATCHER] IGV_SCRIPT: $IGV_SCRIPT"
    echo "[WATCHER] igv.sh exists: $(test -f $IGV_SCRIPT && echo YES || echo NO)"
    echo "[WATCHER] igv.sh executable: $(test -x $IGV_SCRIPT && echo YES || echo NO)"
    echo "[WATCHER] /root perms       : $(ls -la / 2>/dev/null | grep ' root$' || echo 'cannot read')"
    echo "[WATCHER] /root/.igv        : $(ls -la /root/.igv 2>/dev/null || echo 'does not exist')"
    echo "[WATCHER] /root/.java       : $(ls -la /root/.java 2>/dev/null || echo 'does not exist')"
    echo "[WATCHER] write test /root/.igv: $(touch /root/.igv/.wtest 2>/dev/null && echo OK && rm -f /root/.igv/.wtest || echo FAILED)"
    echo "[WATCHER] snapshot_dir exists: $(test -d "$snapshot_dir" && echo YES || echo NO)"
    echo "[WATCHER] snapshot_dir writable: $(touch "${snapshot_dir}/.wtest" 2>/dev/null && echo YES && rm -f "${snapshot_dir}/.wtest" || echo NO)"
    echo "[WATCHER] batch file contents:"
    cat "$batch_file"
    echo "[WATCHER] --- End diagnostics ---"
    wlog INFO "Pre-IGV: whoami=$(whoami 2>/dev/null || id) | HOME=$HOME | igv.sh=$(test -x $IGV_SCRIPT && echo OK || echo MISSING)"
    # ─────────────────────────────────────────────────────────────────────────

    # Run IGV with a hard timeout (30 min max per patient) in the background
    timeout 1800 $IGV_SCRIPT -b "$batch_file" \
        > "${batch_file}.log" 2>&1 &
    local igv_pid=$!
    echo "[WATCHER] IGV started (PID: $igv_pid)"
    wlog INFO "IGV spuštěn (PID: $igv_pid) s hard-timeout 1800 s"

    # ── WATCHDOG ─────────────────────────────────────────────────────────────
    # Polls every 60 s while IGV is running.
    # Kill conditions:
    #   a) 0 PNGs after 120 s  (startup/load failure)
    #   b) PNG count not increased for 180 s  (stalled mid-run)
    (
        local wd_last_count=-1
        local wd_stall_since=0   # epoch seconds when stall started (0 = not stalling)
        local wd_elapsed=0
        local STALL_LIMIT=180    # seconds without new PNG before kill

        while kill -0 "$igv_pid" 2>/dev/null; do
            sleep 60
            wd_elapsed=$(( wd_elapsed + 60 ))

            local png_now=0
            [ -d "$snapshot_dir" ] && png_now=$(find "$snapshot_dir" -maxdepth 1 -name "*.png" | wc -l)

            # (a) Hard check at 120 s: still 0 PNGs → stuck at startup
            if [ "$wd_elapsed" -ge 120 ] && [ "$png_now" -eq 0 ]; then
                echo "[WATCHDOG] ⚠️  0 PNGs after ${wd_elapsed}s – startup stall. Killing IGV (PID $igv_pid)."
                kill "$igv_pid" 2>/dev/null
                {
                    echo "Watchdog: IGV killed - no PNG produced within ${wd_elapsed} seconds (startup stall)"
                    echo "---"
                    echo "Last 5 lines of IGV log:"
                    tail -5 "${batch_file}.log" 2>/dev/null || echo "(no log)"
                } > "$error_file"
                wlog WARN "Watchdog: IGV zabit – 0 PNG za ${wd_elapsed} s (startup stall, PID $igv_pid)"
                wlog WARN "=== Watcher zpracování ukončeno (WATCHDOG KILL startup) ==="
                exit 0
            fi

            # (b) Stall detection: PNG count not growing
            if [ "$png_now" -gt "$wd_last_count" ]; then
                # Progress made – reset stall timer
                echo "[WATCHDOG] Progress: ${png_now}/${expected_count} PNGs (was ${wd_last_count})"
                wd_last_count="$png_now"
                wd_stall_since=$(date +%s)
            else
                # No new PNG since last check
                if [ "$wd_stall_since" -gt 0 ]; then
                    local stall_secs=$(( $(date +%s) - wd_stall_since ))
                    echo "[WATCHDOG] No new PNG for ${stall_secs}s (${png_now}/${expected_count})"
                    if [ "$stall_secs" -ge "$STALL_LIMIT" ]; then
                        echo "[WATCHDOG] ⚠️  Stalled ${stall_secs}s with no new PNG – killing IGV (PID $igv_pid)."
                        kill "$igv_pid" 2>/dev/null
                        {
                            echo "Watchdog: IGV killed - no new PNG for ${stall_secs}s (stalled at ${png_now}/${expected_count})"
                            echo "---"
                            echo "Last 5 lines of IGV log:"
                            tail -5 "${batch_file}.log" 2>/dev/null || echo "(no log)"
                        } > "$error_file"
                        wlog WARN "Watchdog: IGV zabit – žádné nové PNG ${stall_secs} s (${png_now}/${expected_count}, PID $igv_pid)"
                        wlog WARN "=== Watcher zpracování ukončeno (WATCHDOG KILL stall) ==="
                        exit 0
                    fi
                else
                    # First time we see no progress (after having seen some)
                    wd_stall_since=$(date +%s)
                fi
            fi
        done
        echo "[WATCHDOG] IGV process ended – watchdog exiting normally"
    ) &
    local watchdog_pid=$!
    # ─────────────────────────────────────────────────────────────────────────

    # Write progress to .progress file while waiting
    (
        while kill -0 $igv_pid 2>/dev/null; do
            if [ -d "$snapshot_dir" ] && [ "$expected_count" -gt 0 ]; then
                done_pngs=$(find "$snapshot_dir" -maxdepth 1 -name "*.png" | wc -l)
                echo "$done_pngs/$expected_count" > "${batch_file}.progress"
            fi
            sleep 5
        done
    ) &
    local progress_pid=$!

    # Wait for IGV to finish (or be killed by watchdog / timeout)
    wait $igv_pid
    local igv_exit_code=$?

    # Stop watchdog and progress writer
    kill $watchdog_pid 2>/dev/null; wait $watchdog_pid 2>/dev/null
    kill $progress_pid 2>/dev/null; wait $progress_pid 2>/dev/null
    rm -f "${batch_file}.progress"
    # Kill Xvfb (now $xvfb_pid is the real Xvfb PID, not sed)
    kill $xvfb_pid 2>/dev/null; wait $xvfb_pid 2>/dev/null
    rm -f "$xvfb_log"
    echo "[WATCHER] Xvfb stopped (PID: $xvfb_pid)"

    local timeout_exit=124  # exit code from `timeout` when it kills the process
    if [ $igv_exit_code -eq $timeout_exit ]; then
        echo "[WATCHER] ⏱️  IGV hard-timeout (30 min) reached – process killed"
        wlog WARN "Hard-timeout 1800 s dosažen – IGV zabit"
        igv_exit_code=1
    fi
    
    echo "[WATCHER] IGV execution completed with exit code: $igv_exit_code"
    
    # Show relevant parts of IGV log if there was an error
    if [ $igv_exit_code -ne 0 ] && [ -f "${batch_file}.log" ]; then
        echo "[WATCHER] IGV error log (last 20 lines):"
        tail -20 "${batch_file}.log" | sed 's/^/[IGV LOG] /'
    fi

    # Create done or error file based on exit code
    # Also treat "exit 0 but 0 PNGs produced" as an error so R aborts quickly
    if [ ! -f "$error_file" ]; then
        if [ $igv_exit_code -eq 0 ]; then
            # Verify that at least one PNG was actually written
            local produced=0
            [ -d "$snapshot_dir" ] && produced=$(find "$snapshot_dir" -maxdepth 1 -name "*.png" | wc -l)
            if [ "$expected_count" -gt 0 ] && [ "$produced" -eq 0 ]; then
                echo "[WATCHER] ⚠️  IGV exited 0 but produced 0 / $expected_count PNGs – treating as error"
                {
                    echo "IGV exited with code 0 but produced no PNG snapshots ($produced/$expected_count)"
                    echo "---"
                    echo "Last 10 lines of IGV log:"
                    tail -10 "${batch_file}.log" 2>/dev/null || echo "(no log)"
                } > "$error_file"
                wlog ERROR "IGV exit 0 ale 0/$expected_count PNG – zpracováno jako chyba"
                wlog ERROR "=== Watcher zpracování ukončeno (0 PNG) ==="
            else
                echo "[WATCHER] ✅ SUCCESS – $produced PNG(s) produced"
                echo "success" > "$done_file"
                echo "[WATCHER] Done file created: $done_file"
                wlog INFO "Úspěch – $produced/$expected_count PNG vytvořeno"
                wlog INFO "=== Watcher zpracování ukončeno (SUCCESS) ==="
            fi
        else
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
            wlog ERROR "IGV skončil s exit code $igv_exit_code"
            wlog ERROR "=== Watcher zpracování ukončeno (IGV ERROR) ==="
        fi
    fi
    
    # Move to processed directory
    mkdir -p "$PROCESSED_DIR/$patient_id"
    cp "$batch_file" "$PROCESSED_DIR/$patient_id/"
    echo "[WATCHER] Batch file backed up to: $PROCESSED_DIR/$patient_id/"
    
    echo "[WATCHER] =========================================="
}

# Display counter for assigning unique Xvfb displays to parallel processes
DISPLAY_COUNTER=$XVFB_DISPLAY_START

# Main watch loop
echo "[WATCHER] Starting monitoring loop (checking every 2 seconds)..."
echo "[WATCHER] Press Ctrl+C to stop"

while true; do
    # Find all batch files that don't have .done, .error or .inprogress files
    # Use process substitution (< <(...)) instead of pipe so DISPLAY_COUNTER persists
    if [ -d "$WATCH_DIR" ]; then
        while read -r batch_file; do
            done_file="${batch_file}.done"
            error_file="${batch_file}.error"
            inprogress_file="${batch_file}.inprogress"
            
            # Only process if not already done, not errored, and not in progress
            if [ ! -f "$done_file" ] && [ ! -f "$error_file" ] && [ ! -f "$inprogress_file" ]; then
                # Check if file is complete (not being written)
                size1=$(stat -c%s "$batch_file" 2>/dev/null || echo 0)
                sleep 1
                size2=$(stat -c%s "$batch_file" 2>/dev/null || echo 0)
                
                if [ "$size1" -eq "$size2" ] && [ "$size1" -gt 0 ]; then
                    # Assign sequential display number (done before subshell, so counter increments correctly)
                    ASSIGNED_DISPLAY=$DISPLAY_COUNTER
                    DISPLAY_COUNTER=$(( (DISPLAY_COUNTER - XVFB_DISPLAY_START + 1) % 100 + XVFB_DISPLAY_START ))
                    
                    # Mark as in-progress BEFORE launching background process
                    touch "$inprogress_file"
                    echo "[WATCHER] Launching parallel processing for: $(basename $batch_file) on DISPLAY :$ASSIGNED_DISPLAY"
                    # Run in background so multiple patients process simultaneously
                    ( process_batch_file "$batch_file" "$ASSIGNED_DISPLAY"; rm -f "$inprogress_file" ) &
                fi
            fi
        done < <(find "$WATCH_DIR" -type f -name "*_batch.txt")
    fi
    
    sleep 2
done
