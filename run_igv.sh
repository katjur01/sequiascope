#!/bin/bash

# IGV Wrapper Script for xvfb
# Usage: ./run_igv.sh path/to/batch_file.txt

if [ $# -ne 1 ]; then
    echo "Usage: $0 <batch_file_path>"
    exit 1
fi

BATCH_FILE="$1"
DISPLAY_NUM=8
TIMEOUT_DURATION=100  # Increased for larger batches

echo "Starting IGV with batch file: $BATCH_FILE"

# Function to cleanup
cleanup() {
    echo "Cleaning up..."
    if [ ! -z "$XVFB_PID" ]; then
        kill $XVFB_PID 2>/dev/null
        wait $XVFB_PID 2>/dev/null
    fi
}

# Set trap for cleanup on exit
trap cleanup EXIT

# Start Xvfb with extra extensions
echo "Starting Xvfb on display :$DISPLAY_NUM..."
Xvfb :$DISPLAY_NUM -screen 0 1920x1080x24 -ac -extension GLX -extension RENDER -extension RANDR 2> xvfb.err &
XVFB_PID=$!
sleep 5  # Increased wait for stability

# Check if Xvfb started
if ! kill -0 $XVFB_PID 2>/dev/null; then
    echo "ERROR: Failed to start Xvfb"
    cat xvfb.err
    exit 1
fi

# Verify display
export DISPLAY=:$DISPLAY_NUM
xdpyinfo > xdpy.out 2> xdpy.err
if [ -s xdpy.err ]; then
    echo "WARNING: xdpyinfo reported issues"
    cat xdpy.err
fi

# Set Java options
export JAVA_TOOL_OPTIONS="-Djava.awt.headless=false -Dawt.toolkit=sun.awt.X11Toolkit -Xmx16g -Dsun.java2d.opengl=false"

# Check Java toolkit
/usr/lib/jvm/java-11-openjdk-amd64/bin/java -XshowSettings:properties -Djava.awt.headless=false 2>&1 | grep awt.toolkit > toolkit.out
echo "Java AWT toolkit: $(cat toolkit.out)"

echo "Running IGV..."
timeout $TIMEOUT_DURATION /usr/bin/IGV_2.16.1/igv.sh -b "$BATCH_FILE" > igv.out 2> igv.err
IGV_EXIT_CODE=$?

# Dump logs
echo "IGV stdout:"
cat igv.out
echo "IGV stderr:"
cat igv.err
echo "Xvfb stderr:"
cat xvfb.err

if [ $IGV_EXIT_CODE -eq 124 ]; then
    echo "WARNING: IGV timed out after $TIMEOUT_DURATION seconds"
elif [ $IGV_EXIT_CODE -eq 0 ]; then
    echo "IGV completed successfully"
else
    echo "IGV exited with code: $IGV_EXIT_CODE"
fi

echo "Done."
exit $IGV_EXIT_CODE