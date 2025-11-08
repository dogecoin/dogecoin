#!/bin/bash

# PAT Visual Simulator - PyInstaller Build Script
# Creates a standalone executable for easy distribution

echo "🚀 Building PAT Visual Simulator with PyInstaller..."
echo "==================================================="

# Check if pyinstaller is installed
if ! command -v pyinstaller &> /dev/null; then
    echo "❌ PyInstaller not found. Installing..."
    pip3 install pyinstaller
    if [ $? -ne 0 ]; then
        echo "❌ Failed to install PyInstaller. Please install manually: pip3 install pyinstaller"
        exit 1
    fi
fi

# Check for required dependencies before building
echo "🔍 Checking dependencies..."

# Check Python modules and capture output
DEP_CHECK=$(python3 -c "
import sys
missing = []

# Core dependencies
try:
    import numpy
    print('✅ numpy available')
except ImportError:
    missing.append('numpy')

try:
    import pygame
    print('✅ pygame available')
except ImportError:
    missing.append('pygame')

# Optional dependencies
try:
    import numba
    print('✅ numba available (optional)')
except ImportError:
    print('⚠️  numba not available (reduced performance)')

try:
    import psutil
    print('✅ psutil available (optional)')
except ImportError:
    print('⚠️  psutil not available (no energy monitoring)')

# Check pygame_gui (may not be available due to compatibility)
try:
    import pygame_gui
    print('✅ pygame_gui available (full GUI)')
except ImportError:
    print('⚠️  pygame_gui not available (basic mode only)')

if missing:
    print('MISSING_DEPS:', ' '.join(missing))
    sys.exit(1)
else:
    print('✅ All required dependencies available')
" 2>&1)

# Print the output
echo "$DEP_CHECK"

# Check if dependencies are missing
if echo "$DEP_CHECK" | grep -q "MISSING_DEPS:"; then
    MISSING_DEPS=$(echo "$DEP_CHECK" | grep "MISSING_DEPS:" | sed 's/MISSING_DEPS: //')
    echo "❌ Missing required dependencies: $MISSING_DEPS"
    echo "   Install with: pip3 install $MISSING_DEPS"
    exit 1
fi

if [ $? -ne 0 ]; then
    echo "❌ Dependency check failed"
    exit 1
fi

# Clean previous builds
echo "🧹 Cleaning previous builds..."
rm -rf build/ dist/ *.spec

# Build with PyInstaller
echo "📦 Building standalone executable..."

# Get the absolute path to the script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_PATH="$SCRIPT_DIR/pat_visual_sim.py"

if [ ! -f "$SCRIPT_PATH" ]; then
    echo "❌ Error: pat_visual_sim.py not found in $SCRIPT_DIR"
    exit 1
fi

# Build with the specified command
# Use full path to PyInstaller (may be installed in user directory)
PYINSTALLER_CMD="${HOME}/Library/Python/3.9/bin/pyinstaller"
if [ ! -x "$PYINSTALLER_CMD" ]; then
    # Try to find pyinstaller in PATH
    PYINSTALLER_CMD=$(which pyinstaller 2>/dev/null || echo "")
    if [ -z "$PYINSTALLER_CMD" ]; then
        echo "❌ PyInstaller not found in expected locations"
        echo "   Please ensure PyInstaller is installed and accessible"
        exit 1
    fi
fi

echo "📍 Using PyInstaller: $PYINSTALLER_CMD"

# Build with conditional assets inclusion
# Suppress pygame_gui hook warnings
export PYINSTALLER_DISABLE_PYGUI_HOOK=1

if [ -n "$(find assets -type f 2>/dev/null)" ]; then
    echo "📁 Including assets directory..."
    "$PYINSTALLER_CMD" --onefile --windowed --name PAT_Sim --add-data "assets/*:assets" "$SCRIPT_PATH" 2>&1 | grep -v "EntryPoint.*pygame_gui" || true
else
    echo "📁 No assets found, building without assets..."
    "$PYINSTALLER_CMD" --onefile --windowed --name PAT_Sim "$SCRIPT_PATH" 2>&1 | grep -v "EntryPoint.*pygame_gui" || true
fi

if [ $? -ne 0 ]; then
    echo "❌ PyInstaller build failed"
    exit 1
fi

# Check if build succeeded
if [ -f "dist/PAT_Sim" ]; then
    echo "✅ Build successful!"
    echo ""
    echo "📁 Files created:"
    echo "   • dist/PAT_Sim (standalone executable)"
    echo "   • PAT_Sim.spec (build specification)"
    echo ""
    echo "🎯 Installation & Usage:"
    echo "   1. Copy dist/PAT_Sim to your Applications folder (or Desktop)"
    echo "   2. Make executable: chmod +x PAT_Sim"
    echo "   3. Double-click PAT_Sim to run (no terminal needed!)"
    echo ""
    echo "💡 Features:"
    echo "   • Self-contained - no dependencies required"
    echo "   • Cross-platform executable"
    echo "   • Includes all PAT modules and assets"
    echo "   • Auto-handles pygame_gui compatibility"
    echo ""
    echo "🚀 Ready for distribution!"
else
    echo "❌ Build failed - executable not found in dist/"
    exit 1
fi
