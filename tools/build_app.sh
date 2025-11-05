#!/bin/bash

# PAT Visual Simulator - macOS .app Build Script
# Creates a double-clickable macOS application bundle

echo "🚀 Building PAT Visual Simulator .app bundle..."
echo "==============================================="

# Check if required dependencies are available
echo "📋 Checking dependencies..."
python3 -c "
import sys
missing = []
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

if missing:
    print(f'❌ Missing required dependencies: {\" \".join(missing)}')
    print('Install with: pip install numpy pygame')
    sys.exit(1)
else:
    print('✅ Core dependencies available')
"

# Create assets directory if it doesn't exist
mkdir -p assets

# Build the .app bundle
echo "🔨 Building macOS .app bundle..."

# Use the installed pyinstaller (adjust path if needed)
PYINSTALLER_CMD="/Users/caseymacmini/Library/Python/3.9/bin/pyinstaller"

echo "🔍 Checking PyInstaller at: $PYINSTALLER_CMD"
echo "   File exists: $([ -f "$PYINSTALLER_CMD" ] && echo 'Yes' || echo 'No')"
echo "   Is executable: $([ -x "$PYINSTALLER_CMD" ] && echo 'Yes' || echo 'No')"

if [ ! -x "$PYINSTALLER_CMD" ]; then
    echo "❌ PyInstaller not found at $PYINSTALLER_CMD"
    echo "   Current USER: $USER"
    echo "   Please install with: pip install pyinstaller"
    echo "   Or update the PYINSTALLER_CMD path in this script"
    exit 1
fi

echo "📍 Using PyInstaller: $PYINSTALLER_CMD"

# Build with assets if they exist
if [ -n "$(find assets -type f 2>/dev/null)" ]; then
    "$PYINSTALLER_CMD" --onedir \
        --windowed \
        --name PAT_Sim \
        --add-data "assets/*:assets" \
        tools/pat_visual_sim.py
else
    "$PYINSTALLER_CMD" --onedir \
        --windowed \
        --name PAT_Sim \
        tools/pat_visual_sim.py
fi

# Check if build was successful
if [ -d "dist" ] && [ -f "dist/PAT_Sim.app/Contents/MacOS/PAT_Sim" ]; then
    echo "✅ Build successful!"
    echo "📁 App bundle created: dist/PAT_Sim.app"
    echo ""
    echo "🎯 Installation Instructions:"
    echo "1. Copy dist/PAT_Sim.app to your Applications folder"
    echo "2. Double-click PAT_Sim.app to run (no terminal needed!)"
    echo "3. The app will check for dependencies on first run"
    echo ""
    echo "📊 Bundle Info:"
    ls -lh dist/PAT_Sim.app/Contents/MacOS/PAT_Sim
    echo ""
    echo "🧪 Test the bundle:"
    echo "   open dist/PAT_Sim.app"
else
    echo "❌ Build failed!"
    echo "💡 Check the error messages above"
    exit 1
fi

echo "🎉 PAT Visual Simulator .app bundle ready!"
echo "   Double-click to run - no terminal required!"
