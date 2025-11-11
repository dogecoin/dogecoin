# PAT (Paw Aggregation Technique) - Quantum-Resistant Signature Aggregation

## Overview

The Paw Aggregation Technique (PAT) incorporates the Post-Quantum Armor Wrapper (PAW), emphasizing resilient bundling against quantum threats. PAT provides quantum-resistant signature aggregation originally designed for Dogecoin's Scrypt-based PoW, but engineered for seamless adoption across heterogeneous blockchain networks.

## Multi-Chain Extensibility 🌐

PAT supports multiple blockchain architectures:

- **Dogecoin PoW**: Optimized for Scrypt mining economics with 34k+ compression ratios
- **Litecoin Scrypt**: Compatible with Litecoin's MWEB privacy extensions
- **Solana PoH**: Adaptable to SVM parallelization for 10x TPS improvements
- **Cross-Chain**: Unified framework supporting PoW, PoS, and PoH consensus models

## Economic Analysis

PAT economic projections use conservative 2025 low-fee data from BitInfoCharts/Statista/CoinLaw with 70-90% fee reductions for multi-sig batches.

High-volume users see ~5-50 DOGE monthly savings; miner revenue impacts are 5-15%. Results vary with mempool congestion—conservative estimates used to avoid exaggeration.

## Project Structure

```
pat/
├── docs/           # Academic paper, plots, references
├── src/            # Core PAT implementation
├── tools/          # Visual simulator, build scripts
├── extensions/     # Quantum sims, multi-chain, security proofs
├── requirements.txt # Python dependencies
└── pat-env/        # Virtual environment (optional)
```

## Interactive Visual Simulator

The PAT Visual Simulator (`tools/pat_visual_sim.py`) provides an educational neural network-style visualization of signature aggregation techniques, designed to be accessible to beginners and experts alike.

### Features
- **🧠 Neural Network Visualization**: Interactive node graphs showing signature aggregation
- **🎓 Guided Educational Demo**: Step-by-step explanations for beginners (n=1000)
- **🎮 Layperson-Friendly Interface**: Click buttons or use simple keyboard controls
- **⚡ Real-time Animations**: Frame-by-frame merging with spiking neural effects
- **🔧 Auto-Dependency Management**: Automatically installs missing packages
- **📊 Performance Optimized**: Handles 1000+ signatures with 30+ FPS on modern hardware
- **🎨 Minimalist Design**: Clean black background with pulsing blue neural connections

### Compatibility Notes

For full GUI functionality, use compatible pygame versions:

```bash
# Install compatible versions for pygame_gui support
pip3 uninstall pygame pygame_gui
pip3 install 'pygame==2.5.2' 'pygame_gui==0.6.0'
```

**Important**: Newer pygame versions (>2.5.2) cause `pygame_gui` compatibility issues with `DIRECTION_LTR` errors. The simulator will automatically detect version conflicts and provide installation guidance.

### Usage

**For Beginners (Recommended):**
```bash
# Double-click pat_visual_sim.py or run:
python3 tools/pat_visual_sim.py

# Follow the on-screen prompts:
# 1. Click "🚀 Run Guided Demo" for educational tour
# 2. Or use keyboard: ENTER for demo, SPACE for manual controls
```

**The script automatically:**
- Detects and fixes pygame_gui compatibility issues
- Installs missing numpy, numba, or psutil dependencies
- Provides layperson-friendly interface with explanations
- Offers guided demo with 1000 signatures and animations

**Keyboard Controls (when GUI unavailable):**
- `ENTER` - Start guided demo (n=1000)
- `SPACE` - Manual simulator controls
- `↑↓` - Adjust signature count (±100)
- `←→` or `S` - Cycle aggregation strategies
- `T` - Cycle threat levels
- `A` - Toggle attack visualization
- `S` - Save screenshot (pat_sim_YYYYMMDD_HHMMSS.png)
- `V` - Export video (requires moviepy)
- `ESC` - Exit

### Real-Time Metrics Overlay

The simulator displays **real performance data** from benchmark results in the lower panel:

- **📊 Compression ratios** (e.g., "Compression: 672k+ | Energy: 80% saved")
- **⚡ Energy consumption** and memory usage from actual benchmark runs
- **🚀 Throughput metrics** (signatures per second)
- **⏱️ Verification times** and other performance indicators

**Data Source:** `src/pat_comprehensive_benchmark_results.csv` with real measurement data.

### Browser-Based Web Simulator

**🎯 Interactive 3D Web Visualization:**
- **Open `tools/pat_web_sim/index.html`** in any modern web browser
- **No installation required** - runs directly in browser
- **3D neural network visualization** with Three.js
- **Real-time interactive controls** for all parameters
- **WebGL-accelerated animations** and particle effects

**Key Features:**
- **3D Node Graph**: Gray spheres connected by pulsing blue lines
- **Interactive Sliders**: Adjust signature count (10-10,000)
- **Strategy Selection**: Logarithmic, threshold, Merkle batch, stacked multi
- **Threat Levels**: LOW/MEDIUM/HIGH with color-coded nodes
- **Blockchain Selection**: Dogecoin, Litecoin, Solana with specific optimizations
- **Demo Animation**: Automated n=1000 visualization with particle bursts
- **Export PNG**: Save screenshots directly from browser
- **Real Metrics**: Live benchmark data overlay

**Usage:**
```bash
# Open in default browser
open tools/pat_web_sim/index.html

# Or drag index.html into browser
# Or serve locally: python3 -m http.server 8000
```

**Shareable States:**
Create shareable links with specific configurations:
```bash
# High threat Dogecoin simulation
tools/pat_web_sim/index.html?n=1000&threat=HIGH&chain=DOGECOIN

# Low threat Solana demo
tools/pat_web_sim/index.html?n=500&threat=LOW&chain=SOLANA

# Medium threat Litecoin benchmark
tools/pat_web_sim/index.html?n=2000&threat=MEDIUM&chain=LITECOIN
```

**Keyboard Controls:**
- **T**: Cycle threat levels (LOW → MEDIUM → HIGH)
- **S**: Cycle aggregation strategies
- **C**: Cycle blockchain chains
- **A**: Toggle attack visualization
- **R**: Run merge animation
- **Q**: Toggle quantum view
- **D**: Run demo (n=1000)
- **SPACE**: Reset camera view

**Accessibility:**
- Full ARIA labels for screen readers
- Keyboard navigation for all controls
- High contrast color schemes

**Performance Profiling:**
- Built-in Stats.js FPS/memory monitoring (top-right corner)
- Automatic detail reduction for n>5000
- WebGL shader optimizations

**Perfect for:**
- **Educational presentations** and tutorials
- **Quick demonstrations** without Python installation
- **GitHub Pages hosting** for shareable links
- **Cross-platform compatibility** (works on any device)

### Export Features

**Desktop Application Screenshots:**
- Press `S` during simulation to save current visualization
- Files saved as: `pat_sim_YYYYMMDD_HHMMSS.png`
- Captures complete window including animations and status

**Desktop Video Export:**
- Press `V` during simulation to start video recording
- Requires moviepy: `pip3 install moviepy`
- Creates MP4 video of simulation with animations

**Web Browser Export:**
- Click "📸 Export PNG" button in web simulator
- Saves current 3D visualization as PNG image
- Works directly in browser without server requirements

### Building Standalone App

For easy distribution without requiring users to install dependencies:

```bash
cd tools
./build_app.sh
```

This creates a standalone executable (`dist/PAT_Sim`) that includes all dependencies and can be run on any compatible system.

**Usage:**
```bash
# Build the app
./build_app.sh

# Run the standalone executable
./dist/PAT_Sim
# Or copy to Applications/Desktop and double-click
```

**Features:**
- Self-contained executable with no external dependencies
- Cross-platform compatibility
- Includes all PAT modules and assets
- Auto-handles pygame_gui compatibility issues
- **Real-time metrics overlay** from benchmark data
- Ready for distribution and sharing

## Getting Started

See `src/README_PAT.md` for detailed setup and usage instructions.

## Academic Resources

- `docs/academic_paper.tex` - Main research paper
- `docs/references.bib` - Bibliography
- `docs/generate_paper_plots.py` - Plot generation scripts
