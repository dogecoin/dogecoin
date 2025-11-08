// PAT Visual Simulator - Browser Edition
// Interactive 3D visualization of Post-Quantum Aggregation Technique

class PATWebSimulator {
    constructor() {
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;

        // Simulation parameters
        this.numSignatures = 1000;
        this.strategy = 'logarithmic';
        this.threatLevel = 'MEDIUM';
        this.selectedChain = 'DOGECOIN';
        this.showAttacks = false;

        // 3D objects
        this.nodes = [];
        this.connections = [];
        this.particles = [];

        // Animation state
        this.animationFrame = 0;
        this.isAnimating = false;
        this.demoMode = false;

        // Data
        this.benchmarkData = {};

        // Colors
        this.colors = {
            node: 0x808080,        // Gray nodes
            connection: 0x0080ff,  // Blue connections
            secure: 0x00ff00,      // Green for secure
            attack: 0xff0000,      // Red for attacks
            threatLow: 0x00ff80,   // Green tint for LOW
            threatMedium: 0xffff80, // Yellow tint for MEDIUM
            threatHigh: 0xff8080   // Red tint for HIGH
        };

        this.init();
    }

    init() {
        this.setupScene();
        this.setupControls();
        this.loadBenchmarkData();
        this.createInitialVisualization();
        this.setupEventListeners();
        this.animate();
        this.hideLoading();
    }

    setupScene() {
        // Create scene
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x000000);

        // Create camera
        this.camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 2000);
        this.camera.position.set(0, 0, 500);

        // Create renderer
        this.renderer = new THREE.WebGLRenderer({ canvas: document.getElementById('canvas'), antialias: true });
        this.renderer.setSize(window.innerWidth, window.innerHeight);
        this.renderer.setPixelRatio(window.devicePixelRatio);

        // Add lighting
        const ambientLight = new THREE.AmbientLight(0x404040, 0.6);
        this.scene.add(ambientLight);

        const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
        directionalLight.position.set(1, 1, 1);
        this.scene.add(directionalLight);

        // Handle window resize
        window.addEventListener('resize', () => {
            this.camera.aspect = window.innerWidth / window.innerHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(window.innerWidth, window.innerHeight);
        });
    }

    setupControls() {
        // Set up OrbitControls for mouse interaction
        this.controls = {
            mouseX: 0,
            mouseY: 0,
            isMouseDown: false,
            update: () => {
                // Simple rotation based on mouse position
                if (!this.isMouseDown) {
                    this.camera.position.x += (this.controls.mouseX - this.camera.position.x) * 0.01;
                    this.camera.position.y += (-this.controls.mouseY - this.camera.position.y) * 0.01;
                    this.camera.lookAt(this.scene.position);
                }
            }
        };

        // Mouse event listeners
        document.addEventListener('mousemove', (event) => {
            this.controls.mouseX = (event.clientX - window.innerWidth / 2) * 0.5;
            this.controls.mouseY = (event.clientY - window.innerHeight / 2) * 0.5;
        });

        document.addEventListener('mousedown', () => {
            this.controls.isMouseDown = true;
        });

        document.addEventListener('mouseup', () => {
            this.controls.isMouseDown = false;
        });

        // Keyboard controls
        document.addEventListener('keydown', (event) => {
            switch(event.key.toLowerCase()) {
                case 't':
                    this.cycleThreatLevel();
                    break;
                case 's':
                    this.cycleStrategy();
                    break;
                case 'c':
                    this.cycleChain();
                    break;
                case 'a':
                    this.toggleAttacks();
                    break;
                case ' ':
                    this.resetView();
                    break;
            }
        });
    }

    cycleThreatLevel() {
        const levels = ['LOW', 'MEDIUM', 'HIGH'];
        const currentIndex = levels.indexOf(this.threatLevel);
        this.threatLevel = levels[(currentIndex + 1) % levels.length];
        document.getElementById('threat-select').value = this.threatLevel;
        this.updateVisualization();
        this.updateStatus(`Threat Level: ${this.threatLevel}`);
    }

    cycleStrategy() {
        const strategies = ['logarithmic', 'threshold', 'merkle_batch', 'stacked_multi'];
        const currentIndex = strategies.indexOf(this.strategy);
        this.strategy = strategies[(currentIndex + 1) % strategies.length];
        document.getElementById('strategy-select').value = this.strategy;
        this.updateVisualization();
        this.updateStatus(`Strategy: ${this.strategy}`);
    }

    cycleChain() {
        const chains = ['DOGECOIN', 'LITECOIN', 'SOLANA'];
        const currentIndex = chains.indexOf(this.selectedChain);
        this.selectedChain = chains[(currentIndex + 1) % chains.length];
        document.getElementById('chain-select').value = this.selectedChain;
        this.updateVisualization();
        this.updateStatus(`Chain: ${this.selectedChain}`);
    }

    toggleAttacks() {
        this.showAttacks = !this.showAttacks;
        this.updateVisualization();
        this.updateStatus(`Attack Visualization: ${this.showAttacks ? 'ON' : 'OFF'}`);
    }

    resetView() {
        this.camera.position.set(0, 0, 500);
        this.camera.lookAt(this.scene.position);
        this.updateStatus('View Reset');
    }

    setupEventListeners() {
        // Slider controls
        const signatureSlider = document.getElementById('signature-slider');
        const signatureValue = document.getElementById('signature-value');

        signatureSlider.addEventListener('input', (event) => {
            this.numSignatures = parseInt(event.target.value);
            signatureValue.textContent = this.numSignatures;
            this.updateVisualization();
        });

        // Dropdown controls
        document.getElementById('threat-select').addEventListener('change', (event) => {
            this.threatLevel = event.target.value;
            this.updateVisualization();
        });

        document.getElementById('strategy-select').addEventListener('change', (event) => {
            this.strategy = event.target.value;
            this.updateVisualization();
        });

        document.getElementById('chain-select').addEventListener('change', (event) => {
            this.selectedChain = event.target.value;
            this.updateVisualization();
        });

        // Demo button
        document.getElementById('demo-btn').addEventListener('click', () => {
            this.runDemo();
        });

        // Export button
        document.getElementById('export-btn').addEventListener('click', () => {
            this.exportPNG();
        });
    }

    createInitialVisualization() {
        this.clearScene();
        this.createTreeVisualization();
        this.updateMetricsDisplay();
    }

    clearScene() {
        // Remove all nodes and connections
        this.nodes.forEach(node => this.scene.remove(node));
        this.connections.forEach(conn => this.scene.remove(conn));
        this.particles.forEach(particle => this.scene.remove(particle));

        this.nodes = [];
        this.connections = [];
        this.particles = [];
    }

    createTreeVisualization() {
        const maxNodes = Math.min(this.numSignatures, 1000); // Limit for performance
        const radius = 300;
        const height = 200;

        // Create nodes in a 3D arc pattern (like a neural network)
        for (let i = 0; i < maxNodes; i++) {
            const angle = (i / maxNodes) * Math.PI * 2;
            const y = (Math.sin(angle) * height) + (Math.random() - 0.5) * 50;
            const x = Math.cos(angle) * radius + (Math.random() - 0.5) * 100;
            const z = Math.sin(angle * 2) * radius * 0.3 + (Math.random() - 0.5) * 100;

            this.createNode(x, y, z, i);
        }

        // Create connections based on strategy
        this.createConnections();
    }

    createNode(x, y, z, index) {
        const geometry = new THREE.SphereGeometry(3, 8, 8);
        const material = new THREE.MeshPhongMaterial({
            color: this.getNodeColor(index),
            transparent: true,
            opacity: 0.8
        });

        const node = new THREE.Mesh(geometry, material);
        node.position.set(x, y, z);
        node.userData = { index, originalColor: material.color.getHex() };

        this.scene.add(node);
        this.nodes.push(node);
    }

    getNodeColor(index) {
        let baseColor = this.colors.node;

        // Apply threat level coloring
        switch(this.threatLevel) {
            case 'LOW':
                baseColor = this.colors.threatLow;
                break;
            case 'MEDIUM':
                baseColor = this.colors.threatMedium;
                break;
            case 'HIGH':
                baseColor = this.colors.threatHigh;
                break;
        }

        // Add some attack visualization
        if (this.showAttacks && Math.random() < 0.1) {
            baseColor = this.colors.attack;
        }

        return baseColor;
    }

    createConnections() {
        this.connections.forEach(conn => this.scene.remove(conn));
        this.connections = [];

        const nodeCount = this.nodes.length;

        // Different connection patterns based on strategy
        switch(this.strategy) {
            case 'logarithmic':
                this.createLogarithmicConnections(nodeCount);
                break;
            case 'threshold':
                this.createThresholdConnections(nodeCount);
                break;
            case 'merkle_batch':
                this.createMerkleConnections(nodeCount);
                break;
            case 'stacked_multi':
                this.createStackedConnections(nodeCount);
                break;
        }
    }

    createLogarithmicConnections(nodeCount) {
        // Logarithmic merging pattern (O(log n) connections)
        for (let i = 0; i < nodeCount; i += 2) {
            if (i + 1 < nodeCount) {
                this.createConnection(i, i + 1);
                if (i + 2 < nodeCount) {
                    this.createConnection(i, i + 2); // Additional connections for tree structure
                }
            }
        }
    }

    createThresholdConnections(nodeCount) {
        // Fixed threshold groups
        const groupSize = 10;
        for (let i = 0; i < nodeCount; i++) {
            const groupStart = Math.floor(i / groupSize) * groupSize;
            for (let j = 1; j < groupSize && groupStart + j < nodeCount; j++) {
                if (i !== groupStart + j) {
                    this.createConnection(i, groupStart + j);
                }
            }
        }
    }

    createMerkleConnections(nodeCount) {
        // Merkle tree structure
        let levelSize = nodeCount;
        let startIndex = 0;

        while (levelSize > 1) {
            for (let i = 0; i < levelSize; i += 2) {
                const left = startIndex + i;
                const right = startIndex + i + 1;
                if (right < nodeCount) {
                    this.createConnection(left, right);
                }
            }
            startIndex += levelSize;
            levelSize = Math.ceil(levelSize / 2);
        }
    }

    createStackedConnections(nodeCount) {
        // No compression - all-to-all connections (limited for performance)
        const maxConnections = 50;
        for (let i = 0; i < Math.min(nodeCount, maxConnections); i++) {
            for (let j = i + 1; j < Math.min(nodeCount, maxConnections); j++) {
                this.createConnection(i, j);
            }
        }
    }

    createConnection(fromIndex, toIndex) {
        if (fromIndex >= this.nodes.length || toIndex >= this.nodes.length) return;

        const fromNode = this.nodes[fromIndex];
        const toNode = this.nodes[toIndex];

        const geometry = new THREE.BufferGeometry().setFromPoints([
            fromNode.position,
            toNode.position
        ]);

        const material = new THREE.LineBasicMaterial({
            color: this.colors.connection,
            transparent: true,
            opacity: 0.6
        });

        const line = new THREE.Line(geometry, material);
        this.scene.add(line);
        this.connections.push(line);
    }

    createBurstEffect(position) {
        // Create particle burst effect
        const particleCount = 20;
        const particles = [];

        for (let i = 0; i < particleCount; i++) {
            const geometry = new THREE.SphereGeometry(1, 4, 4);
            const material = new THREE.MeshBasicMaterial({
                color: this.colors.connection,
                transparent: true,
                opacity: 0.8
            });

            const particle = new THREE.Mesh(geometry, material);

            // Random direction
            const angle = Math.random() * Math.PI * 2;
            const speed = Math.random() * 5 + 2;
            particle.velocity = {
                x: Math.cos(angle) * speed,
                y: Math.sin(angle) * speed,
                z: (Math.random() - 0.5) * speed
            };

            particle.position.copy(position);
            particle.userData = { life: 60 }; // 60 frames lifetime

            this.scene.add(particle);
            particles.push(particle);
        }

        this.particles.push(...particles);
    }

    updateVisualization() {
        this.clearScene();
        this.createTreeVisualization();
        this.updateMetricsDisplay();
    }

    animate() {
        requestAnimationFrame(() => this.animate());

        this.animationFrame++;
        this.controls.update();

        // Update node animations (pulsing)
        this.nodes.forEach((node, index) => {
            const pulse = Math.sin(this.animationFrame * 0.02 + index * 0.1) * 0.1 + 0.9;
            node.scale.setScalar(pulse);
        });

        // Update connection animations
        this.connections.forEach((connection, index) => {
            const opacity = Math.sin(this.animationFrame * 0.03 + index * 0.05) * 0.3 + 0.7;
            connection.material.opacity = opacity;
        });

        // Update particles
        this.particles = this.particles.filter(particle => {
            particle.position.x += particle.velocity.x;
            particle.position.y += particle.velocity.y;
            particle.position.z += particle.velocity.z;

            particle.userData.life--;

            // Fade out
            particle.material.opacity = particle.userData.life / 60;

            if (particle.userData.life <= 0) {
                this.scene.remove(particle);
                return false;
            }
            return true;
        });

        // Create occasional bursts during demo
        if (this.demoMode && this.animationFrame % 120 === 0) {
            const randomNode = this.nodes[Math.floor(Math.random() * this.nodes.length)];
            if (randomNode) {
                this.createBurstEffect(randomNode.position);
            }
        }

        this.renderer.render(this.scene, this.camera);
    }

    runDemo() {
        this.demoMode = true;
        this.numSignatures = 1000;
        document.getElementById('signature-slider').value = 1000;
        document.getElementById('signature-value').textContent = 1000;

        this.updateStatus('🚀 Running Demo Animation...');
        this.updateVisualization();

        // Create a sequence of bursts
        let burstCount = 0;
        const burstInterval = setInterval(() => {
            if (burstCount < 10) {
                const randomNode = this.nodes[Math.floor(Math.random() * this.nodes.length)];
                if (randomNode) {
                    this.createBurstEffect(randomNode.position);
                }
                burstCount++;
            } else {
                clearInterval(burstInterval);
                this.demoMode = false;
                this.updateStatus('Demo Complete - Interactive Mode');
            }
        }, 500);
    }

    exportPNG() {
        try {
            // Render the scene to canvas
            this.renderer.render(this.scene, this.camera);

            // Get image data from canvas
            const canvas = document.getElementById('canvas');
            const link = document.createElement('a');
            link.download = `pat_sim_${Date.now()}.png`;
            link.href = canvas.toDataURL();
            link.click();

            this.updateStatus('📸 Screenshot exported!');
        } catch (error) {
            console.error('Export failed:', error);
            this.updateStatus('❌ Export failed');
        }
    }

    async loadBenchmarkData() {
        try {
            const response = await fetch('pat_benchmarks.csv');
            const csvText = await response.text();
            this.parseBenchmarkCSV(csvText);
            this.updateStatus('✅ Benchmark data loaded');
        } catch (error) {
            console.warn('Failed to load benchmark CSV, using mock data:', error);
            this.mockBenchmarkData();
            this.updateStatus('⚠️ Using demo benchmark data');
        }
        this.updateMetricsDisplay();
    }

    parseBenchmarkCSV(csvText) {
        const lines = csvText.split('\n');
        const headers = lines[0].split(',');

        this.benchmarkData = {};

        for (let i = 1; i < lines.length; i++) {
            const values = lines[i].split(',');
            if (values.length < 10) continue;

            const row = {};
            headers.forEach((header, index) => {
                row[header] = values[index];
            });

            // Extract relevant data
            const signatures = parseInt(row.Signatures || row.num_signatures || '0');
            const strategy = (row.Strategy || row.strategy || 'Unknown').toLowerCase();

            if (signatures > 0 && strategy !== 'unknown') {
                const key = `${signatures}_${strategy}`;
                this.benchmarkData[key] = {
                    compression_ratio: parseFloat(row.Compression_Ratio || row.compression_ratio || '1.0'),
                    energy_consumption: parseFloat(row.energy_consumption_kwh || '0'),
                    throughput: parseFloat(row.throughput_sigs_per_sec || '0'),
                    verify_time: parseFloat(row.Avg_Verify_Time_ms || row.Verify_Time_ms || '0'),
                    memory_peak: parseFloat(row.Peak_Memory_Aggregation_KB || row.memory_peak_mb || '0')
                };
            }
        }

        console.log(`Loaded ${Object.keys(this.benchmarkData).length} benchmark data points`);
    }

    mockBenchmarkData() {
        // Fallback mock data when CSV loading fails
        this.benchmarkData = {
            '1000_logarithmic': {
                compression_ratio: 35550.7,
                energy_consumption: 0.02,
                throughput: 1250,
                verify_time: 3.2,
                memory_peak: 1024
            },
            '500_logarithmic': {
                compression_ratio: 17775.3,
                energy_consumption: 0.015,
                throughput: 2100,
                verify_time: 2.8,
                memory_peak: 512
            },
            '100_merkle_batch': {
                compression_ratio: 3774.2,
                energy_consumption: 0.008,
                throughput: 3800,
                verify_time: 2.1,
                memory_peak: 256
            },
            '25_threshold': {
                compression_ratio: 888.8,
                energy_consumption: 0.005,
                throughput: 5200,
                verify_time: 1.8,
                memory_peak: 128
            }
        };
    }

    updateMetricsDisplay() {
        const metricsContent = document.getElementById('metrics-content');
        const key = `${this.numSignatures}_${this.strategy}`;

        let metrics = this.benchmarkData[key];
        if (!metrics) {
            // Find closest match
            const keys = Object.keys(this.benchmarkData);
            const closestKey = keys.find(k => k.includes(`_${this.strategy}`)) || keys[0];
            metrics = this.benchmarkData[closestKey];
        }

        if (metrics) {
            metricsContent.innerHTML = `
                <div class="metric-item">Compression: <span class="metric-value">${metrics.compression_ratio.toFixed(1)}x</span></div>
                <div class="metric-item">Energy Saved: <span class="metric-value">${((1 - metrics.energy_consumption) * 100).toFixed(0)}%</span></div>
                <div class="metric-item">Throughput: <span class="metric-value">${metrics.throughput} sigs/sec</span></div>
                <div class="metric-item">Verify Time: <span class="metric-value">${metrics.verify_time}ms</span></div>
            `;
        } else {
            metricsContent.innerHTML = '<div class="metric-item">Loading performance data...</div>';
        }
    }

    updateStatus(message) {
        document.getElementById('status-text').textContent = message;
        setTimeout(() => {
            document.getElementById('status-text').textContent = `PAT Simulator - ${this.numSignatures} signatures`;
        }, 3000);
    }

    hideLoading() {
        setTimeout(() => {
            document.getElementById('loading').style.display = 'none';
        }, 1000);
    }
}

// Initialize the simulator when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new PATWebSimulator();
});
