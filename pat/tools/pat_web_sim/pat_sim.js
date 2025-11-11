// PAT Visual Simulator - Browser Edition
// Interactive 3D visualization of Post-Quantum Aggregation Technique (PAT)
// featuring Post-Quantum Armor Wrapper (PAW) for quantum-resistant bundling

class PATWebSimulator {
    constructor() {
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;

        // Simulation parameters
        this.numSignatures = 100; // Test with n=100 for quantum sim
        this.strategy = 'logarithmic';
        this.threatLevel = 'MEDIUM';
        this.selectedChain = 'DOGECOIN';
        this.showAttacks = false;
        this.quantumView = false;

        // 3D objects
        this.nodes = [];
        this.connections = [];
        this.particles = [];

        // Animation state
        this.animationFrame = 0;
        this.isAnimating = true;
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
        this.parseURLParams();
        this.setupScene();
        this.setupControls();
        this.loadBenchmarkData();
        this.createInitialVisualization();
        this.setupEventListeners();
        this.animate();
        this.hideLoading();
    }

    parseURLParams() {
        // Shareable states: parse URL parameters for n, threat, chain
        const urlParams = new URLSearchParams(window.location.search);

        if (urlParams.has('n')) {
            const n = parseInt(urlParams.get('n'));
            if (n >= 10 && n <= 10000) {
                this.numSignatures = n;
            }
        }

        if (urlParams.has('threat')) {
            const threat = urlParams.get('threat').toUpperCase();
            if (['LOW', 'MEDIUM', 'HIGH'].includes(threat)) {
                this.threatLevel = threat;
            }
        }

        if (urlParams.has('chain')) {
            const chain = urlParams.get('chain').toUpperCase();
            if (['DOGECOIN', 'LITECOIN', 'SOLANA'].includes(chain)) {
                this.selectedChain = chain;
            }
        }

        // Update UI elements to match parsed parameters
        if (this.numSignatures !== 100) {
            document.getElementById('signature-slider').value = this.numSignatures;
            document.getElementById('signature-value').textContent = this.numSignatures;
        }
        document.getElementById('threat-select').value = this.threatLevel;
        document.getElementById('chain-select').value = this.selectedChain;
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

        // Performance profiling with Stats.js
        if (typeof Stats !== 'undefined') {
            this.stats = new Stats();
            this.stats.showPanel(0); // 0: fps, 1: ms, 2: mb, 3+: custom
            this.stats.dom.style.position = 'absolute';
            this.stats.dom.style.top = '10px';
            this.stats.dom.style.right = '10px';
            document.body.appendChild(this.stats.dom);
        }

        // Handle window resize
        window.addEventListener('resize', () => {
            this.camera.aspect = window.innerWidth / window.innerHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(window.innerWidth, window.innerHeight);
        });
    }

    setupControls() {
        // Set up OrbitControls for zoom and active control
        this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.05;
        this.controls.enableZoom = true;
        this.controls.enableRotate = false; // Disable auto-rotate for manual control
        this.controls.enablePan = true;
        this.controls.minDistance = 100;
        this.controls.maxDistance = 1000;

        // Custom mouse following for passive 3D exploration
        this.mouseX = 0;
        this.mouseY = 0;
        this.targetX = 0;
        this.targetY = 0;

        // Mouse event listeners for passive following
        document.addEventListener('mousemove', (event) => {
            this.mouseX = (event.clientX - window.innerWidth / 2) * 0.002;
            this.mouseY = (event.clientY - window.innerHeight / 2) * 0.002;
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
                case 'q':
                    this.toggleQuantum();
                    break;
                case 'r':
                    this.runMergeAnimation();
                    break;
                case 'd':
                    this.runDemo();
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

        // Reset mouse following targets
        this.mouseX = 0;
        this.mouseY = 0;
        this.targetX = 0;
        this.targetY = 0;

        // Reset OrbitControls
        this.controls.reset();

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

        // Merge animation button
        document.getElementById('merge-btn').addEventListener('click', () => {
            this.runMergeAnimation();
        });

        // Export button
        document.getElementById('export-btn').addEventListener('click', () => {
            this.exportPNG();
        });

        // Quantum view toggle button
        document.getElementById('quantum-btn').addEventListener('click', () => {
            this.toggleQuantum();
        });

    }

    createInitialVisualization() {
        this.clearScene();
        if (this.quantumView) {
            this.createQuantumVisualization();
        } else {
            this.createTreeVisualization();
        }
        this.updateMetricsDisplay();
    }

    clearScene() {
        // Remove all nodes and connections
        this.nodes.forEach(node => {
            // Clean up chain-specific objects
            if (node.userData && node.userData.trailParticles) {
                node.userData.trailParticles.forEach(particle => {
                    this.scene.remove(particle);
                });
            }
            this.scene.remove(node);
        });
        this.connections.forEach(conn => {
            this.scene.remove(conn);
        });
        this.particles.forEach(particle => this.scene.remove(particle));

        this.nodes = [];
        this.connections = [];
        this.particles = [];
        this.nodePositions = []; // Clear node positions too
    }

    createTreeVisualization() {
        const maxNodes = Math.min(this.numSignatures, 1000); // Limit for performance

        // Store node positions for connection creation
        this.nodePositions = [];

        // Force-directed positioning for better hive_echo layout
        this.calculateNodePositions(maxNodes);

        // Use InstancedMesh for better performance with large node counts
        if (maxNodes > 200) {
            this.createInstancedNodes(maxNodes);
        } else {
            // Create individual nodes for smaller counts
            for (let i = 0; i < maxNodes; i++) {
                const pos = this.nodePositions[i];
                this.createNode(pos.x, pos.y, pos.z, i);
            }
        }

        // Create connections based on strategy
        this.createConnections();
    }

    calculateNodePositions(numNodes) {
        // Force-directed positioning with repulsion for hive_echo layout
        const positions = [];
        const repulsionStrength = 50;
        const iterations = 50;

        // Initialize positions randomly in a sphere
        for (let i = 0; i < numNodes; i++) {
            const theta = Math.random() * Math.PI * 2;
            const phi = Math.acos(2 * Math.random() - 1);
            const radius = 200 + Math.random() * 100;

            positions.push(new THREE.Vector3(
                radius * Math.sin(phi) * Math.cos(theta),
                radius * Math.sin(phi) * Math.sin(theta),
                radius * Math.cos(phi)
            ));
        }

        // Force-directed relaxation
        for (let iter = 0; iter < iterations; iter++) {
            const forces = positions.map(() => new THREE.Vector3());

            // Calculate repulsive forces
            for (let i = 0; i < numNodes; i++) {
                for (let j = i + 1; j < numNodes; j++) {
                    const diff = new THREE.Vector3().subVectors(positions[j], positions[i]);
                    const distance = diff.length();
                    if (distance > 0) {
                        const force = repulsionStrength / (distance * distance);
                        const forceVec = diff.normalize().multiplyScalar(force);
                        forces[i].sub(forceVec);
                        forces[j].add(forceVec);
                    }
                }
            }

            // Apply forces
            for (let i = 0; i < numNodes; i++) {
                positions[i].add(forces[i].multiplyScalar(0.1));
            }
        }

        this.nodePositions = positions;
    }

    createNode(x, y, z, index) {
        // Performance optimization: reduce geometry detail for large n
        const detailLevel = this.numSignatures > 5000 ? 4 : (this.numSignatures > 2000 ? 6 : 8);

        // Chain-specific geometry and material
        let geometry, material;

        switch(this.selectedChain) {
            case 'DOGECOIN':
                geometry = new THREE.TetrahedronGeometry(4, detailLevel); // 3D triangular pyramid - shows rotation clearly
                material = new THREE.MeshStandardMaterial({
                    color: 0xffaa44, // Bright orange-gold (Dogecoin theme)
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.6,
                    roughness: 0.4,
                    transparent: true,
                    opacity: 0.9
                });
                break;

            case 'LITECOIN':
                geometry = new THREE.BoxGeometry(4, 4, 4);
                material = new THREE.MeshStandardMaterial({
                    color: 0x00ff88, // Bright cyan-green (Litecoin theme - clearly visible)
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.7,
                    roughness: 0.3,
                    transparent: true,
                    opacity: 0.95
                });
                break;

            case 'SOLANA':
                geometry = new THREE.TorusGeometry(3, 1, 8, 16);
                material = new THREE.MeshStandardMaterial({
                    color: 0xffff00, // Yellow
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.1,
                    roughness: 0.8,
                    transparent: true,
                    opacity: 0.8
                });
                break;

            default:
                // Default to Dogecoin sphere
                geometry = new THREE.SphereGeometry(3, detailLevel, detailLevel);
                material = new THREE.MeshStandardMaterial({
                    color: 0x808080,
                    metalness: 0.3,
                    roughness: 0.7,
                    transparent: true,
                    opacity: 0.8
                });
        }

        const node = new THREE.Mesh(geometry, material);
        node.position.set(x, y, z);
        node.userData = {
            index,
            chain: this.selectedChain,
            originalColor: material.color.getHex(),
            rotationSpeed: Math.random() * 0.02 + 0.005, // For Litecoin rotation
            trailParticles: [] // For Solana particle trails
        };

        // Initialize particle trail for Solana
        if (this.selectedChain === 'SOLANA') {
            this.initializeSolanaTrail(node);
        }

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

        const nodeCount = this.nodePositions.length; // Use nodePositions length instead of nodes.length

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
            default:
                // Default to logarithmic for any unrecognized strategy
                this.createLogarithmicConnections(nodeCount);
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

    initializeSolanaTrail(node) {
        // Create initial particle trail for Solana nodes (high-TPS simulation)
        const particleCount = 5;
        for (let i = 0; i < particleCount; i++) {
            const particleGeometry = new THREE.SphereGeometry(0.5, 4, 4);
            const particleMaterial = new THREE.MeshBasicMaterial({
                color: 0xffff00,
                transparent: true,
                opacity: 0.6
            });

            const particle = new THREE.Mesh(particleGeometry, particleMaterial);
            particle.position.copy(node.position);
            particle.userData = {
                life: 30 + Math.random() * 20,
                velocity: {
                    x: (Math.random() - 0.5) * 2,
                    y: (Math.random() - 0.5) * 2,
                    z: (Math.random() - 0.5) * 2
                }
            };

            node.userData.trailParticles.push(particle);
            this.scene.add(particle);
        }
    }

    createConnection(fromIndex, toIndex) {
        if (fromIndex >= this.nodePositions.length || toIndex >= this.nodePositions.length) {
            console.warn(`Invalid connection indices: ${fromIndex} -> ${toIndex}, positions: ${this.nodePositions.length}`);
            return;
        }

        const fromPos = this.nodePositions[fromIndex];
        const toPos = this.nodePositions[toIndex];


        const geometry = new THREE.BufferGeometry().setFromPoints([
            fromPos,
            toPos
        ]);

        const material = new THREE.LineBasicMaterial({
            color: this.colors.connection, // 0x0080ff - blue
            transparent: true,
            opacity: 0.8, // Increased opacity for visibility
            linewidth: 2 // Note: linewidth doesn't work on most platforms
        });

        const line = new THREE.Line(geometry, material);
        this.scene.add(line);
        this.connections.push(line);
    }

    createTrail(fromIndex, toIndex, alpha = 0.6) {
        // Create trail effect for merge paths using LineSegments with fade alpha
        if (fromIndex >= this.nodePositions.length || toIndex >= this.nodePositions.length) {
            return;
        }

        const fromPos = this.nodePositions[fromIndex];
        const toPos = this.nodePositions[toIndex];

        // Create multiple trail segments for fade effect
        const segments = 5;
        const positions = [];
        const alphas = [];

        for (let i = 0; i < segments; i++) {
            const t = i / (segments - 1);
            const interpolatedPos = new THREE.Vector3().lerpVectors(fromPos, toPos, t);

            // Add slight offset for trail effect
            const offset = new THREE.Vector3(
                (Math.random() - 0.5) * 2,
                (Math.random() - 0.5) * 2,
                (Math.random() - 0.5) * 2
            );
            interpolatedPos.add(offset);

            positions.push(interpolatedPos.x, interpolatedPos.y, interpolatedPos.z);
            alphas.push(alpha * (1 - t)); // Fade from start to end
        }

        const geometry = new THREE.BufferGeometry();
        geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
        geometry.setAttribute('alpha', new THREE.Float32BufferAttribute(alphas, 1));

        // Custom shader material for alpha blending
        const material = new THREE.ShaderMaterial({
            uniforms: {
                color: { value: new THREE.Color(this.colors.connection) }
            },
            vertexShader: `
                attribute float alpha;
                varying float vAlpha;
                void main() {
                    vAlpha = alpha;
                    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
                }
            `,
            fragmentShader: `
                uniform vec3 color;
                varying float vAlpha;
                void main() {
                    gl_FragColor = vec4(color, vAlpha);
                }
            `,
            transparent: true,
            blending: THREE.AdditiveBlending
        });

        const trail = new THREE.Line(geometry, material);
        this.scene.add(trail);
        this.connections.push(trail);

        // Auto-remove trail after animation
        setTimeout(() => {
            this.scene.remove(trail);
            const index = this.connections.indexOf(trail);
            if (index > -1) {
                this.connections.splice(index, 1);
            }
        }, 2000);
    }

    createInstancedNodes(count) {
        // Use InstancedMesh for better performance with large node counts
        let geometry, material;

        switch(this.selectedChain) {
            case 'DOGECOIN':
                geometry = new THREE.TetrahedronGeometry(4, detailLevel); // 3D triangular pyramid - shows rotation clearly
                material = new THREE.MeshStandardMaterial({
                    color: 0xffaa44, // Bright orange-gold (Dogecoin theme)
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.6,
                    roughness: 0.4,
                    transparent: true,
                    opacity: 0.9
                });
                break;
            case 'LITECOIN':
                geometry = new THREE.BoxGeometry(4, 4, 4);
                material = new THREE.MeshStandardMaterial({
                    color: 0x00ff88, // Bright cyan-green (Litecoin theme - clearly visible)
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.7,
                    roughness: 0.3,
                    transparent: true,
                    opacity: 0.95
                });
                break;
            case 'SOLANA':
                geometry = new THREE.TorusGeometry(3, 1, 8, 16);
                material = new THREE.MeshStandardMaterial({
                    color: 0xffff00, // Yellow
                    emissive: 0x0000ff, // Blue emissive glow as requested
                    emissiveIntensity: 0.5, // As requested
                    metalness: 0.1,
                    roughness: 0.8,
                    transparent: true,
                    opacity: 0.8
                });
                break;
            default:
                geometry = new THREE.SphereGeometry(3, detailLevel, detailLevel);
                material = new THREE.MeshStandardMaterial({
                    color: 0x808080,
                    metalness: 0.3,
                    roughness: 0.7,
                    transparent: true,
                    opacity: 0.8
                });
        }

        const instancedMesh = new THREE.InstancedMesh(geometry, material, count);
        const matrix = new THREE.Matrix4();
        const color = new THREE.Color();

        for (let i = 0; i < count; i++) {
            const pos = this.nodePositions[i];

            matrix.setPosition(pos.x, pos.y, pos.z);
            instancedMesh.setMatrixAt(i, matrix);

            // Store instance data for animations
            instancedMesh.userData.instances = instancedMesh.userData.instances || [];
            instancedMesh.userData.instances[i] = {
                position: pos.clone(),
                originalScale: 1,
                chain: this.selectedChain,
                rotationSpeed: Math.random() * 0.02 + 0.005
            };
        }

        this.scene.add(instancedMesh);
        this.nodes.push(instancedMesh);
    }

    createSimpleParticleBurst(position) {
        // Simple particle burst using THREE.PointsMaterial as requested
        const particleCount = 30;
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(particleCount * 3);

        // Create random positions around the burst point
        for (let i = 0; i < particleCount; i++) {
            const i3 = i * 3;
            positions[i3] = position.x + (Math.random() - 0.5) * 20;
            positions[i3 + 1] = position.y + (Math.random() - 0.5) * 20;
            positions[i3 + 2] = position.z + (Math.random() - 0.5) * 20;
        }

        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));

        // Use THREE.PointsMaterial as requested - made more visible for demo
        const material = new THREE.PointsMaterial({
            color: 0x00ff00, // Green color as requested
            size: 0.3, // Larger size for better visibility
            transparent: true,
            opacity: 0.9
        });

        const particleSystem = new THREE.Points(geometry, material);
        this.scene.add(particleSystem);
        this.particles.push(particleSystem);

        // Animate with GSAP fade/scale effects
        gsap.fromTo(particleSystem.material,
            { opacity: 0.8, size: 0.1 },
            {
                opacity: 0,
                size: 0.5,
                duration: 2,
                ease: "power2.out",
                onComplete: () => {
                    this.scene.remove(particleSystem);
                }
            }
        );
    }

    createBurstEffect(position) {
        // Create high-performance particle burst using THREE.Points with BufferGeometry
        const particleCount = 50;
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(particleCount * 3);
        const alphas = new Float32Array(particleCount);

        // Initialize particles
        for (let i = 0; i < particleCount; i++) {
            const i3 = i * 3;

            // Start all particles at burst position
            positions[i3] = position.x;
            positions[i3 + 1] = position.y;
            positions[i3 + 2] = position.z;

            // Full opacity initially
            alphas[i] = 1.0;
        }

        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
        geometry.setAttribute('alpha', new THREE.BufferAttribute(alphas, 1));

        // Chain-specific particle colors
        let particleColor = this.colors.connection; // Default blue
        switch(this.selectedChain) {
            case 'DOGECOIN':
                particleColor = 0xffaa44; // Match Dogecoin orange-gold
                break;
            case 'LITECOIN':
                particleColor = 0x00ff88; // Match Litecoin cyan-green
                break;
            case 'SOLANA':
                particleColor = 0xffff00; // Match Solana yellow
                break;
        }

        // Custom shader material for alpha blending
        const material = new THREE.ShaderMaterial({
            uniforms: {
                color: { value: new THREE.Color(particleColor) },
                size: { value: 3.0 }
            },
            vertexShader: `
                attribute float alpha;
                varying float vAlpha;
                uniform float size;

                void main() {
                    vAlpha = alpha;
                    vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
                    gl_PointSize = size * (300.0 / -mvPosition.z);
                    gl_Position = projectionMatrix * mvPosition;
                }
            `,
            fragmentShader: `
                uniform vec3 color;
                varying float vAlpha;

                void main() {
                    float r = length(gl_PointCoord - vec2(0.5));
                    if (r > 0.5) discard;

                    gl_FragColor = vec4(color, vAlpha * (1.0 - r * 2.0));
                }
            `,
            transparent: true,
            depthWrite: false,
            blending: THREE.AdditiveBlending
        });

        const particleSystem = new THREE.Points(geometry, material);

        // Store particle velocities for animation
        particleSystem.userData.velocities = [];
        for (let i = 0; i < particleCount; i++) {
            const angle = Math.random() * Math.PI * 2;
            const speed = Math.random() * 8 + 3;
            particleSystem.userData.velocities.push({
                x: Math.cos(angle) * speed,
                y: Math.sin(angle) * speed,
                z: (Math.random() - 0.5) * speed * 2
            });
        }

        particleSystem.userData.life = 90; // 90 frames lifetime
        particleSystem.userData.initialLife = 90;

        this.scene.add(particleSystem);
        this.particles.push(particleSystem);
    }

    updateVisualization() {
        this.clearScene();
        if (this.quantumView) {
            this.createQuantumVisualization();
        } else {
            this.createTreeVisualization();
        }
        this.updateMetricsDisplay();
    }

    toggleQuantum() {
        this.quantumView = !this.quantumView;
        const button = document.getElementById('quantum-btn');

        if (this.quantumView) {
            button.innerHTML = '🔬 PAT<br>View';
            button.style.background = '#9c27b0';
            this.updateStatus('⚛️ Quantum View: Simulating Grover\'s Algorithm');
        } else {
            button.innerHTML = '⚛️ Quantum<br>View';
            button.style.background = '#2196f3';
            this.updateStatus('🔬 PAT View: PAW-Armored Post-Quantum Aggregation');
        }

        this.updateVisualization();
    }

    createQuantumVisualization() {
        const numQubits = Math.min(this.numSignatures, 100); // Limit for performance
        const radius = 200;
        const height = 150;

        // Store qubit positions for connections
        this.nodePositions = [];

        console.log(`Creating quantum visualization with ${numQubits} qubits`);

        for (let i = 0; i < numQubits; i++) {
            const angle = (i / numQubits) * Math.PI * 2;
            const y = (Math.sin(angle) * height) + (Math.random() - 0.5) * 30;
            const x = Math.cos(angle) * radius + (Math.random() - 0.5) * 50;
            const z = Math.sin(angle * 2) * radius * 0.3 + (Math.random() - 0.5) * 50;

            this.nodePositions.push(new THREE.Vector3(x, y, z));
            this.createQuantumBit(x, y, z, i);
        }

        // Create quantum entanglement connections
        this.createQuantumConnections(numQubits);
    }

    createQuantumBit(x, y, z, index) {
        // Create qubit as small sphere with lattice shader for quantum visualization
        const geometry = new THREE.SphereGeometry(0.15, 16, 16);

        // Lattice shader material for quantum visualization
        const probRatio = Math.log10(8.64e-78); // Very negative number representing Grover probability
        const normalizedProb = Math.max(0, Math.min(1, (probRatio + 200) / 100)); // Scale to 0-1

        const material = new THREE.ShaderMaterial({
            uniforms: {
                time: { value: 0 },
                probability: { value: normalizedProb },
                threatLevel: { value: this.showAttacks ? 1.0 : 0.0 }
            },
            vertexShader: `
                varying vec3 vPosition;
                varying vec3 vNormal;
                varying vec2 vUv;

                void main() {
                    vPosition = position;
                    vNormal = normal;
                    vUv = uv;
                    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
                }
            `,
            fragmentShader: `
                uniform float time;
                uniform float probability;
                uniform float threatLevel;
                varying vec3 vPosition;
                varying vec3 vNormal;
                varying vec2 vUv;

                void main() {
                    // Lattice pattern for quantum crystal structure
                    vec3 latticeColor = vec3(0.0, 0.5, 1.0); // Blue lattice base
                    vec3 threatColor = vec3(1.0, 0.0, 0.0); // Red for threats

                    // Create lattice grid pattern
                    vec3 pos = vPosition * 20.0;
                    float grid = sin(pos.x) * sin(pos.y) * sin(pos.z);
                    grid = smoothstep(0.0, 0.1, abs(grid));

                    // Probability-based color mixing (red=unsafe, blue=safe)
                    vec3 probColor = mix(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), probability);

                    // Quantum interference pattern
                    float interference = sin(time * 2.0 + length(vPosition) * 10.0) * 0.5 + 0.5;

                    // Combine lattice, probability, and interference
                    vec3 finalColor = mix(probColor, latticeColor, grid * 0.3);
                    finalColor = mix(finalColor, threatColor, threatLevel * interference);

                    // Add glow effect
                    float fresnel = 1.0 - dot(vNormal, vec3(0.0, 0.0, 1.0));
                    finalColor += vec3(0.2, 0.4, 0.8) * fresnel * 0.5;

                    gl_FragColor = vec4(finalColor, 0.8);
                }
            `,
            transparent: true,
            side: THREE.DoubleSide
        });

        const qubit = new THREE.Mesh(geometry, material);
        qubit.position.set(x, y, z);

        // Add quantum state data
        qubit.userData = {
            index,
            quantumState: Math.random() > 0.5 ? '|0⟩' : '|1⟩', // Random initial state
            probability: 8.64e-78,
            phase: Math.random() * Math.PI * 2,
            groverStep: 0,
            isThreatened: Math.random() < 0.1 // 10% chance of AI exploit
        };

        // Add red threat indicator for AI exploits
        if (qubit.userData.isThreatened) {
            const threatGeometry = new THREE.SphereGeometry(0.15, 8, 8);
            const threatMaterial = new THREE.MeshStandardMaterial({
                color: 0xff0000, // Red for threat
                emissive: 0x440000,
                emissiveIntensity: 0.8,
                transparent: true,
                opacity: 0.6
            });
            const threatSphere = new THREE.Mesh(threatGeometry, threatMaterial);
            threatSphere.position.copy(qubit.position);
            threatSphere.position.y += 0.2; // Offset above qubit
            qubit.add(threatSphere);
        }

        this.scene.add(qubit);
        this.nodes.push(qubit);
    }

    createQuantumConnections(numQubits) {
        this.connections.forEach(conn => this.scene.remove(conn));
        this.connections = [];

        // Create quantum entanglement connections (fewer, more meaningful)
        for (let i = 0; i < numQubits; i += 3) { // Every 3rd qubit
            if (i + 2 < numQubits) {
                // Connect in triangle patterns representing quantum entanglement
                this.createQuantumConnection(i, i + 1);
                this.createQuantumConnection(i + 1, i + 2);
                this.createQuantumConnection(i, i + 2);
            }
        }
    }

    createQuantumConnection(fromIndex, toIndex) {
        if (fromIndex >= this.nodePositions.length || toIndex >= this.nodePositions.length) return;

        const fromPos = this.nodePositions[fromIndex];
        const toPos = this.nodePositions[toIndex];

        const geometry = new THREE.BufferGeometry().setFromPoints([fromPos, toPos]);

        // Quantum entanglement lines - cyan color
        const material = new THREE.LineBasicMaterial({
            color: 0x00ffff, // Cyan for quantum entanglement
            transparent: true,
            opacity: 0.4
        });

        const line = new THREE.Line(geometry, material);
        this.scene.add(line);
        this.connections.push(line);
    }

    animate() {
        requestAnimationFrame(() => this.animate());

        this.animationFrame++;

        // Performance profiling with Stats.js
        if (this.stats) {
            this.stats.update();
        }

        // Update OrbitControls for zoom and pan
        this.controls.update();

        // Smooth mouse following for passive 3D exploration
        this.targetX += (this.mouseX - this.targetX) * 0.02;
        this.targetY += (-this.mouseY - this.targetY) * 0.02;

        this.camera.position.x += (this.targetX * 200 - this.camera.position.x) * 0.05;
        this.camera.position.y += (this.targetY * 200 - this.camera.position.y) * 0.05;

        this.camera.lookAt(this.scene.position);

        // Update node animations (chain-specific effects or quantum effects)

        this.nodes.forEach((node, index) => {
            if (this.quantumView) {
                // Handle quantum qubit animations
                this.animateQuantumBit(node);
            } else if (node.isInstancedMesh) {
                // Handle InstancedMesh animations
                this.animateInstancedMesh(node);
            } else {
                // Handle individual mesh animations
                const chain = node.userData.chain;

                switch(chain) {
                    case 'DOGECOIN':
                        // Rotation + Pulsing (mining activity simulation - both Dogecoin and Litecoin mine)
                        if (node.userData.rotationSpeed) {
                            node.rotation.x += node.userData.rotationSpeed;
                            node.rotation.y += node.userData.rotationSpeed * 0.8;
                            node.rotation.z += node.userData.rotationSpeed * 0.4;
                        }
                        // Add pulsing like Solana
                        const dogePulse = Math.sin(this.animationFrame * 0.02 + index * 0.1) * 0.15 + 0.9;
                        node.scale.setScalar(dogePulse);
                        break;

                    case 'LITECOIN':
                        // Rotation (mining activity simulation - both Dogecoin and Litecoin mine)
                        if (node.userData.rotationSpeed) {
                            node.rotation.x += node.userData.rotationSpeed;
                            node.rotation.y += node.userData.rotationSpeed * 0.7;
                            node.rotation.z += node.userData.rotationSpeed * 0.3;
                        }
                        break;

                    case 'SOLANA':
                        // Subtle pulsing with particle trail updates
                        const solanaPulse = Math.sin(this.animationFrame * 0.03 + index * 0.1) * 0.1 + 0.95;
                        node.scale.setScalar(solanaPulse);

                        // Update particle trails
                        if (node.userData.trailParticles) {
                            node.userData.trailParticles.forEach(particle => {
                                particle.position.x += particle.userData.velocity.x;
                                particle.position.y += particle.userData.velocity.y;
                                particle.position.z += particle.userData.velocity.z;

                                particle.userData.life--;

                                // Fade out particles
                                particle.material.opacity = particle.userData.life / 50;

                                // Respawn particles near node
                                if (particle.userData.life <= 0) {
                                    particle.position.copy(node.position);
                                    particle.position.x += (Math.random() - 0.5) * 10;
                                    particle.position.y += (Math.random() - 0.5) * 10;
                                    particle.position.z += (Math.random() - 0.5) * 10;
                                    particle.userData.life = 30 + Math.random() * 20;
                                    particle.material.opacity = 0.6;
                                }
                            });
                        }
                        break;

                    default:
                        // Default pulsing for any other chains
                        const defaultPulse = Math.sin(this.animationFrame * 0.02 + index * 0.1) * 0.1 + 0.9;
                        node.scale.setScalar(defaultPulse);
                }
            }
        });

        // Update connection animations
        this.connections.forEach((connection, index) => {
            const opacity = Math.sin(this.animationFrame * 0.03 + index * 0.05) * 0.3 + 0.7;
            connection.material.opacity = opacity;
        });

        // Update particles (THREE.Points with alpha fade and individual mesh particles)
        this.particles = this.particles.filter(particle => {
            if (particle.isPoints) {
                // Handle THREE.Points particle systems
                particle.userData.life--;

                const positions = particle.geometry.attributes.position.array;
                const alphas = particle.geometry.attributes.alpha.array;
                const velocities = particle.userData.velocities;

                // Update each particle
                for (let i = 0; i < velocities.length; i++) {
                    const i3 = i * 3;

                    // Update position
                    positions[i3] += velocities[i].x;
                    positions[i3 + 1] += velocities[i].y;
                    positions[i3 + 2] += velocities[i].z;

                    // Apply gravity/damping
                    velocities[i].y -= 0.1; // gravity
                    velocities[i].x *= 0.98; // air resistance
                    velocities[i].y *= 0.98;
                    velocities[i].z *= 0.98;

                    // Update alpha for fade out
                    const lifeRatio = particle.userData.life / particle.userData.initialLife;
                    alphas[i] = Math.max(0, lifeRatio - (i / velocities.length) * 0.3);
                }

                // Mark attributes as needing update
                particle.geometry.attributes.position.needsUpdate = true;
                particle.geometry.attributes.alpha.needsUpdate = true;

                if (particle.userData.life <= 0) {
                    this.scene.remove(particle);
                    return false;
                }
                return true;
            } else {
                // Handle individual mesh particles (explosions, celebrations)
                particle.position.x += particle.userData.velocity.x;
                particle.position.y += particle.userData.velocity.y;
                particle.position.z += particle.userData.velocity.z;

                // Apply gravity
                particle.userData.velocity.y += particle.userData.gravity || -0.1;

                // Apply air resistance
                particle.userData.velocity.x *= 0.99;
                particle.userData.velocity.y *= 0.99;
                particle.userData.velocity.z *= 0.99;

                particle.userData.life--;

                // Fade out
                if (particle.material) {
                    particle.material.opacity = particle.userData.life / 60;
                }

                if (particle.userData.life <= 0) {
                    this.scene.remove(particle);
                    return false;
                }
                return true;
            }
        });

        // Create occasional bursts during demo
        if (this.demoMode && this.animationFrame % 120 === 0) {
            const randomNode = this.nodes[Math.floor(Math.random() * this.nodes.length)];
            if (randomNode) {
                if (randomNode.position) {
                    this.createSimpleParticleBurst(randomNode.position);
                } else if (randomNode.userData && randomNode.userData.instances && randomNode.userData.instances.length > 0) {
                    // For InstancedMesh, use first instance position
                    this.createSimpleParticleBurst(randomNode.userData.instances[0].position);
                }
            }
        }

        this.renderer.render(this.scene, this.camera);
    }

    animateInstancedMesh(instancedMesh) {
        // Animate InstancedMesh instances
        const instances = instancedMesh.userData.instances;
        const matrix = new THREE.Matrix4();

        for (let i = 0; i < instances.length; i++) {
            const instance = instances[i];
            const chain = instance.chain;

            // Create transformation matrix for this instance
            switch(chain) {
                case 'DOGECOIN':
                    // Rotation + Pulsing (mining activity - both Dogecoin and Litecoin mine)
                    const dogePulse = Math.sin(this.animationFrame * 0.02 + i * 0.1) * 0.15 + 0.9;
                    matrix.makeRotationFromEuler(new THREE.Euler(
                        this.animationFrame * instance.rotationSpeed,
                        this.animationFrame * instance.rotationSpeed * 0.8,
                        this.animationFrame * instance.rotationSpeed * 0.4
                    ));
                    matrix.scale(new THREE.Vector3(dogePulse, dogePulse, dogePulse));
                    matrix.setPosition(instance.position);
                    break;

                case 'LITECOIN':
                    // Rotation (mining activity - both Dogecoin and Litecoin mine)
                    matrix.makeRotationFromEuler(new THREE.Euler(
                        this.animationFrame * instance.rotationSpeed,
                        this.animationFrame * instance.rotationSpeed * 0.7,
                        this.animationFrame * instance.rotationSpeed * 0.3
                    ));
                    matrix.setPosition(instance.position);
                    break;

                case 'SOLANA':
                    const solanaPulse = Math.sin(this.animationFrame * 0.03 + i * 0.1) * 0.1 + 0.95;
                    matrix.makeScale(solanaPulse, solanaPulse, solanaPulse);
                    matrix.setPosition(instance.position);
                    break;

                default:
                    const defaultPulse = Math.sin(this.animationFrame * 0.02 + i * 0.1) * 0.1 + 0.9;
                    matrix.makeScale(defaultPulse, defaultPulse, defaultPulse);
                    matrix.setPosition(instance.position);
            }

            instancedMesh.setMatrixAt(i, matrix);
        }

        instancedMesh.instanceMatrix.needsUpdate = true;
    }

    animateQuantumBit(qubit) {
        const userData = qubit.userData;

        // Update lattice shader uniforms
        if (qubit.material.uniforms) {
            qubit.material.uniforms.time.value = this.animationFrame * 0.01;
            qubit.material.uniforms.threatLevel.value = this.showAttacks ? 1.0 : 0.0;
        }

        // Grover's algorithm simulation: periodic rotations and collapses
        userData.phase += 0.02; // Continuous phase evolution

        // Simulate Grover iteration (every ~2 seconds)
        if (this.animationFrame % 120 === 0) {
            userData.groverStep++;

            // Grover's algorithm: amplitude amplification
            if (userData.groverStep % 3 === 0) {
                // Oracle query - flip phase for marked state
                if (userData.quantumState === '|1⟩') {
                    userData.phase += Math.PI; // Phase flip
                }
            } else if (userData.groverStep % 3 === 1) {
                // Diffusion operator - amplitude amplification
                qubit.scale.setScalar(1.5); // Visual amplification
                setTimeout(() => qubit.scale.setScalar(1.0), 200);
            }
        }

        // Continuous quantum rotation (superposition representation)
        qubit.rotation.x = userData.phase * 0.5;
        qubit.rotation.y = userData.phase * 0.3;
        qubit.rotation.z = userData.phase * 0.7;

        // Threat animation for AI exploits
        if (userData.isThreatened) {
            // Find the threat indicator (red sphere)
            const threatSphere = qubit.children.find(child =>
                child.material && child.material.color &&
                child.material.color.getHex() === 0xff0000
            );

            if (threatSphere) {
                // Pulsing threat indicator
                const threatPulse = Math.sin(this.animationFrame * 0.1) * 0.3 + 0.7;
                threatSphere.scale.setScalar(threatPulse);

                // Simulate attack attempts
                if (this.animationFrame % 300 === 0) { // Every 5 seconds
                    // Attack failure - qubit collapses and recovers
                    const originalOpacity = qubit.material.opacity;
                    gsap.to(qubit.material, {
                        duration: 0.5,
                        opacity: 0.2,
                        yoyo: true,
                        repeat: 3,
                        ease: "power2.inOut"
                    });

                    // Threat sphere flashes red
                    gsap.to(threatSphere.material, {
                        duration: 0.3,
                        emissiveIntensity: 1.5,
                        yoyo: true,
                        repeat: 5,
                        ease: "power2.inOut"
                    });
                }
            }
        }

        // Subtle quantum fluctuation
        const fluctuation = Math.sin(this.animationFrame * 0.05 + userData.index) * 0.05;
        qubit.position.y += fluctuation * 0.01; // Tiny vertical movement
    }

    runMergeAnimation() {
        // PAW merge animation: Dramatic GSAP-powered visualization of quantum-armored signature bundling
        console.log('🎬 GSAP Merge Animation triggered - nodes:', this.nodes.length);

        if (this.nodes.length < 2) {
            console.log('❌ Not enough nodes for merge animation');
            return;
        }

        // Check GSAP availability
        if (!window.gsap) {
            console.error('GSAP load failed—check CDN');
            console.log('Falling back to simple animation');
            this.simpleMergeAnimation();
            return;
        }

        // Test GSAP functionality
        try {
            gsap.set({}, {}); // Simple test
            console.log('✅ GSAP available and functional, starting merge animation');
        } catch (e) {
            console.error('GSAP loaded but not functional:', e);
            this.simpleMergeAnimation();
            return;
        }
        this.updateStatus('🎬 Running Dramatic PAW Merge Animation...');

        // Select strategic pairs for merging demonstration
        const pairs = [];
        const numPairs = Math.min(8, Math.floor(this.nodes.length / 2));

        // Create more strategic pairs (closer nodes for better visual effect)
        for (let i = 0; i < numPairs; i++) {
            const baseIdx = i * 2;
            if (baseIdx + 1 < this.nodes.length) {
                pairs.push([baseIdx, baseIdx + 1]);
            }
        }

        // Store original camera position for dramatic effects
        const originalCameraPos = this.camera.position.clone();

        // Master timeline for the entire sequence
        const masterTl = gsap.timeline();
        console.log('🎬 Created GSAP timeline, starting camera zoom...');

        // Add camera zoom-in at the start
        masterTl.to(this.camera.position, {
            duration: 1.5,
            z: originalCameraPos.z * 0.6,
            ease: "power2.inOut",
            onUpdate: () => { this.renderer.render(this.scene, this.camera); }
        });

        // Animate each pair merging with dramatic effects
        pairs.forEach(([idx1, idx2], pairIndex) => {
            const node1 = this.nodes[idx1];
            const node2 = this.nodes[idx2];

            if (!node1 || !node2) return;

            // Calculate midpoint for merge destination
            const midpoint = new THREE.Vector3()
                .addVectors(node1.position, node2.position)
                .multiplyScalar(0.5);

            // Individual timeline for this pair
            const pairTl = gsap.timeline({ delay: pairIndex * 0.8 });

            // Phase 1: Dramatic approach with camera focus
            pairTl.to(this.camera.position, {
                duration: 0.5,
                x: midpoint.x * 0.8,
                y: midpoint.y * 0.8,
                z: midpoint.z + 50,
                ease: "power2.inOut",
                onUpdate: () => { this.renderer.render(this.scene, this.camera); }
            }, 0)

            // Nodes charge toward each other with dramatic scaling
            .to([node1.position, node2.position], {
                duration: 1.2,
                x: midpoint.x,
                y: midpoint.y,
                z: midpoint.z,
                ease: "back.out(2.0)",
                onUpdate: () => { this.renderer.render(this.scene, this.camera); }
            }, 0)

            .to([node1.scale, node2.scale], {
                duration: 1.2,
                x: 2.5,
                y: 2.5,
                z: 2.5,
                ease: "elastic.out(1.2, 0.3)",
                onUpdate: () => { this.renderer.render(this.scene, this.camera); },
                onComplete: () => {
                    // Screen shake effect
                    const shakeTl = gsap.timeline();
                    shakeTl.to(this.camera.position, {
                        duration: 0.1,
                        x: "+=5",
                        yoyo: true,
                        repeat: 3,
                        ease: "power2.inOut",
                        onUpdate: () => { this.renderer.render(this.scene, this.camera); }
                    })
                    .to(this.camera.position, {
                        duration: 0.1,
                        y: "+=5",
                        yoyo: true,
                        repeat: 3,
                        ease: "power2.inOut",
                        onUpdate: () => { this.renderer.render(this.scene, this.camera); }
                    }, 0);

                    // Multiple burst effects with simple particles
                    this.createSimpleParticleBurst(midpoint);
                    setTimeout(() => this.createSimpleParticleBurst(midpoint), 200);
                    setTimeout(() => this.createSimpleParticleBurst(midpoint), 400);
                }
            }, 0)

            // Phase 2: Energy transfer and merge
            .to([node1.material, node2.material], {
                duration: 0.8,
                opacity: 0.3,
                ease: "power2.in",
                onUpdate: () => { this.renderer.render(this.scene, this.camera); },
                onStart: () => {
                    // Create energy arcs between nodes
                    this.createEnergyArc(node1.position, node2.position);
                }
            }, 1.0)

            .to([node1.scale, node2.scale], {
                duration: 0.8,
                x: 0.1,
                y: 0.1,
                z: 0.1,
                ease: "power4.in",
                onUpdate: () => { this.renderer.render(this.scene, this.camera); },
                onComplete: () => {
                    // Remove merged nodes with explosion effect
                    this.createExplosionEffect(midpoint);
                    this.scene.remove(node1);
                    this.scene.remove(node2);
                    const idx1_pos = this.nodes.indexOf(node1);
                    const idx2_pos = this.nodes.indexOf(node2);
                    if (idx1_pos > -1) this.nodes.splice(idx1_pos, 1);
                    if (idx2_pos > -1) this.nodes.splice(idx2_pos, 1);

                    // Create new merged node with birth effect
                    this.createNode(midpoint.x, midpoint.y, midpoint.z, this.nodes.length);
                    const newNode = this.nodes[this.nodes.length - 1];

                    // Dramatic birth animation
                    newNode.scale.setScalar(0);
                    newNode.material.opacity = 0;
                    gsap.to(newNode.scale, {
                        duration: 1.0,
                        x: 1.8,
                        y: 1.8,
                        z: 1.8,
                        ease: "elastic.out(1.0, 0.3)",
                        onUpdate: () => { this.renderer.render(this.scene, this.camera); }
                    });

                    gsap.to(newNode.material, {
                        duration: 1.0,
                        opacity: 1.0,
                        ease: "power2.out",
                        onUpdate: () => { this.renderer.render(this.scene, this.camera); },
                        onComplete: () => {
                            // Final celebration burst
                            this.createCelebrationBurst(midpoint);
                        }
                    });
                }
            }, 1.0);

            // Add this pair's timeline to master timeline
            masterTl.add(pairTl, pairIndex * 0.3);
        });

        // Final camera pullback and reset
        masterTl.to(this.camera.position, {
            duration: 2.0,
            x: originalCameraPos.x,
            y: originalCameraPos.y,
            z: originalCameraPos.z,
            ease: "power2.inOut",
            onUpdate: () => { this.renderer.render(this.scene, this.camera); }
        }, "+=1");

        // Reset view after animation
        setTimeout(() => {
            this.updateStatus('🎉 PAW Merge Animation Complete - Interactive Mode');
        }, pairs.length * 1200 + 4000);
    }

    simpleMergeAnimation() {
        // Fallback merge animation without GSAP
        console.log('🔄 Running simple merge animation');
        this.updateStatus('🔄 Running Simple PAW Merge Animation...');

        if (this.nodes.length < 2) return;

        // Simple animation: just create some burst effects
        let burstCount = 0;
        const burstInterval = setInterval(() => {
            if (burstCount < 5) {
                const randomNode = this.nodes[Math.floor(Math.random() * this.nodes.length)];
                if (randomNode && !randomNode.isInstancedMesh) {
                    this.createBurstEffect(randomNode.position);
                }
                burstCount++;
            } else {
                clearInterval(burstInterval);
                this.updateStatus('✨ PAW Simple Animation Complete');
            }
        }, 800);
    }

    createEnergyArc(startPos, endPos) {
        // Create lightning-like energy arc between merging nodes
        const arcPoints = 20;
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(arcPoints * 3);

        for (let i = 0; i < arcPoints; i++) {
            const t = i / (arcPoints - 1);
            const basePos = new THREE.Vector3().lerpVectors(startPos, endPos, t);

            // Add zigzag to simulate lightning
            const perpendicular = new THREE.Vector3()
                .crossVectors(new THREE.Vector3(0, 0, 1), endPos.clone().sub(startPos))
                .normalize();

            const zigzag = Math.sin(t * Math.PI * 8) * 5;
            basePos.add(perpendicular.multiplyScalar(zigzag));

            positions[i * 3] = basePos.x;
            positions[i * 3 + 1] = basePos.y;
            positions[i * 3 + 2] = basePos.z;
        }

        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));

        const material = new THREE.LineBasicMaterial({
            color: 0x00ffff,
            transparent: true,
            opacity: 0.8
        });

        const arc = new THREE.Line(geometry, material);
        this.scene.add(arc);

        // Remove arc after animation
        setTimeout(() => {
            this.scene.remove(arc);
        }, 1000);
    }

    createExplosionEffect(position) {
        // Create explosion particle effect
        for (let i = 0; i < 30; i++) {
            const particleGeometry = new THREE.SphereGeometry(0.8, 6, 6);
            const particleMaterial = new THREE.MeshBasicMaterial({
                color: Math.random() > 0.5 ? 0xffaa44 : 0x00ff88,
                transparent: true,
                opacity: 0.9
            });

            const particle = new THREE.Mesh(particleGeometry, particleMaterial);
            particle.position.copy(position);

            const velocity = {
                x: (Math.random() - 0.5) * 20,
                y: (Math.random() - 0.5) * 20,
                z: (Math.random() - 0.5) * 20
            };

            particle.userData = {
                velocity: velocity,
                life: 60,
                gravity: -0.3
            };

            this.scene.add(particle);
            this.particles.push(particle);
        }
    }

    createCelebrationBurst(position) {
        // Final celebration burst with multiple colors
        const colors = [0xffaa44, 0x00ff88, 0xffff00, 0xff0088, 0x88ff00];
        for (let i = 0; i < 50; i++) {
            const particleGeometry = new THREE.SphereGeometry(0.3, 4, 4);
            const particleMaterial = new THREE.MeshBasicMaterial({
                color: colors[Math.floor(Math.random() * colors.length)],
                transparent: true,
                opacity: 0.9
            });

            const particle = new THREE.Mesh(particleGeometry, particleMaterial);
            particle.position.copy(position);

            const speed = 8 + Math.random() * 12;
            const angle = Math.random() * Math.PI * 2;
            const elevation = Math.random() * Math.PI * 0.5;

            particle.userData = {
                velocity: {
                    x: Math.cos(angle) * Math.cos(elevation) * speed,
                    y: Math.sin(elevation) * speed,
                    z: Math.sin(angle) * Math.cos(elevation) * speed
                },
                life: 120,
                gravity: -0.2
            };

            this.scene.add(particle);
            this.particles.push(particle);
        }
    }

    runDemo() {
        console.log('🚀 Starting demo mode');
        this.demoMode = true;
        this.numSignatures = 1000;
        document.getElementById('signature-slider').value = 1000;
        document.getElementById('signature-value').textContent = 1000;

        this.updateStatus('🚀 Running Enhanced PAW Demo Animation...');
        this.updateVisualization();

        // Create a sequence of more visible bursts
        let burstCount = 0;
        const burstInterval = setInterval(() => {
            if (burstCount < 15) { // More bursts
                const randomNode = this.nodes[Math.floor(Math.random() * this.nodes.length)];
                if (randomNode) {
                    let position;
                    if (randomNode.position) {
                        position = randomNode.position;
                    } else if (randomNode.userData && randomNode.userData.instances && randomNode.userData.instances.length > 0) {
                        // For InstancedMesh, use first instance position
                        position = randomNode.userData.instances[0].position;
                    }

                    if (position) {
                        // Use both particle systems for more visible effect
                        this.createSimpleParticleBurst(position);
                        setTimeout(() => this.createBurstEffect(position), 100);
                    }
                }
                burstCount++;
            } else {
                clearInterval(burstInterval);
                this.demoMode = false;
                this.updateStatus('PAW Demo Complete - Interactive Mode');
            }
        }, 300); // Faster bursts
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
                <div class="metric-item" style="margin-top: 15px; border-top: 1px solid #4a4a6a; padding-top: 10px; font-size: 12px; line-height: 1.4;">
                    <strong>📖 Academic Reference:</strong><br>
                    This 3D view depicts PAT merging (gray nodes to blue lines)—see paper for EU-CMA proofs (adv ≤ 2^{-128})
                </div>
            `;
        } else {
            metricsContent.innerHTML = '<div class="metric-item">Loading performance data...</div>';
        }
    }

    updateStatus(message) {
        document.getElementById('status-text').textContent = message;
        setTimeout(() => {
            document.getElementById('status-text').textContent = `PAT with PAW Simulator - ${this.numSignatures} signatures`;
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
