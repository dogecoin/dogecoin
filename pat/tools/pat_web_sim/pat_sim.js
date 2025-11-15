// PAT Visual Simulator - Browser Edition
// Interactive 3D visualization of Post-Quantum Aggregation Technique (PAT)
// featuring Post-Quantum Armor Wrapper (PAW) for quantum-resistant bundling

class PATWebSimulator {
    constructor() {
        this.fps = 0;
        this.lastTime = performance.now();
        this.frameCount = 0;
        this.prevFrameTime = performance.now();
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

        // Quantum visualization properties
        this.qubitCount = 12;          // fixed visible qubits
        this.groverIterations = 4;     // number of animated iterations
        this.blochSpheres = [];
        this.qubitLabels = [];
        this.amplitudes = [];          // complex amplitudes for each state (only track target for visuals)

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
            threatHigh: 0xff8080,  // Red tint for HIGH
            // Connection colors based on threat level
            connectionLow: 0x00ff00,    // Green connections for low threat
            connectionMedium: 0xffaa00, // Orange connections for medium threat
            connectionHigh: 0xff0000    // Red connections for high threat
        };

        this.init();
    }

    init() {
        try {
            console.log('PAT Simulator: Starting initialization...');
            
            // Check for required dependencies
            if (!window.THREE) {
                console.error('Three.js not found!');
                throw new Error('Three.js not loaded');
            }
            console.log('✓ Three.js loaded');

            this.parseURLParams();
            console.log('✓ URL params parsed');
            
            this.setupScene();
            console.log('✓ Scene setup complete, renderer size:', this.renderer.domElement.width, 'x', this.renderer.domElement.height);
            
            this.setupControls();
            console.log('✓ Controls setup complete');
            
            this.loadBenchmarkData();
            console.log('✓ Benchmark data loading...');
            
            this.createInitialVisualization();
            console.log('✓ Initial visualization created, nodes:', this.nodes.length);
            
            this.setupEventListeners();
            console.log('✓ Event listeners setup');
            
            this.animate();
            console.log('✓ Animation loop started');
            
            this.hideLoading();
            console.log('✓ PAT Simulator fully initialized!');
        } catch (error) {
            console.error('Initialization error:', error);
            console.error('Stack trace:', error.stack);
            document.getElementById('loading').innerHTML = '<div style="color: #ff0000;">Error: ' + error.message + '</div>';
        }
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

        // Performance profiling with Stats.js - will be added to metrics panel later
        if (typeof Stats !== 'undefined') {
            this.stats = new Stats();
            this.stats.showPanel(0); // 0: fps, 1: ms, 2: mb, 3+: custom
            // Don't append to body yet - we'll integrate it into the metrics panel
        }

        // Handle window resize
        window.addEventListener('resize', () => {
            this.camera.aspect = window.innerWidth / window.innerHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(window.innerWidth, window.innerHeight);
        });
    }

    setupControls() {
        // FIXED: Correct OrbitControls loading for three.js r128 + separate OrbitControls.js
        if (typeof OrbitControls !== 'undefined') {
            this.controls = new OrbitControls(this.camera, this.renderer.domElement);
            this.controls.enableDamping = true;
            this.controls.dampingFactor = 0.05;
            this.controls.enableZoom = true;
            this.controls.enableRotate = false;
            this.controls.enablePan = true;
            this.controls.minDistance = 100;
            this.controls.maxDistance = 1000;
            this.controls.autoRotate = false;
        } else {
            console.error('OrbitControls is missing! Check CDN order.');
            this.controls = { update: () => {}, reset: () => {} };
        }

        // Passive mouse parallax (camera follow)
        this.mouseX = 0;
        this.mouseY = 0;
        this.targetX = 0;
        this.targetY = 0;

        document.addEventListener('mousemove', (event) => {
            this.mouseX = (event.clientX - window.innerWidth / 2) * 0.002;
            this.mouseY = (event.clientY - window.innerHeight / 2) * 0.002;

            if (this.quantumView && this.camera && this.scene) {
                this.checkQuantumHover(event);
            }
        });

        // Raycaster setup (required for tooltips)
        this.raycaster = new THREE.Raycaster();
        this.mouse = new THREE.Vector2();

        // Keyboard controls with full implementation
        document.addEventListener('keydown', (event) => {
            // Prevent default for space key
            if (event.key === ' ') {
                event.preventDefault();
            }

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
                case 'escape':
                    // Reset all animations
                    this.demoMode = false;
                    this.updateStatus('Animation stopped');
                    break;
                case 'm':
                    // Measurement collapse in quantum view
                    if (this.quantumView) {
                        this.performMeasurementCollapse();
                    }
                    break;
                case 'q':
                    // Toggle quantum view
                    this.toggleQuantum();
                    break;
            }
        });
    }
    
    checkQuantumHover(event) {
        // Convert mouse position to normalized device coordinates
        const rect = this.renderer.domElement.getBoundingClientRect();
        this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
        this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
        
        // Update raycaster
        this.raycaster.setFromCamera(this.mouse, this.camera);
        
        // Check intersections with quantum nodes
        const intersects = this.raycaster.intersectObjects(this.nodes);
        
        const tooltip = document.getElementById('tooltip');
        if (intersects.length > 0 && tooltip) {
            const node = intersects[0].object;
            const userData = node.userData;
            
            let tooltipText = '';
            if (userData.mode === 'grover' && userData.isMarkedState) {
                tooltipText = 'Marked state in Grover\'s algorithm - Oracle identifies this as the search target';
            } else if (userData.mode === 'shor' && userData.isFactor) {
                tooltipText = 'Factor state in Shor\'s algorithm - Quantum Fourier Transform extracts period';
            } else if (userData.mode === 'superposition') {
                tooltipText = 'Superposition state - Exists as |0⟩ and |1⟩ simultaneously until measured';
            } else if (userData.isEntangled) {
                tooltipText = 'Entangled qubit - Measurement instantly affects its paired qubit';
            }
            
            if (tooltipText) {
                tooltip.innerHTML = tooltipText;
                tooltip.style.display = 'block';
                tooltip.style.left = event.pageX + 10 + 'px';
                tooltip.style.top = event.pageY + 10 + 'px';
            }
        } else if (tooltip) {
            tooltip.style.display = 'none';
        }
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

        // Tooltip functionality
        this.setupTooltips();
    }

    setupTooltips() {
        const tooltip = document.getElementById('tooltip');
        if (!tooltip) return;

        // Add tooltips to various UI elements
        const tooltipElements = [
            { selector: '#signature-slider', text: 'Number of signatures to aggregate (n). In quantum view, represents number of qubits.' },
            { selector: '#threat-select', text: 'Quantum threat level: LOW (AES-128 safe), MEDIUM (hybrid needed), HIGH (full post-quantum required).' },
            { selector: '#strategy-select', text: 'Aggregation strategy determines connection patterns. Not used in quantum view.' },
            { selector: '#chain-select', text: 'Blockchain choice affects node shapes and animation styles in classical view.' },
            { selector: '#merge-btn', text: 'Run merge animation showing quantum-resistant signature aggregation.' },
            { selector: '#export-btn', text: 'Export the current 3D visualization as a PNG image.' },
            { selector: '#quantum-btn', text: 'Toggle quantum view: See Grover search, Shor factorization, and superposition states.' },
            { selector: '#demo-btn', text: 'Run enhanced demo showing 2^n quantum states with entanglement.' }
        ];
        
        // Add quantum-specific educational tooltips
        if (this.quantumView) {
            const quantumTooltips = [
                { text: 'Superposition: Qubits exist in multiple states simultaneously until measured', trigger: 'superposition' },
                { text: 'Entanglement: Quantum particles with correlated states, shown as mirrored velocities', trigger: 'entanglement' },
                { text: 'Grover\'s Algorithm: Quantum search with √N speedup, threatens symmetric crypto', trigger: 'grover' },
                { text: 'Shor\'s Algorithm: Quantum factorization threatening RSA/ECC cryptography', trigger: 'shor' },
                { text: 'Measurement Collapse: Press M to collapse quantum states to classical values', trigger: 'measurement' },
                { text: 'Quantum Advantage: 2^n states processed in parallel vs classical n states', trigger: 'advantage' }
            ];
            
            // Store quantum tooltips for hover detection
            this.quantumTooltips = quantumTooltips;
        }

        tooltipElements.forEach(({ selector, text }) => {
            const element = document.querySelector(selector);
            if (element) {
                element.addEventListener('mouseenter', (e) => {
                    tooltip.textContent = text;
                    tooltip.style.display = 'block';
                    
                    // Position tooltip near element
                    const rect = e.target.getBoundingClientRect();
                    tooltip.style.left = rect.left + 'px';
                    tooltip.style.top = (rect.bottom + 10) + 'px';
                    
                    // Keep tooltip within viewport
                    const tooltipRect = tooltip.getBoundingClientRect();
                    if (tooltipRect.right > window.innerWidth) {
                        tooltip.style.left = (window.innerWidth - tooltipRect.width - 10) + 'px';
                    }
                    if (tooltipRect.bottom > window.innerHeight) {
                        tooltip.style.top = (rect.top - tooltipRect.height - 10) + 'px';
                    }
                });

                element.addEventListener('mouseleave', () => {
                    tooltip.style.display = 'none';
                });
            }
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
        this.setupPerformanceStats();
    }

    setupPerformanceStats() {
        // Initialize performance tracking variables
        this.frameTime = 0;
        this.fps = 60;
        this.lastTime = performance.now();
    }

    clearScene() {
        // Kill any running Grover timeline to prevent orphaned animations
        if (this.groverTimeline) {
            this.groverTimeline.kill();
            this.groverTimeline = null;
        }
        
        // Remove all nodes and connections
        this.nodes.forEach(node => {
            // Clean up chain-specific objects
            if (node.userData && node.userData.trailParticles) {
                node.userData.trailParticles.forEach(particle => {
                    this.scene.remove(particle);
                });
            }
            // Clean up Bloch vectors
            if (node.userData && node.userData.blochVector) {
                this.scene.remove(node.userData.blochVector);
            }
            this.scene.remove(node);
        });
        this.connections.forEach(conn => {
            this.scene.remove(conn);
        });
        this.particles.forEach(particle => this.scene.remove(particle));
        
        // Remove Bloch spheres specifically
        this.blochSpheres.forEach(sphere => {
            if (sphere && sphere.parent) {
                this.scene.remove(sphere);
            }
        });
        
        // Remove all children from scene to catch sprites, text labels, etc.
        const toRemove = [];
        this.scene.traverse((child) => {
            if (child !== this.scene && child.type !== 'AmbientLight' && child.type !== 'DirectionalLight') {
                toRemove.push(child);
            }
        });
        toRemove.forEach(child => {
            if (child.parent) {
                child.parent.remove(child);
            }
        });
        
        // Remove threat indicators specifically
        this.scene.traverse((child) => {
            if (child.userData && child.userData.threatIndicator) {
                child.parent?.remove(child);
            }
        });
        
        // Remove blockchain indicators
        this.scene.traverse((child) => {
            if (child.userData && child.userData.blockchainIndicator) {
                child.parent?.remove(child);
            }
        });

        // Clear all arrays
        this.nodes = [];
        this.connections = [];
        this.particles = [];
        this.nodePositions = [];
        this.entangledPairs = [];
        this.blochSpheres = [];
        this.qubitLabels = [];
        this.amplitudes = [];
        
        // Reset camera to default position
        this.camera.position.set(0, 0, 500);
        this.camera.lookAt(0, 0, 0);
    }

    createTreeVisualization() {
        this.clearScene();

        // ────── ALWAYS USE SMALL CANONICAL TREE FOR CLARITY ──────
        const DISPLAY_N = Math.min(64, this.numSignatures);        // never >64 on screen
        const REAL_N    = this.numSignatures;                     // the real number we are claiming

        // Choose tree depth so that leaf count ≈ DISPLAY_N
        const depth = Math.ceil(Math.log2(DISPLAY_N));
        const leafCount = Math.pow(2, depth);                      // perfect binary tree

        this.nodePositions = [];
        this.nodes = [];
        this.connections = [];

        // Layout: perfect layered Merkle tree (Y = level)
        const verticalSpacing   = 90;
        const horizontalSpread  = 800 / Math.pow(2, depth-1);

        // Create nodes bottom-up (leaves first)
        const nodeIndexToPath = {};  // for later highlighting

        for (let level = depth; level >= 0; level--) {
            const nodesInLevel = Math.pow(2, depth - level);
            const y = -level * verticalSpacing + 200;

            for (let i = 0; i < nodesInLevel; i++) {
                const x = (i - (nodesInLevel - 1) / 2) * horizontalSpread * Math.pow(0.9, level);

                const pos = new THREE.Vector3(x, y, 0);
                this.nodePositions.push(pos);

                const isLeaf = (level === depth);
                
                // Blockchain-specific geometries
                let geometry;
                switch(this.selectedChain) {
                    case 'DOGECOIN':
                        // Dogecoin: Fun, approachable shapes - dodecahedron for leaves, icosahedron for internal
                        geometry = isLeaf ? 
                            new THREE.DodecahedronGeometry(6, 0) : 
                            new THREE.IcosahedronGeometry(7, 0);
                        break;
                    case 'LITECOIN':
                        // Litecoin: Solid, reliable shapes - box for leaves, octahedron for internal
                        geometry = isLeaf ? 
                            new THREE.BoxGeometry(8, 8, 8) : 
                            new THREE.OctahedronGeometry(7, 0);
                        break;
                    case 'SOLANA':
                        // Solana: Fast, modern shapes - cylinder for leaves, cone for internal
                        geometry = isLeaf ? 
                            new THREE.CylinderGeometry(4, 6, 8, 6) : 
                            new THREE.ConeGeometry(6, 10, 8);
                        break;
                    default:
                        geometry = isLeaf ? 
                            new THREE.TetrahedronGeometry(6, 0) : 
                            new THREE.OctahedronGeometry(7, 0);
                }

                // Combined blockchain and threat-level colors
                let nodeColor, emissiveColor;
                const baseColors = this.getBlockchainColors();
                
                // Modify base colors based on threat level
                switch(this.threatLevel) {
                    case 'LOW':
                        // Green tint
                        nodeColor = isLeaf ? baseColors.leafColor : baseColors.internalColor;
                        emissiveColor = baseColors.emissive;
                        break;
                    case 'MEDIUM':
                        // Orange tint
                        nodeColor = isLeaf ? 0xffaa44 : 0xff8822;
                        emissiveColor = isLeaf ? 0x442200 : 0x331100;
                        break;
                    case 'HIGH':
                        // Red tint
                        nodeColor = isLeaf ? 0xff4444 : 0xff2222;
                        emissiveColor = isLeaf ? 0x440000 : 0x220000;
                        break;
                    default:
                        nodeColor = isLeaf ? baseColors.leafColor : baseColors.internalColor;
                        emissiveColor = baseColors.emissive;
                }

                const material = new THREE.MeshStandardMaterial({
                    color: nodeColor,
                    emissive: emissiveColor,
                    emissiveIntensity: 0.6,
                    metalness: baseColors.metalness || 0.8,
                    roughness: baseColors.roughness || 0.2
                });

                const node = new THREE.Mesh(geometry, material);
                node.position.copy(pos);
                node.userData = {
                    level,
                    indexInLevel: i,
                    isLeaf,
                    pathString: isLeaf ? this.getPathString(depth, i) : null,
                    realSignatures: isLeaf ? Math.floor(REAL_N / leafCount) : 0
                };

                // Store reverse lookup
                nodeIndexToPath[this.nodes.length] = node.userData.pathString;

                this.scene.add(node);
                this.nodes.push(node);

                // Label leaves with number of real signatures they represent
                if (isLeaf && REAL_N > leafCount) {
                    this.addTextLabel(
                        node.position.clone().add(new THREE.Vector3(0, -15, 0)),
                        `${Math.floor(REAL_N/leafCount)} sigs`,
                        0x00ff88
                    );
                }
            }
        }

        // Create perfect tree connections
        this.createPerfectTreeConnections(depth);

        // Add root label showing final compressed size
        const rootPos = this.nodes[this.nodes.length - 1].position.clone().add(new THREE.Vector3(0, 30, 0));
        const compressedBytes = Math.max(64, Math.ceil(Math.log2(REAL_N)) * 8); // rough
        this.addTextLabel(rootPos, `Final PAW aggregate ≈ ${compressedBytes} bytes`, 0x00ffffff);

        // Add top banner with real scale
        this.addTextLabel(
            new THREE.Vector3(0, 350, 0),
            `Visualising canonical tree (n=${leafCount})\nRepresenting real ${REAL_N.toLocaleString()} signatures\nCompression = ${(REAL_N * 2420 / compressedBytes).toFixed(0)}×`,
            0xffffff,
            28
        );

        // Add threat level indicator
        this.addThreatLevelIndicator();
        
        // Add blockchain-specific visual indicators
        this.addBlockchainIndicators();

        // Start step-by-step merge animation automatically
        setTimeout(() => this.runStepByStepMerge(depth), 1500);
    }

    // Add visual indicators for threat level
    addThreatLevelIndicator() {
        let indicatorText, indicatorColor, glowIntensity;
        
        switch(this.threatLevel) {
            case 'LOW':
                indicatorText = '🛡️ QUANTUM SAFE\nECDSA signatures protected by PAW';
                indicatorColor = 0x00ff00;
                glowIntensity = 0.3;
                break;
            case 'MEDIUM':
                indicatorText = '⚠️ HYBRID PROTECTION\nECDSA + Dilithium3 required';
                indicatorColor = 0xffaa00;
                glowIntensity = 0.5;
                // Add pulsing warning effect
                this.addPulsingWarning();
                break;
            case 'HIGH':
                indicatorText = '🚨 FULL QUANTUM THREAT\nDilithium3-only signatures needed';
                indicatorColor = 0xff0000;
                glowIntensity = 0.7;
                // Add threat particles
                this.addThreatParticles();
                break;
        }
        
        // Add threat level label
        this.addTextLabel(
            new THREE.Vector3(-280, 250, 0),
            indicatorText,
            indicatorColor,
            20
        );
        
        // Add ambient glow based on threat level
        const glowLight = new THREE.PointLight(indicatorColor, glowIntensity, 500);
        glowLight.position.set(0, 0, 100);
        this.scene.add(glowLight);
        
        // Store for cleanup
        glowLight.userData = { threatIndicator: true };
    }
    
    addPulsingWarning() {
        // Create pulsing ring for medium threat
        const geometry = new THREE.RingGeometry(150, 160, 64);
        const material = new THREE.MeshBasicMaterial({
            color: 0xffaa00,
            transparent: true,
            opacity: 0.3,
            side: THREE.DoubleSide
        });
        const ring = new THREE.Mesh(geometry, material);
        ring.position.set(0, 0, -50);
        this.scene.add(ring);
        
        // Animate the ring
        gsap.to(ring.scale, {
            x: 1.2,
            y: 1.2,
            duration: 2,
            repeat: -1,
            yoyo: true,
            ease: "power2.inOut"
        });
        
        gsap.to(material, {
            opacity: 0.1,
            duration: 2,
            repeat: -1,
            yoyo: true,
            ease: "power2.inOut"
        });
        
        ring.userData = { threatIndicator: true };
    }
    
    addThreatParticles() {
        // Create threat particles for high threat level
        const particleCount = 50;
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(particleCount * 3);
        
        for (let i = 0; i < particleCount; i++) {
            const angle = (i / particleCount) * Math.PI * 2;
            const radius = 200 + Math.random() * 100;
            positions[i * 3] = Math.cos(angle) * radius;
            positions[i * 3 + 1] = Math.sin(angle) * radius;
            positions[i * 3 + 2] = Math.random() * 50 - 25;
        }
        
        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
        
        const material = new THREE.PointsMaterial({
            color: 0xff0000,
            size: 4,
            transparent: true,
            opacity: 0.6,
            blending: THREE.AdditiveBlending
        });
        
        const particles = new THREE.Points(geometry, material);
        this.scene.add(particles);
        
        // Animate threat particles
        gsap.to(particles.rotation, {
            z: Math.PI * 2,
            duration: 20,
            repeat: -1,
            ease: "none"
        });
        
        particles.userData = { threatIndicator: true };
    }

    // Add blockchain-specific visual indicators
    addBlockchainIndicators() {
        const colors = this.getBlockchainColors();
        
        switch(this.selectedChain) {
            case 'DOGECOIN':
                // Add Dogecoin mascot particles (playful animation)
                this.addDogeParticles();
                
                // Add fun label
                this.addTextLabel(
                    new THREE.Vector3(280, 250, 0),
                    '🐕 DOGECOIN\nScrypt Mining\nMuch Secure!',
                    colors.leafColor,
                    20
                );
                break;
                
            case 'LITECOIN':
                // Add silver/metallic effects
                this.addMetallicSheen();
                
                // Add technical label
                this.addTextLabel(
                    new THREE.Vector3(280, 250, 0),
                    '🥈 LITECOIN\nMWEB Privacy\nDigital Silver',
                    colors.leafColor,
                    20
                );
                break;
                
            case 'SOLANA':
                // Add speed lines effect
                this.addSpeedLines();
                
                // Add high-performance label
                this.addTextLabel(
                    new THREE.Vector3(280, 250, 0),
                    '⚡ SOLANA\nProof of History\n65,000 TPS',
                    colors.leafColor,
                    20
                );
                break;
        }
        
        // Add subtle ambient light in blockchain color
        const ambientLight = new THREE.PointLight(colors.emissive, 0.3, 400);
        ambientLight.position.set(0, 100, 50);
        ambientLight.userData = { blockchainIndicator: true };
        this.scene.add(ambientLight);
    }
    
    addDogeParticles() {
        // Create fun, bouncing particles for Dogecoin
        const particleCount = 30;
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(particleCount * 3);
        
        for (let i = 0; i < particleCount; i++) {
            positions[i * 3] = (Math.random() - 0.5) * 400;
            positions[i * 3 + 1] = Math.random() * 200 + 100;
            positions[i * 3 + 2] = (Math.random() - 0.5) * 100;
        }
        
        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
        
        const material = new THREE.PointsMaterial({
            color: 0xffcc66,
            size: 8,
            transparent: true,
            opacity: 0.8,
            blending: THREE.AdditiveBlending,
            map: this.createDogeTexture()
        });
        
        const particles = new THREE.Points(geometry, material);
        particles.userData = { blockchainIndicator: true, type: 'doge' };
        this.scene.add(particles);
        
        // Animate particles bouncing
        gsap.to(particles.position, {
            y: "+=20",
            duration: 2,
            repeat: -1,
            yoyo: true,
            ease: "power1.inOut"
        });
    }
    
    createDogeTexture() {
        const canvas = document.createElement('canvas');
        canvas.width = 64;
        canvas.height = 64;
        const ctx = canvas.getContext('2d');
        
        // Draw a simple circle (could be replaced with actual Doge image)
        ctx.fillStyle = '#ffcc66';
        ctx.beginPath();
        ctx.arc(32, 32, 30, 0, Math.PI * 2);
        ctx.fill();
        
        const texture = new THREE.CanvasTexture(canvas);
        return texture;
    }
    
    addMetallicSheen() {
        // Create a reflective plane for Litecoin
        const geometry = new THREE.PlaneGeometry(600, 600, 1, 1);
        const material = new THREE.MeshStandardMaterial({
            color: 0x0099cc,
            metalness: 1,
            roughness: 0,
            transparent: true,
            opacity: 0.1,
            side: THREE.DoubleSide
        });
        
        const plane = new THREE.Mesh(geometry, material);
        plane.rotation.x = Math.PI / 2;
        plane.position.y = -100;
        plane.userData = { blockchainIndicator: true };
        this.scene.add(plane);
        
        // Animate subtle rotation for shimmer effect
        gsap.to(plane.rotation, {
            z: Math.PI * 2,
            duration: 30,
            repeat: -1,
            ease: "none"
        });
    }
    
    addSpeedLines() {
        // Create speed lines for Solana
        const lineCount = 20;
        for (let i = 0; i < lineCount; i++) {
            const geometry = new THREE.BufferGeometry();
            const y = (Math.random() - 0.5) * 300;
            const z = (Math.random() - 0.5) * 100;
            
            const positions = new Float32Array([
                -400, y, z,
                400, y, z
            ]);
            
            geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
            
            const material = new THREE.LineBasicMaterial({
                color: 0x14F195,
                transparent: true,
                opacity: 0.3,
                linewidth: 1
            });
            
            const line = new THREE.Line(geometry, material);
            line.userData = { blockchainIndicator: true, type: 'speedLine' };
            this.scene.add(line);
            
            // Animate speed lines moving fast
            const duration = 1 + Math.random() * 2;
            gsap.fromTo(line.position, 
                { x: -600 },
                { 
                    x: 600,
                    duration: duration,
                    repeat: -1,
                    ease: "none",
                    delay: Math.random() * duration
                }
            );
            
            // Fade in and out
            gsap.to(line.material, {
                opacity: 0.6,
                duration: duration / 2,
                repeat: -1,
                yoyo: true,
                ease: "power2.inOut"
            });
        }
    }

    // Get blockchain-specific colors
    getBlockchainColors() {
        switch(this.selectedChain) {
            case 'DOGECOIN':
                return {
                    leafColor: 0xffaa44,      // Dogecoin orange-gold
                    internalColor: 0xff8822,  // Darker orange
                    emissive: 0x442200,       // Orange glow
                    connectionColor: 0xffaa44,
                    particleColor: 0xffcc66,
                    metalness: 0.7,
                    roughness: 0.3
                };
            case 'LITECOIN':
                return {
                    leafColor: 0x00ccff,      // Litecoin cyan-blue
                    internalColor: 0x0099cc,  // Darker cyan
                    emissive: 0x003344,       // Blue glow
                    connectionColor: 0x00ccff,
                    particleColor: 0x00ff88,
                    metalness: 0.9,           // More metallic
                    roughness: 0.1
                };
            case 'SOLANA':
                return {
                    leafColor: 0x14F195,      // Solana gradient green
                    internalColor: 0x9945FF,  // Solana purple
                    emissive: 0x220044,       // Purple glow
                    connectionColor: 0xDC1FFF, // Solana gradient purple
                    particleColor: 0x14F195,
                    metalness: 0.5,
                    roughness: 0.4
                };
            default:
                return {
                    leafColor: 0x00ff88,
                    internalColor: 0x0088ff,
                    emissive: 0x002244,
                    connectionColor: 0x0080ff,
                    particleColor: 0x00ffff,
                    metalness: 0.8,
                    roughness: 0.2
                };
        }
    }

    // Helper: build human-readable Merkle path (e.g. "L-R-L")
    getPathString(depth, leafIndex) {
        let path = '';
        let idx = leafIndex;
        for (let d = depth; d > 0; d--) {
            path = (idx % 2 === 0 ? 'L' : 'R') + path;
            idx = Math.floor(idx / 2);
        }
        return path;
    }

    runStepByStepMerge(depth) {
        let level = depth;
        const tl = gsap.timeline({repeat: 0});

        const mergeLevel = () => {
            if (level <= 0) {
                this.updateStatus('PAW aggregation complete – O(log n) achieved');
                return;
            }

            const nodesInLevel = Math.pow(2, depth - level + 1); // children
            const startIdx = this.nodes.length - Math.pow(2, depth - level + 2); // rough

            // Highlight all pairs at this level
            for (let i = 0; i < nodesInLevel; i += 2) {
                const child1 = this.nodes[startIdx + i];
                const child2 = this.nodes[startIdx + i + 1];
                if (child1 && child2) {
                    // More subtle scale animation
                    tl.to([child1.scale, child2.scale], {x:1.5,y:1.5,z:1.5, duration:0.4}, 0);
                    tl.to([child1.scale, child2.scale], {x:0.1, y:0.1, z:0.1, duration:0.6, ease:"power2.in"}, "+=0.2");
                    // Add glow effect
                    tl.to([child1.material, child2.material], {emissiveIntensity: 1.0, duration:0.4}, 0);
                    this.createSimpleParticleBurst(child1.position ? child1.position.clone() : new THREE.Vector3());
                }
            }

            // Camera slowly descends one level (but limit the descent)
            const newY = Math.max(this.camera.position.y - 60, -200);
            tl.to(this.camera.position, {
                y: newY,
                duration: 2.0,
                ease: "power2.inOut"
            }, "-=1.0");

            level--;
            // Use onComplete callback instead of .then()
            tl.call(mergeLevel);
        };

        mergeLevel();
    }

    createPerfectTreeConnections(depth) {
        this.connections.forEach(conn => this.scene.remove(conn));
        this.connections = [];

        // Create connections for perfect binary tree
        let nodeIndex = this.nodes.length - 1; // Start from root
        
        for (let level = 0; level < depth; level++) {
            const nodesInLevel = Math.pow(2, level);
            
            for (let i = 0; i < nodesInLevel; i++) {
                const parentIdx = nodeIndex;
                const leftChildIdx = parentIdx - Math.pow(2, depth - level) + i * 2;
                const rightChildIdx = leftChildIdx + 1;
                
                // Create connections to children
                if (leftChildIdx >= 0 && leftChildIdx < this.nodes.length) {
                    this.createConnection(this.nodes[parentIdx], this.nodes[leftChildIdx], 0x00ff88);
                }
                if (rightChildIdx >= 0 && rightChildIdx < this.nodes.length) {
                    this.createConnection(this.nodes[parentIdx], this.nodes[rightChildIdx], 0x00ff88);
                }
                
                nodeIndex--;
            }
        }
    }

    createConnection(node1, node2, color = null) {
        if (!node1 || !node2) return;
        
        // Use threat-level and blockchain-specific colors if no color provided
        if (!color) {
            switch(this.threatLevel) {
                case 'LOW':
                    // Use blockchain-specific colors for low threat
                    const blockchainColors = this.getBlockchainColors();
                    color = blockchainColors.connectionColor;
                    break;
                case 'MEDIUM':
                    color = 0xffaa00; // Orange connections
                    break;
                case 'HIGH':
                    color = 0xff0000; // Red connections
                    break;
                default:
                    color = 0x00ff88;
            }
        }
        
        const geometry = new THREE.BufferGeometry().setFromPoints([
            node1.position,
            node2.position
        ]);
        
        const material = new THREE.LineBasicMaterial({
            color: color,
            opacity: 0.6,
            transparent: true,
            linewidth: 2
        });
        
        const line = new THREE.Line(geometry, material);
        this.scene.add(line);
        this.connections.push(line);
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
                geometry = new THREE.TetrahedronGeometry(4, 0); // 3D triangular pyramid - shows rotation clearly
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
                geometry = new THREE.SphereGeometry(3, 8, 8);
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
        let baseColor;

        // Apply threat level coloring with more distinct visual differences
        switch(this.threatLevel) {
            case 'LOW':
                // Green-tinted nodes for low threat - quantum safe
                baseColor = 0x00ff88;
                break;
            case 'MEDIUM':
                // Yellow/orange nodes for medium threat - hybrid protection needed
                baseColor = 0xffaa44;
                break;
            case 'HIGH':
                // Red-tinted nodes for high threat - full quantum protection required
                baseColor = 0xff4444;
                break;
            default:
                baseColor = this.colors.node;
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
                this.createConnectionByIndex(i, i + 1);
                if (i + 2 < nodeCount) {
                    this.createConnectionByIndex(i, i + 2); // Additional connections for tree structure
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
                    this.createConnectionByIndex(i, groupStart + j);
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
                    this.createConnectionByIndex(left, right);
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
                this.createConnectionByIndex(i, j);
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

    createConnectionByIndex(fromIndex, toIndex) {
        if (fromIndex >= this.nodePositions.length || toIndex >= this.nodePositions.length) {
            return;
        }

        const fromPos = this.nodePositions[fromIndex];
        const toPos = this.nodePositions[toIndex];


        const geometry = new THREE.BufferGeometry().setFromPoints([
            fromPos,
            toPos
        ]);

        // Use threat-level-specific connection colors
        let connectionColor = this.colors.connection;
        switch(this.threatLevel) {
            case 'LOW':
                connectionColor = this.colors.connectionLow;
                break;
            case 'MEDIUM':
                connectionColor = this.colors.connectionMedium;
                break;
            case 'HIGH':
                connectionColor = this.colors.connectionHigh;
                break;
        }

        const material = new THREE.LineBasicMaterial({
            color: connectionColor,
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

        // Use threat-level-specific connection colors
        let connectionColor = this.colors.connection;
        switch(this.threatLevel) {
            case 'LOW':
                connectionColor = this.colors.connectionLow;
                break;
            case 'MEDIUM':
                connectionColor = this.colors.connectionMedium;
                break;
            case 'HIGH':
                connectionColor = this.colors.connectionHigh;
                break;
        }

        // Custom shader material for alpha blending
        const material = new THREE.ShaderMaterial({
            uniforms: {
                color: { value: new THREE.Color(connectionColor) }
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
        const detailLevel = this.numSignatures > 5000 ? 4 : (this.numSignatures > 2000 ? 6 : 8);
        let geometry, material;

        switch(this.selectedChain) {
            case 'DOGECOIN':
                geometry = new THREE.TetrahedronGeometry(4, 0); // 3D triangular pyramid - shows rotation clearly
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
                geometry = new THREE.SphereGeometry(3, 8, 8);
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
        const wasQuantumView = this.quantumView;
        this.quantumView = !this.quantumView;
        const button = document.getElementById('quantum-btn');

        // Fade out current scene
        this.nodes.forEach(node => {
            if (node.material) {
                gsap.to(node.material, {
                    opacity: 0,
                    duration: 0.5,
                    ease: "power2.in"
                });
            }
        });
        
        this.connections.forEach(conn => {
            if (conn.material) {
                gsap.to(conn.material, {
                    opacity: 0,
                    duration: 0.5,
                    ease: "power2.in"
                });
            }
        });
        
        this.particles.forEach(particle => {
            if (particle.material) {
                gsap.to(particle.material, {
                    opacity: 0,
                    duration: 0.5,
                    ease: "power2.in"
                });
            }
        });

        // After fade out, update visualization
        setTimeout(() => {
            if (this.quantumView) {
                button.innerHTML = '🔬 PAT<br>View';
                button.style.background = '#9c27b0';
                this.clearScene();
                this.createQuantumVisualization();
                this.updateStatus('Quantum View – Grover attack on PAW aggregate');
            } else {
                button.innerHTML = '⚛️ Quantum<br>View';
                button.style.background = '#2196f3';
                this.clearScene();
                this.createTreeVisualization();
                this.updateStatus('Classical PAT aggregation view');
            }
            
            // Fade in new scene
            setTimeout(() => {
                this.nodes.forEach(node => {
                    gsap.to(node.material, {
                        opacity: node.material.transparent ? 0.85 : 1.0,
                        duration: 0.5,
                        ease: "power2.out"
                    });
                });
                
                this.connections.forEach(conn => {
                    gsap.to(conn.material, {
                        opacity: conn.userData && conn.userData.type === 'oracle' ? 0.8 : 0.4,
                        duration: 0.5,
                        ease: "power2.out"
                    });
                });
                
                this.particles.forEach(particle => {
                    if (particle.material && particle.userData.type === 'quantumStates') {
                        gsap.to(particle.material, {
                            opacity: 0.6,
                            duration: 0.5,
                            ease: "power2.out"
                        });
                    }
                });
            }, 100);
        }, 500);
    }

    createQuantumVisualization() {
        this.clearScene();

        const radius = 220;
        const qubitCount = Math.min(16, this.qubitCount);

        // Initialize arrays
        this.blochSpheres = [];
        this.nodes = [];

        // Initial equal superposition amplitudes (visual only)
        this.amplitudes = new Array(1 << qubitCount).fill(1 / Math.sqrt(1 << qubitCount));

        // Target state = the one an attacker would need to find (forged aggregate)
        const targetState = Math.floor(Math.random() * (1 << qubitCount));  // random for demo

        // Create Bloch spheres in a perfect circle
        for (let i = 0; i < qubitCount; i++) {
            const angle = (i / qubitCount) * Math.PI * 2;
            const x = Math.cos(angle) * radius;
            const z = Math.sin(angle) * radius;

            // Bloch sphere (standard visualisation)
            const geometry = new THREE.SphereGeometry(18, 32, 32);
            const material = new THREE.MeshStandardMaterial({
                color: 0x0088ff,
                emissive: 0x003366,
                metalness: 0.9,
                roughness: 0.1,
                transparent: true,
                opacity: 0.9
            });
            const sphere = new THREE.Mesh(geometry, material);
            sphere.position.set(x, 0, z);
            sphere.userData = {
                type: 'blochSphere',
                index: i
            };
            this.scene.add(sphere);
            this.blochSpheres.push(sphere);
            // Don't add to nodes array - track separately
            
            // Qubit label
            this.addTextLabel(
                new THREE.Vector3(x, -40, z),
                `q${i}`,
                0x00ffffff,
                24
            );

            // Axis lines
            const axes = new THREE.AxesHelper(25);
            sphere.add(axes);
        }

        // Central target state label
        this.addTextLabel(
            new THREE.Vector3(0, 120, 0),
            `Target state |${targetState.toString(2).padStart(qubitCount,'0')}⟩\n(the forged aggregate)`,
            0xff0088,
            26
        );

        // Real attack cost banner (updates with slider n)
        const realStates = Math.pow(2, this.numSignatures);
        const groverQueries = Math.ceil(Math.PI / 4 * Math.sqrt(realStates));
        const bannerText = `Real attack on ${this.numSignatures.toLocaleString()} signatures\n= 2^${this.numSignatures} states\nGrover needs ≈ ${groverQueries.toExponential(2)} queries\nProbability per query ≈ ${(1/groverQueries).toExponential(2)}`;
        
        this.addTextLabel(
            new THREE.Vector3(0, 280, 0),
            bannerText,
            0xffdd00,
            28
        );

        // Start Grover animation after short delay
        setTimeout(() => this.runGroverAnimation(targetState, qubitCount), 2000);
    }

    runGroverAnimation(targetState, qubitCount) {
        const tl = gsap.timeline({repeat: 0});

        // Mark target with dramatic red flash + electric particles
        const oraclePhaseFlip = () => {
            if (!this.blochSpheres || this.blochSpheres.length === 0) {
                console.warn('No Bloch spheres available for animation');
                return;
            }
            
            this.blochSpheres.forEach((sphere, i) => {
                if (!sphere || !sphere.position || !sphere.material) {
                    console.warn(`Invalid sphere at index ${i}`);
                    return;
                }
                
                tl.to(sphere.material, {emissiveIntensity: 3, emissive: 0xff0000, duration: 0.3}, 0);
                tl.to(sphere.material, {emissiveIntensity: 0.6, emissive: 0x003366, duration: 0.8}, "+=0.3");
                this.createElectricArc(sphere.position);
            });

            // Central zap
            this.createSimpleParticleBurst(new THREE.Vector3(0,0,0));
            this.updateStatus('Grover Oracle: phase flip on target state');
        };

        // Diffusion operator – amplify amplitude visually
        const diffusion = () => {
            if (!this.blochSpheres || this.blochSpheres.length === 0) {
                console.warn('No Bloch spheres available for diffusion');
                return;
            }
            
            const validSpheres = this.blochSpheres.filter(s => s && s.scale);
            if (validSpheres.length === 0) {
                console.warn('No valid spheres for diffusion animation');
                return;
            }
            
            tl.to(validSpheres.map(s => s.scale), {
                x: 1.2, y: 1.2, z: 1.2,
                duration: 0.6,
                stagger: 0.05,
                ease: "power2.out"
            }, 0);
            tl.to(validSpheres.map(s => s.scale), {
                x: 1, y: 1, z: 1,
                duration: 0.6
            }, "+=0.1");

            this.updateStatus('Diffusion operator – amplitude amplification');
        };

        // Run iterations
        for (let iter = 1; iter <= this.groverIterations; iter++) {
            oraclePhaseFlip();
            diffusion();
            this.updateStatus(`Grover iteration ${iter}/${this.groverIterations} complete`);
        }

        // Final dramatic reveal
        const validSpheresForFinal = this.blochSpheres.filter(s => s && s.material);
        if (validSpheresForFinal.length > 0) {
            tl.to(validSpheresForFinal.map(s => s.material), {
                emissive: 0x00ff00,
                emissiveIntensity: 4,
                duration: 1.5
            }, "+=1");
        }
        
        tl.call(() => {
            this.updateStatus('Grover attack fails at scale – PAW remains secure');
        });
        
        // Store timeline reference so we can kill it if scene changes
        this.groverTimeline = tl;
    }

    createGroverLayout(numQubits) {
        // Grover's algorithm visualization - oracle and diffusion operator
        const gridSize = Math.ceil(Math.sqrt(numQubits));
        const spacing = 40;
        const offsetX = -(gridSize * spacing) / 2;
        const offsetZ = -(gridSize * spacing) / 2;

        for (let i = 0; i < numQubits; i++) {
            const row = Math.floor(i / gridSize);
            const col = i % gridSize;
            const x = offsetX + col * spacing;
            const z = offsetZ + row * spacing;
            const y = Math.sin(this.animationFrame * 0.01 + i * 0.1) * 10; // Wave effect

            this.nodePositions.push(new THREE.Vector3(x, y, z));
            this.createQuantumBit(x, y, z, i, 'grover');
        }

        // Create search space connections
        this.createGroverConnections(numQubits, gridSize);
    }

    createShorLayout(numQubits) {
        // Shor's algorithm - quantum fourier transform visualization
        const levels = 5; // QFT levels
        const radius = 150;
        
        for (let i = 0; i < numQubits; i++) {
            const level = Math.floor(i / (numQubits / levels));
            const angleInLevel = (i % (numQubits / levels)) * (2 * Math.PI / (numQubits / levels));
            
            const x = Math.cos(angleInLevel) * (radius - level * 25);
            const y = level * 40 - 80;
            const z = Math.sin(angleInLevel) * (radius - level * 25);

            this.nodePositions.push(new THREE.Vector3(x, y, z));
            this.createQuantumBit(x, y, z, i, 'shor');
        }

        // Create QFT connections
        this.createShorConnections(numQubits, levels);
    }

    createSuperpositionLayout(numQubits) {
        // Bloch sphere representation for superposition states
        const radius = 200;
        const height = 150;

        for (let i = 0; i < numQubits; i++) {
            const theta = (i / numQubits) * Math.PI; // Polar angle
            const phi = (i / numQubits) * Math.PI * 4; // Azimuthal angle
            
            const x = radius * Math.sin(theta) * Math.cos(phi);
            const y = radius * Math.cos(theta);
            const z = radius * Math.sin(theta) * Math.sin(phi);

            this.nodePositions.push(new THREE.Vector3(x, y, z));
            this.createQuantumBit(x, y, z, i, 'superposition');
            
            // Add Bloch sphere vectors
            this.addBlochVector(x, y, z, i);
        }

        // Create entanglement pairs
        for (let i = 0; i < numQubits - 1; i += 2) {
            this.entangledPairs.push([i, i + 1]);
            this.createQuantumConnection(i, i + 1, 'entangle');
        }

        // Create additional entanglement connections
        this.createQuantumConnections(numQubits);
    }
    
    addBlochVector(x, y, z, index) {
        // Create Bloch vector visualization
        const origin = new THREE.Vector3(x, y, z);
        const direction = new THREE.Vector3(
            Math.random() - 0.5,
            Math.random() - 0.5,
            Math.random() - 0.5
        ).normalize();
        
        const arrowHelper = new THREE.ArrowHelper(direction, origin, 15, 0x00ffff, 5, 3);
        arrowHelper.userData = {
            type: 'blochVector',
            index: index,
            basePosition: origin.clone()
        };
        
        this.scene.add(arrowHelper);
        // Store reference in the qubit node
        if (this.nodes[index]) {
            this.nodes[index].userData.blochVector = arrowHelper;
        }
    }

    createQuantumBit(x, y, z, index, mode = 'grover') {
        // Create qubit visualization based on quantum algorithm mode
        const geometry = mode === 'shor' ? 
            new THREE.OctahedronGeometry(5, 0) : 
            new THREE.SphereGeometry(mode === 'grover' ? 6 : 8, 16, 16);

        // Calculate quantum security parameters
        const classicalBits = 2048; // RSA key size
        const quantumBits = Math.ceil(Math.log2(classicalBits)); // Qubits needed for attack
        const groverAdvantage = Math.sqrt(Math.pow(2, this.numSignatures)); // Square root speedup
        const shorAdvantage = Math.pow(this.numSignatures, 3); // Polynomial speedup
        
        // Security level calculation (NIST levels)
        const securityLevel = this.threatLevel === 'HIGH' ? 5 : 
                            this.threatLevel === 'MEDIUM' ? 3 : 1;

        const material = new THREE.ShaderMaterial({
            uniforms: {
                time: { value: 0 },
                mode: { value: mode === 'grover' ? 0.0 : mode === 'shor' ? 1.0 : 2.0 },
                securityLevel: { value: securityLevel / 5.0 },
                quantumThreat: { value: this.showAttacks ? 1.0 : 0.0 },
                nodeIndex: { value: index / this.numSignatures }
            },
            vertexShader: `
                uniform float time;
                uniform float mode;
                
                varying vec3 vPosition;
                varying vec3 vNormal;
                varying vec2 vUv;
                varying vec3 vWorldPos;

                void main() {
                    vPosition = position;
                    vNormal = normal;
                    vUv = uv;
                    
                    vec3 pos = position;
                    
                    // Superposition wave distortion
                    if (mode > 1.5) {
                        float wave1 = sin(position.x * 0.5 + time * 2.0) * 0.3;
                        float wave2 = cos(position.y * 0.5 + time * 1.5) * 0.3;
                        float wave3 = sin(position.z * 0.5 + time * 1.8) * 0.3;
                        pos += normal * (wave1 + wave2 + wave3);
                    }
                    
                    vec4 worldPos = modelMatrix * vec4(pos, 1.0);
                    vWorldPos = worldPos.xyz;
                    gl_Position = projectionMatrix * viewMatrix * worldPos;
                }
            `,
            fragmentShader: `
                uniform float time;
                uniform float mode; // 0=grover, 1=shor, 2=superposition
                uniform float securityLevel;
                uniform float quantumThreat;
                uniform float nodeIndex;
                
                varying vec3 vPosition;
                varying vec3 vNormal;
                varying vec2 vUv;
                varying vec3 vWorldPos;

                void main() {
                    vec3 color = vec3(0.0);
                    
                    if (mode < 0.5) {
                        // Grover's algorithm - search visualization
                        float searchPhase = sin(time * 2.0 + nodeIndex * 6.28) * 0.5 + 0.5;
                        color = mix(
                            vec3(0.0, 0.8, 1.0), // Cyan - unsearched
                            vec3(1.0, 0.0, 1.0), // Magenta - marked state
                            searchPhase * quantumThreat
                        );
                        
                        // Add amplitude rings
                        float rings = sin(length(vPosition) * 50.0 - time * 3.0);
                        color += vec3(0.0, 1.0, 0.5) * rings * 0.2;
                        
                    } else if (mode < 1.5) {
                        // Shor's algorithm - factorization threat
                        float qftPhase = atan(vPosition.y, vPosition.x) + time;
                        float periodicity = sin(qftPhase * 8.0) * 0.5 + 0.5;
                        
                        color = mix(
                            vec3(1.0, 0.5, 0.0), // Orange - classical RSA
                            vec3(0.0, 1.0, 0.0), // Green - quantum factored
                            periodicity * quantumThreat
                        );
                        
                        // Add QFT waves
                        float wave = sin(vWorldPos.y * 0.1 + time * 2.0);
                        color += vec3(0.5, 1.0, 0.5) * wave * 0.3;
                        
                    } else {
                        // Superposition - Bloch sphere visualization
                        float theta = atan(vPosition.z, vPosition.x);
                        float phi = acos(vPosition.y / length(vPosition));
                        
                        // Quantum state colors
                        vec3 zeroState = vec3(0.0, 0.5, 1.0); // Blue |0⟩
                        vec3 oneState = vec3(1.0, 0.5, 0.0);  // Orange |1⟩
                        vec3 superState = vec3(0.5, 1.0, 0.5); // Green superposition
                        
                        float stateMix = sin(phi + time) * 0.5 + 0.5;
                        color = mix(mix(zeroState, oneState, stateMix), superState, 0.3);
                        
                        // Add entanglement visualization
                        float entangle = sin(theta * 4.0 + time * 1.5) * 0.5 + 0.5;
                        color = mix(color, vec3(1.0, 0.0, 1.0), entangle * 0.3);
                    }
                    
                    // Security level overlay
                    float securityGlow = securityLevel;
                    color = mix(color, vec3(0.0, 1.0, 0.0), securityGlow * 0.2);
                    
                    // Threat indicator
                    if (quantumThreat > 0.5) {
                        float pulse = sin(time * 10.0) * 0.5 + 0.5;
                        color = mix(color, vec3(1.0, 0.0, 0.0), pulse * 0.3);
                    }
                    
                    // Edge glow
                    float fresnel = 1.0 - abs(dot(normalize(vNormal), vec3(0.0, 0.0, 1.0)));
                    color += color * fresnel * 0.5;
                    
                    // Add subtle noise for realism
                    float noise = fract(sin(dot(vPosition.xy, vec2(12.9898, 78.233))) * 43758.5453);
                    color += noise * 0.05;
                    
                    gl_FragColor = vec4(color, 0.85);
                }
            `,
            transparent: true,
            side: THREE.DoubleSide,
            depthWrite: false,
            blending: THREE.AdditiveBlending
        });

        const qubit = new THREE.Mesh(geometry, material);
        qubit.position.set(x, y, z);

        // Enhanced quantum state data for crypto professionals
        const isMarkedState = index === Math.floor(this.numSignatures / 2); // Middle element as marked
        qubit.userData = {
            index,
            mode,
            quantumState: Math.random() > 0.5 ? '|0⟩' : '|1⟩',
            amplitude: isMarkedState ? 0.8 : 0.2,
            phase: Math.random() * Math.PI * 2,
            groverIterations: Math.ceil(Math.PI / 4 * Math.sqrt(this.numSignatures)),
            shorPeriod: Math.floor(Math.random() * 15) + 2,
            securityBits: mode === 'grover' ? 
                Math.log2(groverAdvantage) : 
                Math.log2(shorAdvantage),
            isMarkedState,
            entanglementPartners: []
        };

        // Add visual indicators for marked states or threats
        if (isMarkedState && mode === 'grover') {
            this.addQuantumMarker(qubit, 'marked');
        } else if (mode === 'shor' && index % 7 === 0) {
            this.addQuantumMarker(qubit, 'factor');
        }

        this.scene.add(qubit);
        this.nodes.push(qubit);
    }

    addQuantumMarker(qubit, type) {
        const markerGeometry = new THREE.RingGeometry(10, 12, 6);
        const markerMaterial = new THREE.MeshBasicMaterial({
            color: type === 'marked' ? 0xff00ff : 0x00ff00,
            transparent: true,
            opacity: 0.8,
            side: THREE.DoubleSide
        });
        const marker = new THREE.Mesh(markerGeometry, markerMaterial);
        marker.rotation.x = Math.PI / 2;
        qubit.add(marker);
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

    createGroverConnections(numQubits, gridSize) {
        // Create grid connections for Grover search space
        for (let i = 0; i < numQubits; i++) {
            const row = Math.floor(i / gridSize);
            const col = i % gridSize;
            
            // Connect to right neighbor
            if (col < gridSize - 1 && i + 1 < numQubits) {
                this.createQuantumConnection(i, i + 1, 'grover');
            }
            
            // Connect to bottom neighbor
            if (row < gridSize - 1 && i + gridSize < numQubits) {
                this.createQuantumConnection(i, i + gridSize, 'grover');
            }
            
            // Diagonal oracle connections for marked states
            if (this.nodes[i] && this.nodes[i].userData.isMarkedState) {
                // Connect marked state to all corners
                const corners = [0, gridSize - 1, numQubits - gridSize, numQubits - 1];
                corners.forEach(corner => {
                    if (corner !== i && corner < numQubits) {
                        this.createQuantumConnection(i, corner, 'oracle');
                    }
                });
            }
        }
    }

    createShorConnections(numQubits, levels) {
        // Create QFT butterfly connections
        const nodesPerLevel = Math.ceil(numQubits / levels);
        
        for (let level = 0; level < levels - 1; level++) {
            const startIdx = level * nodesPerLevel;
            const nextStartIdx = (level + 1) * nodesPerLevel;
            
            for (let i = 0; i < nodesPerLevel; i++) {
                const currentIdx = startIdx + i;
                if (currentIdx >= numQubits) break;
                
                // QFT butterfly pattern
                const stride = Math.pow(2, level);
                for (let j = 0; j < stride && j < nodesPerLevel; j++) {
                    const targetIdx = nextStartIdx + ((i + j) % nodesPerLevel);
                    if (targetIdx < numQubits) {
                        this.createQuantumConnection(currentIdx, targetIdx, 'qft');
                    }
                }
            }
        }
    }

    createQuantumConnection(fromIndex, toIndex, type = 'entangle') {
        if (fromIndex >= this.nodePositions.length || toIndex >= this.nodePositions.length) return;

        const fromPos = this.nodePositions[fromIndex];
        const toPos = this.nodePositions[toIndex];

        const geometry = new THREE.BufferGeometry().setFromPoints([fromPos, toPos]);

        // Different colors for different quantum operations
        const colors = {
            'entangle': 0x00ffff, // Cyan - entanglement
            'grover': 0x0080ff,   // Blue - search connections
            'oracle': 0xff00ff,   // Magenta - oracle marking
            'qft': 0xffaa00      // Orange - quantum fourier transform
        };

        const material = new THREE.LineBasicMaterial({
            color: colors[type] || 0x00ffff,
            transparent: true,
            opacity: type === 'oracle' ? 0.8 : 0.4,
            linewidth: type === 'oracle' ? 3 : 1
        });

        const line = new THREE.Line(geometry, material);
        line.userData = { type, fromIndex, toIndex };
        
        this.scene.add(line);
        this.connections.push(line);
    }

    createQuantumMetricsDisplay() {
        // Create 3D text for quantum metrics
        const metrics = {
            algorithm: this.quantumMode === 'grover' ? 'Grover Search' : 
                      this.quantumMode === 'shor' ? 'Shor Factorization' : 'Superposition',
            qubits: this.numSignatures,
            iterations: this.quantumMode === 'grover' ? 
                Math.ceil(Math.PI / 4 * Math.sqrt(this.numSignatures)) : 
                Math.pow(Math.log2(2048), 2), // For 2048-bit RSA
            threatLevel: `NIST Level ${this.threatLevel === 'HIGH' ? 5 : this.threatLevel === 'MEDIUM' ? 3 : 1}`,
            quantumAdvantage: this.quantumMode === 'grover' ? 
                `√N = ${Math.sqrt(this.numSignatures).toFixed(1)}x` : 
                `O(log³ N)`
        };

        // Store metrics for display update
        this.quantumMetrics = metrics;
    }
    
    createQuantumStateParticles() {
        // Create particles representing 2^n quantum states
        const maxQubits = Math.min(this.numSignatures, 10); // Cap at 10 for performance (2^10 = 1024 particles)
        const numStates = Math.pow(2, maxQubits);
        
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(numStates * 3);
        const colors = new Float32Array(numStates * 3);
        const velocities = [];
        
        // Color based on threat level
        const particleColor = this.threatLevel === 'HIGH' ? [1, 0, 0] :  // Red for high threat
                            this.threatLevel === 'MEDIUM' ? [1, 1, 0] :  // Yellow for medium
                            [0, 1, 0]; // Green for low/secure
        
        for (let i = 0; i < numStates; i++) {
            const angle = (i / numStates) * Math.PI * 2;
            const radius = 100 + Math.random() * 200;
            const height = (Math.random() - 0.5) * 300;
            
            positions[i * 3] = Math.cos(angle) * radius;
            positions[i * 3 + 1] = height;
            positions[i * 3 + 2] = Math.sin(angle) * radius;
            
            // Add some variation to colors
            colors[i * 3] = particleColor[0] + (Math.random() - 0.5) * 0.2;
            colors[i * 3 + 1] = particleColor[1] + (Math.random() - 0.5) * 0.2;
            colors[i * 3 + 2] = particleColor[2] + (Math.random() - 0.5) * 0.2;
            
            // Store velocities for entanglement
            velocities.push({
                x: (Math.random() - 0.5) * 2,
                y: (Math.random() - 0.5) * 2,
                z: (Math.random() - 0.5) * 2
            });
        }
        
        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
        geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
        
        const material = new THREE.PointsMaterial({
            size: 2.0,
            vertexColors: true,
            transparent: true,
            opacity: 0.6,
            blending: THREE.AdditiveBlending
        });
        
        const particleSystem = new THREE.Points(geometry, material);
        particleSystem.userData = {
            type: 'quantumStates',
            velocities: velocities,
            numStates: numStates,
            life: -1, // Permanent particles
            entangled: true
        };
        
        this.scene.add(particleSystem);
        this.particles.push(particleSystem);
    }
    
    performMeasurementCollapse() {
        this.updateStatus('⚡ Quantum Measurement - Wave Function Collapse!');
        
        // Collapse all wave functions with GSAP
        this.nodes.forEach((node, index) => {
            if (node.userData && node.userData.mode === 'superposition') {
                // Stop wave distortions
                if (node.material.uniforms) {
                    gsap.to(node.material.uniforms.time, {
                        value: 0,
                        duration: 1.5,
                        ease: "power2.out"
                    });
                }
                
                // Collapse to definite state
                const collapsed = Math.random() > 0.5 ? 1 : -1;
                gsap.to(node.position, {
                    y: this.nodePositions[index].y + (collapsed * 30),
                    duration: 1.5,
                    ease: "bounce.out"
                });
                
                // Collapse Bloch vector
                if (node.userData.blochVector) {
                    const vector = node.userData.blochVector;
                    gsap.to(vector.rotation, {
                        x: 0,
                        y: 0,
                        z: collapsed > 0 ? 0 : Math.PI,
                        duration: 1.5,
                        ease: "power2.inOut"
                    });
                }
            }
        });
        
        // Collapse particle states
        this.particles.forEach(particleSystem => {
            if (particleSystem.userData.type === 'quantumStates') {
                // Collapse to discrete positions
                const positions = particleSystem.geometry.attributes.position.array;
                const numStates = particleSystem.userData.numStates;
                
                for (let i = 0; i < numStates; i++) {
                    const targetY = Math.random() > 0.5 ? 50 : -50;
                    const currentY = positions[i * 3 + 1];
                    
                    gsap.to(positions, {
                        [i * 3 + 1]: targetY,
                        duration: 2,
                        ease: "elastic.out(1, 0.5)",
                        onUpdate: () => {
                            particleSystem.geometry.attributes.position.needsUpdate = true;
                        }
                    });
                }
            }
        });
        
        // Reset after 3 seconds
        setTimeout(() => {
            this.updateStatus('🔄 Quantum states reset to superposition');
        }, 3000);
    }

    // ────── VISUALISATION HELPERS (add these to the class) ──────
    addTextLabel(position, text, color = 0xffffff, size = 20) {
        const canvas = document.createElement('canvas');
        canvas.width = 512;
        canvas.height = 128;
        const ctx = canvas.getContext('2d');
        
        ctx.font = `Bold ${size}px Arial`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        
        ctx.fillStyle = 'rgba(0,0,0,0.8)';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        
        ctx.fillStyle = `#${color.toString(16).padStart(6,'0')}`;
        ctx.fillText(text, canvas.width / 2, canvas.height / 2);
        
        const texture = new THREE.CanvasTexture(canvas);
        const sprite = new THREE.Sprite(new THREE.SpriteMaterial({map: texture, transparent: true, depthTest: false}));
        sprite.position.copy(position);
        sprite.scale.set(200, 50, 1);
        this.scene.add(sprite);
        return sprite;
    }

    createParticleBurst(position, color = 0x00ffff, count = 60) {
        const geometry = new THREE.BufferGeometry();
        const positions = [];
        const velocities = [];
        
        for (let i = 0; i < count; i++) {
            const theta = Math.random() * Math.PI * 2;
            const phi = Math.random() * Math.PI;
            const speed = 2 + Math.random() * 8;
            
            positions.push(0,0,0);
            velocities.push(
                Math.sin(phi) * Math.cos(theta) * speed,
                Math.cos(phi) * speed + 3,
                Math.sin(phi) * Math.sin(theta) * speed
            );
        }
        
        geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
        geometry.setAttribute('velocity', new THREE.Float32BufferAttribute(velocities, 3));
        
        const material = new THREE.PointsMaterial({color, size: 8, transparent: true, opacity: 1});
        const particles = new THREE.Points(geometry, material);
        particles.position.copy(position);
        this.scene.add(particles);
        
        // Animate burst
        gsap.to(material, {opacity: 0, duration: 1.2, ease: "power2.out"});
        const start = performance.now();
        const animateBurst = () => {
            const elapsed = (performance.now() - start) / 1000;
            if (elapsed > 1.5) {
                this.scene.remove(particles);
                return;
            }
            const pos = geometry.attributes.position.array;
            for (let i = 0; i < pos.length; i += 3) {
                pos[i]   += geometry.attributes.velocity.array[i]   * elapsed * 0.1;
                pos[i+1] += geometry.attributes.velocity.array[i+1] * elapsed * 0.1 - 9.81 * elapsed * elapsed * 0.5; // gravity
                pos[i+2] += geometry.attributes.velocity.array[i+2] * elapsed * 0.1;
            }
            geometry.attributes.position.needsUpdate = true;
            requestAnimationFrame(animateBurst);
        };
        animateBurst();
    }

    createSimpleParticleBurst(position, color = 0x00ff88) {
        this.createParticleBurst(position, color, 40);
    }

    createElectricArc(position) {
        const points = [];
        const segments = 15;
        for (let i = 0; i <= segments; i++) {
            const t = i / segments;
            points.push(new THREE.Vector3(
                (Math.random() - 0.5) * 60,
                t * 100 - 50,
                (Math.random() - 0.5) * 60
            ));
        }
        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        const material = new THREE.LineBasicMaterial({
            color: 0x00ffff,
            transparent: true,
            opacity: 1,
            linewidth: 3
        });
        const arc = new THREE.Line(geometry, material);
        arc.position.copy(position);
        this.scene.add(arc);
        
        gsap.to(material, {
            opacity: 0,
            duration: 0.7,
            ease: "power1.out",
            onComplete: () => this.scene.remove(arc)
        });
    }

    animate() {
        if (this.controls?.update) this.controls.update();
        requestAnimationFrame(() => this.animate());

        const now = performance.now();
        this.frameCount++;
        if (now - this.lastTime >= 1000) {
            this.fps = Math.round((this.frameCount * 1000) / (now - this.lastTime));
            this.frameCount = 0;
            this.lastTime = now;
            this.updatePerformanceDisplay(now - this.prevFrameTime);
        }

        const deltaTime = now - this.prevFrameTime;
        this.prevFrameTime = now;

        this.animationFrame++;

        // Performance profiling with Stats.js (if available)
        if (this.stats) {
            this.stats.update();
        }

        // Smooth mouse following for passive 3D exploration
        this.targetX += (this.mouseX - this.targetX) * 0.02;
        this.targetY += (-this.mouseY - this.targetY) * 0.02;

        this.camera.position.x += (this.targetX * 200 - this.camera.position.x) * 0.05;
        this.camera.position.y += (this.targetY * 200 - this.camera.position.y) * 0.05;

        this.camera.lookAt(this.scene.position);

        // Update quantum mode cycling every 15 seconds
        if (this.quantumView && this.animationFrame % 900 === 0) {
            // Cycle through quantum algorithms
            this.updateVisualization();
        }

        // Update connection animations for quantum view
        if (this.quantumView) {
            this.connections.forEach((connection, index) => {
                if (connection.userData && connection.userData.type) {
                    const opacity = connection.userData.type === 'oracle' ? 
                        Math.sin(this.animationFrame * 0.1) * 0.4 + 0.6 : 
                        Math.sin(this.animationFrame * 0.03 + index * 0.05) * 0.2 + 0.3;
                    connection.material.opacity = opacity;
                }
            });
        }

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
                const velocities = particle.userData.velocities;

                // Check if this is a simple burst particle (no alpha attribute)
                const isBurstParticle = particle.userData.isBurstParticle;

                if (particle.userData.type === 'quantumStates') {
                    // Handle quantum state particles with entanglement
                    for (let i = 0; i < velocities.length; i++) {
                        const i3 = i * 3;
                        
                        // Entanglement: pairs have mirrored velocities
                        if (particle.userData.entangled && i % 2 === 1) {
                            // Mirror the velocity of the previous particle
                            velocities[i].x = -velocities[i - 1].x;
                            velocities[i].y = -velocities[i - 1].y;
                            velocities[i].z = -velocities[i - 1].z;
                        }
                        
                        // Update position
                        positions[i3] += velocities[i].x * 0.2;
                        positions[i3 + 1] += velocities[i].y * 0.2;
                        positions[i3 + 2] += velocities[i].z * 0.2;
                        
                        // Quantum oscillation
                        const oscillation = Math.sin(this.animationFrame * 0.05 + i * 0.1) * 0.5;
                        positions[i3 + 1] += oscillation;
                    }
                    
                    // No life reduction for quantum states (permanent)
                } else if (isBurstParticle) {
                    // Handle simple burst particles
                    for (let i = 0; i < velocities.length; i++) {
                        const i3 = i * 3;

                        // Update position
                        positions[i3] += velocities[i].x * 0.5;
                        positions[i3 + 1] += velocities[i].y * 0.5;
                        positions[i3 + 2] += velocities[i].z * 0.5;

                        // Apply gravity/damping
                        velocities[i].y -= 0.3; // gravity
                        velocities[i].x *= 0.95; // air resistance
                        velocities[i].y *= 0.95;
                        velocities[i].z *= 0.95;
                    }

                    // Update material opacity for fade out
                    const lifeRatio = particle.userData.life / particle.userData.initialLife;
                    particle.material.opacity = Math.max(0, lifeRatio);
                } else {
                    // Handle complex burst particles with alpha attribute
                    const alphas = particle.geometry.attributes.alpha.array;

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
                    
                    // Mark alpha attribute as needing update
                    particle.geometry.attributes.alpha.needsUpdate = true;
                }

                // Mark position attribute as needing update
                particle.geometry.attributes.position.needsUpdate = true;

                // Quantum states are permanent
                if (particle.userData.type === 'quantumStates') {
                    return true;
                }

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
        if (this.demoMode && this.animationFrame % 60 === 0) { // More frequent bursts
            // Create bursts in the general visualization space
            const angle = Math.random() * Math.PI * 2;
            const radius = Math.random() * 150 + 50;
            const height = (Math.random() - 0.5) * 150;
            
            const burstPosition = new THREE.Vector3(
                Math.cos(angle) * radius,
                height,
                Math.sin(angle) * radius
            );
            
            this.createSimpleParticleBurst(burstPosition);
        }

        // Debug: Log every 60th frame
        if (this.animationFrame % 60 === 0) {
            console.log(`Frame ${this.animationFrame}: Rendering ${this.nodes.length} nodes, ${this.connections.length} connections`);
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

        // Update shader uniforms
        if (qubit.material.uniforms) {
            qubit.material.uniforms.time.value = this.animationFrame * 0.01;
            qubit.material.uniforms.quantumThreat.value = this.showAttacks ? 1.0 : 0.0;
        }

        // Mode-specific animations
        switch (userData.mode) {
            case 'grover':
                this.animateGroverQubit(qubit, userData);
                break;
            case 'shor':
                this.animateShorQubit(qubit, userData);
                break;
            case 'superposition':
                this.animateSuperpositionQubit(qubit, userData);
                break;
        }

        // Update markers if any
        qubit.children.forEach(child => {
            if (child.geometry && child.geometry.type === 'RingGeometry') {
                child.rotation.z += 0.02;
                const pulse = Math.sin(this.animationFrame * 0.05) * 0.2 + 0.8;
                child.scale.setScalar(pulse);
            }
        });
    }

    animateGroverQubit(qubit, userData) {
        // Grover's algorithm animation with iterative search visualization
        const groverIteration = Math.floor(this.animationFrame / 120) % userData.groverIterations;
        const searchProgress = groverIteration / userData.groverIterations;
        
        // Calculate current search space reduction
        const currentSearchSpace = Math.pow(2, this.numSignatures) * Math.pow(0.5, groverIteration);
        
        // Oracle phase - mark states with low probability flashes
        if (groverIteration % 4 === 0) {
            if (userData.isMarkedState) {
                // Marked state gets stronger pulse
                const pulse = Math.sin(this.animationFrame * 0.1) * 0.5 + 1.5;
                qubit.scale.setScalar(pulse);
                
                // Bright flash for marked state
                if (qubit.material.uniforms) {
                    qubit.material.uniforms.quantumThreat.value = 1.0;
                }
            } else {
                // Non-marked states flash red with probability 8.64e-78
                const flashProbability = 8.64e-78 * Math.pow(10, 78); // Scale up for visualization
                if (Math.random() < flashProbability * 0.01) {
                    // Red flash for low probability
                    gsap.to(qubit.material, {
                        emissive: 0xff0000,
                        emissiveIntensity: 0.8,
                        duration: 0.2,
                        yoyo: true,
                        repeat: 1,
                        ease: "power2.inOut"
                    });
                }
            }
            
            // Rotate faster during oracle query
            qubit.rotation.y += 0.1 * (1 + searchProgress);
        } else {
            qubit.scale.setScalar(1.0);
        }

        // Diffusion operator visualization - wave collapses as search narrows
        if (groverIteration % 4 === 2) {
            // Wave effect that decreases with iterations
            const waveAmplitude = 10 * (1 - searchProgress);
            const wave = Math.sin(this.animationFrame * 0.05 + userData.index * 0.2) * waveAmplitude;
            qubit.position.y = this.nodePositions[userData.index].y + wave;
            
            // Create search space reduction particles
            if (this.animationFrame % 60 === 0) {
                this.createGroverSearchParticles(qubit.position, currentSearchSpace);
            }
        }

        // Continuous rotation representing quantum phase evolution
        qubit.rotation.x += 0.005 * (1 + searchProgress * 2);
        qubit.rotation.z += 0.003 * (1 + searchProgress);
        
        // Color intensity increases as algorithm converges
        if (qubit.material.uniforms) {
            const convergence = Math.pow(searchProgress, 2);
            qubit.material.uniforms.nodeIndex.value = convergence;
        }
    }
    
    createGroverSearchParticles(position, searchSpace) {
        // Create particles showing search space reduction
        const particleCount = Math.min(20, Math.log2(searchSpace));
        const geometry = new THREE.BufferGeometry();
        const positions = new Float32Array(particleCount * 3);
        
        for (let i = 0; i < particleCount; i++) {
            const angle = (i / particleCount) * Math.PI * 2;
            positions[i * 3] = position.x + Math.cos(angle) * 30;
            positions[i * 3 + 1] = position.y;
            positions[i * 3 + 2] = position.z + Math.sin(angle) * 30;
        }
        
        geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
        
        const material = new THREE.PointsMaterial({
            color: 0xff0000, // Red for eliminated states
            size: 2.0,
            transparent: true,
            opacity: 0.8
        });
        
        const searchParticles = new THREE.Points(geometry, material);
        searchParticles.userData = {
            type: 'groverSearch',
            life: 60,
            initialLife: 60
        };
        
        this.scene.add(searchParticles);
        this.particles.push(searchParticles);
        
        // Animate particles halving the search space
        gsap.to(searchParticles.scale, {
            x: 0.1,
            y: 0.1,
            z: 0.1,
            duration: 1.0,
            ease: "power2.in"
        });
        
        gsap.to(searchParticles.material, {
            opacity: 0,
            duration: 1.0,
            ease: "power2.out",
            onComplete: () => {
                this.scene.remove(searchParticles);
                const idx = this.particles.indexOf(searchParticles);
                if (idx > -1) this.particles.splice(idx, 1);
            }
        });
    }

    animateShorQubit(qubit, userData) {
        // Shor's algorithm - QFT animation
        const qftPhase = (this.animationFrame * 0.02 + userData.index * 0.1) % (Math.PI * 2);
        
        // Simulate period finding
        const period = userData.shorPeriod;
        const amplitude = Math.sin(qftPhase * period) * 0.5 + 0.5;
        
        // Vertical oscillation based on QFT
        const baseY = this.nodePositions[userData.index].y;
        qubit.position.y = baseY + amplitude * 30;
        
        // Rotation speed varies with level
        const level = Math.floor(userData.index / (this.numSignatures / 5));
        qubit.rotation.y += 0.01 * (level + 1);
        qubit.rotation.x += 0.005;
        
        // Scale based on factorization progress
        if (userData.index % userData.shorPeriod === 0) {
            const factorPulse = Math.sin(this.animationFrame * 0.08) * 0.2 + 1.1;
            qubit.scale.setScalar(factorPulse);
        }
    }

    animateSuperpositionQubit(qubit, userData) {
        // Bloch sphere rotation animation
        const theta = userData.phase + this.animationFrame * 0.01;
        const phi = userData.index * 0.1 + this.animationFrame * 0.005;
        
        // Precession around Bloch sphere
        const radius = 200;
        const basePos = this.nodePositions[userData.index];
        
        qubit.position.x = basePos.x + Math.sin(theta) * Math.cos(phi) * 20;
        qubit.position.y = basePos.y + Math.cos(theta) * 20;
        qubit.position.z = basePos.z + Math.sin(theta) * Math.sin(phi) * 20;
        
        // Spin representing quantum state evolution
        qubit.rotation.x = theta;
        qubit.rotation.y = phi;
        qubit.rotation.z += 0.02;
        
        // Animate Bloch vector if it exists
        if (userData.blochVector) {
            const vector = userData.blochVector;
            
            // Random walk on Bloch sphere
            const wanderSpeed = 0.02;
            const newTheta = theta * 2;
            const newPhi = phi * 1.5;
            
            // Update vector direction
            const direction = new THREE.Vector3(
                Math.sin(newTheta) * Math.cos(newPhi),
                Math.cos(newTheta),
                Math.sin(newTheta) * Math.sin(newPhi)
            );
            
            vector.setDirection(direction.normalize());
            
            // Follow qubit position
            vector.position.copy(qubit.position);
        }
        
        // Entanglement visualization - connected qubits synchronize
        if (this.animationFrame % 60 === 0) {
            // Check for entangled pairs
            this.entangledPairs.forEach(([idx1, idx2]) => {
                if (userData.index === idx1 || userData.index === idx2) {
                    // Synchronize phase with entangled partner
                    const partnerIdx = userData.index === idx1 ? idx2 : idx1;
                    const partner = this.nodes[partnerIdx];
                    if (partner && partner.userData) {
                        // Entangled states have opposite phases
                        userData.phase = -partner.userData.phase;
                    }
                }
            });
        }
    }

    runMergeAnimation() {
        // PAW merge animation: Dramatic GSAP-powered visualization of quantum-armored signature bundling
        if (this.nodes.length < 2) {
            this.updateStatus('❌ Not enough nodes for merge animation');
            return;
        }

        // Check GSAP availability
        if (!window.gsap || typeof gsap === 'undefined') {
            // Check if GSAP is still loading
            if (window.dependencyStatus && !window.dependencyStatus.gsap) {
                this.updateStatus('⏳ GSAP is still loading, please try again in a moment');
                // Try again in 1 second
                setTimeout(() => this.runMergeAnimation(), 1000);
                return;
            }
            this.updateStatus('GSAP not available, using simple animation');
            this.simpleMergeAnimation();
            return;
        }

        // Test GSAP functionality
        try {
            gsap.timeline(); // Better test for GSAP
        } catch (e) {
            this.updateStatus('GSAP error, using simple animation');
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
        this.updateStatus('🔄 Running Simple PAW Merge Animation...');

        if (this.nodes.length < 2) return;

        // Create merge animations without GSAP
        const pairs = [];
        const numPairs = Math.min(5, Math.floor(this.nodes.length / 2));

        // Select pairs of nodes to merge
        for (let i = 0; i < numPairs * 2; i += 2) {
            if (i + 1 < this.nodes.length) {
                const node1 = this.nodes[i];
                const node2 = this.nodes[i + 1];
                
                // Get positions
                let pos1, pos2;
                if (node1.isInstancedMesh && node1.userData.instances) {
                    pos1 = node1.userData.instances[0].position.clone();
                } else if (node1.position) {
                    pos1 = node1.position.clone();
                }
                
                if (node2.isInstancedMesh && node2.userData.instances) {
                    pos2 = node2.userData.instances[0].position.clone();
                } else if (node2.position) {
                    pos2 = node2.position.clone();
                }

                if (pos1 && pos2) {
                    pairs.push({ pos1, pos2, node1, node2 });
                }
            }
        }

        // Animate the pairs
        let pairIndex = 0;
        const animatePair = () => {
            if (pairIndex < pairs.length) {
                const pair = pairs[pairIndex];
                const midpoint = new THREE.Vector3()
                    .addVectors(pair.pos1, pair.pos2)
                    .multiplyScalar(0.5);

                // Create energy arc effect
                this.createEnergyArc(pair.pos1, pair.pos2);
                
                // Create burst at midpoint
                setTimeout(() => {
                    this.createSimpleParticleBurst(midpoint);
                    this.createExplosionEffect(midpoint);
                }, 300);

                pairIndex++;
                setTimeout(animatePair, 800);
            } else {
                this.updateStatus('✨ PAW Simple Animation Complete');
            }
        };

        animatePair();
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
        this.demoMode = true;
        this.numSignatures = 1000;
        document.getElementById('signature-slider').value = 1000;
        document.getElementById('signature-value').textContent = 1000;

        this.updateStatus('🚀 Running Enhanced PAW Demo Animation...');
        this.updateVisualization();

        // Wait for visualization to be created
        setTimeout(() => {
            // Create a sequence of more visible bursts
            let burstCount = 0;
            const totalBursts = 20; // More bursts for better effect
            
            const createDemoBurst = () => {
                if (this.nodes.length === 0) {
                    this.updateStatus('No nodes available for demo');
                    return;
                }

                // Create multiple bursts at once for more dramatic effect
                for (let i = 0; i < 3 && burstCount < totalBursts; i++) {
                    // Pick random positions across the visualization space
                    const angle = Math.random() * Math.PI * 2;
                    const radius = Math.random() * 200 + 50;
                    const height = (Math.random() - 0.5) * 200;
                    
                    const burstPosition = new THREE.Vector3(
                        Math.cos(angle) * radius,
                        height,
                        Math.sin(angle) * radius
                    );

                    // Create burst at random position
                    this.createSimpleParticleBurst(burstPosition);
                    
                    // Sometimes create a secondary burst nearby
                    if (Math.random() > 0.5 && this.createBurstEffect) {
                        const offset = new THREE.Vector3(
                            (Math.random() - 0.5) * 50,
                            (Math.random() - 0.5) * 50,
                            (Math.random() - 0.5) * 50
                        );
                        const secondaryPosition = burstPosition.clone().add(offset);
                        setTimeout(() => this.createBurstEffect(secondaryPosition), 100);
                    }
                    
                    burstCount++;
                }

                if (burstCount < totalBursts) {
                    setTimeout(createDemoBurst, 250); // Continue creating bursts
                } else {
                    setTimeout(() => {
                        this.demoMode = false;
                        this.updateStatus('✨ PAW Demo Complete - Interactive Mode');
                    }, 1000);
                }
            };

            // Start the demo burst sequence
            createDemoBurst();
        }, 500); // Wait for visualization setup
    }

    exportPNG() {
        try {
            // Render the scene to canvas
            this.renderer.render(this.scene, this.camera);

            // Get image data from canvas
            const canvas = document.getElementById('canvas');
            
            // Use toBlob for better performance and error handling
            canvas.toBlob((blob) => {
                if (blob) {
                    const url = URL.createObjectURL(blob);
                    const link = document.createElement('a');
                    link.download = `pat_sim_${Date.now()}.png`;
                    link.href = url;
                    link.click();
                    
                    // Clean up
                    setTimeout(() => URL.revokeObjectURL(url), 100);
                    
                    this.updateStatus('📸 Screenshot exported!');
                } else {
                    this.updateStatus('❌ Export failed - unable to create image');
                }
            }, 'image/png');
        } catch (error) {
            this.updateStatus('❌ Export failed: ' + error.message);
        }
    }

    async loadBenchmarkData() {
        try {
            const response = await fetch('pat_benchmarks.csv');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const csvText = await response.text();
            if (csvText && csvText.length > 0) {
                this.parseBenchmarkCSV(csvText);
                this.updateStatus('✅ Benchmark data loaded');
            } else {
                throw new Error('Empty CSV file');
            }
        } catch (error) {
            console.warn('Failed to load benchmark CSV, using mock data:', error);
            this.mockBenchmarkData();
            this.updateStatus('⚠️ Using demo benchmark data');
        }
        this.updateMetricsDisplay();
    }

    parseBenchmarkCSV(csvText) {
        try {
            const lines = csvText.split('\n').filter(line => line.trim().length > 0);
            if (lines.length < 2) {
                throw new Error('Invalid CSV format');
            }

            const headers = lines[0].split(',').map(h => h.trim());
            this.benchmarkData = {};

            for (let i = 1; i < lines.length; i++) {
                const values = lines[i].split(',').map(v => v.trim());
                if (values.length < headers.length) continue;

                const row = {};
                headers.forEach((header, index) => {
                    row[header] = values[index] || '';
                });

                // Extract relevant data with flexible field names
                const signatures = parseInt(
                    row.Signatures || row.signatures || 
                    row.num_signatures || row.n || '0'
                );
                const strategy = (
                    row.Strategy || row.strategy || 
                    row.aggregation_strategy || 'Unknown'
                ).toLowerCase();

                if (signatures > 0 && strategy !== 'unknown') {
                    const key = `${signatures}_${strategy}`;
                    this.benchmarkData[key] = {
                        compression_ratio: parseFloat(
                            row.Compression_Ratio || row.compression_ratio || 
                            row.compression || '1.0'
                        ),
                        energy_consumption: parseFloat(
                            row.energy_consumption_kwh || row.energy_consumption || 
                            row.energy || '0'
                        ),
                        throughput: parseFloat(
                            row.throughput_sigs_per_sec || row.throughput || 
                            row.sigs_per_sec || '0'
                        ),
                        verify_time: parseFloat(
                            row.Avg_Verify_Time_ms || row.Verify_Time_ms || 
                            row.verify_time_ms || row.verify_time || '0'
                        ),
                        memory_peak: parseFloat(
                            row.Peak_Memory_Aggregation_KB || row.memory_peak_mb || 
                            row.memory_kb || row.memory || '0'
                        )
                    };
                }
            }

            // Loaded benchmark data points
        } catch (error) {
            console.error('CSV parsing error:', error);
            this.mockBenchmarkData();
        }
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
        if (!metricsContent) return;

        const key = `${this.numSignatures}_${this.strategy}`;
        let metrics = this.benchmarkData[key];

        if (!metrics) {
            // Find closest match by signatures first
            const keys = Object.keys(this.benchmarkData);
            let closestKey = null;
            let minDiff = Infinity;

            keys.forEach(k => {
                if (k.includes(`_${this.strategy}`)) {
                    const sig = parseInt(k.split('_')[0]);
                    const diff = Math.abs(sig - this.numSignatures);
                    if (diff < minDiff) {
                        minDiff = diff;
                        closestKey = k;
                    }
                }
            });

            // If no strategy match, find any close signature count
            if (!closestKey && keys.length > 0) {
                closestKey = keys.find(k => k.includes(`_${this.strategy}`)) || keys[0];
            }

            metrics = this.benchmarkData[closestKey];
        }

        if (this.quantumView && this.quantumMetrics) {
            // Display quantum-specific metrics
            const qm = this.quantumMetrics;
            metricsContent.innerHTML = `
                <div class="metric-item">Algorithm: <span class="metric-value">${qm.algorithm}</span></div>
                <div class="metric-item">Qubits: <span class="metric-value">${qm.qubits}</span></div>
                <div class="metric-item">Iterations: <span class="metric-value">${qm.iterations}</span></div>
                <div class="metric-item">Security: <span class="metric-value">${qm.threatLevel}</span></div>
                <div class="metric-item">Q-Advantage: <span class="metric-value">${qm.quantumAdvantage}</span></div>
                ${this.quantumMode === 'grover' ? `
                    <div class="metric-item" style="margin-top: 10px; font-size: 11px; color: #888;">
                        <strong>Grover's Algorithm:</strong> Searching ${Math.pow(2, this.numSignatures)} states<br>
                        Classical: ${Math.pow(2, this.numSignatures)} ops → Quantum: ${Math.ceil(Math.PI/4 * Math.sqrt(Math.pow(2, this.numSignatures)))} ops
                    </div>
                ` : this.quantumMode === 'shor' ? `
                    <div class="metric-item" style="margin-top: 10px; font-size: 11px; color: #888;">
                        <strong>Shor's Algorithm:</strong> Factoring 2048-bit RSA<br>
                        Threat to: RSA, DSA, ECDSA | Safe: Lattice, Hash-based
                    </div>
                ` : `
                    <div class="metric-item" style="margin-top: 10px; font-size: 11px; color: #888;">
                        <strong>Quantum Superposition:</strong> ${this.numSignatures} qubits<br>
                        States: 2^${this.numSignatures} = ${Math.pow(2, Math.min(this.numSignatures, 20)).toLocaleString()}${this.numSignatures > 20 ? '...' : ''}
                    </div>
                `}
            `;
        } else if (metrics) {
            // Display classical PAT metrics
            const compression = metrics.compression_ratio || 1.0;
            const energy = metrics.energy_consumption || 0;
            const throughput = Math.round(metrics.throughput || 1000);
            const verifyTime = (metrics.verify_time || 1.0).toFixed(1);
            const energySaved = Math.max(0, Math.min(100, ((1 - energy) * 100)));

            metricsContent.innerHTML = `
                <div class="metric-item">Compression: <span class="metric-value">${compression.toFixed(1)}x</span></div>
                <div class="metric-item">Energy Saved: <span class="metric-value">${energySaved.toFixed(0)}%</span></div>
                <div class="metric-item">Throughput: <span class="metric-value">${throughput} sigs/sec</span></div>
                <div class="metric-item">Verify Time: <span class="metric-value">${verifyTime}ms</span></div>
                <div class="metric-item" style="margin-top: 10px; font-size: 11px; color: #888;">
                    <strong>PAW Protection:</strong> Post-quantum secure<br>
                    Dilithium3 + Ed25519 hybrid signatures
                </div>
            `;
        } else {
            metricsContent.innerHTML = '<div class="metric-item">Loading performance data...</div>';
        }
        
        // Add extra block for quantum view
        if (this.quantumView) {
            const realStates = Math.pow(2, this.numSignatures);
            const groverQueries = Math.ceil((Math.PI/4) * Math.sqrt(realStates));
            metricsContent.innerHTML += `
                <div class="metric-item" style="color:#ff0088; margin-top:15px;">
                    Grover queries needed: <span class="metric-value">${groverQueries.toExponential(2)}</span><br>
                    Success probability per query: <span class="metric-value">${(1/groverQueries).toExponential(2)}</span>
                </div>`;
        }
    }

    updatePerformanceDisplay(deltaTime) {
        // Update FPS display
        const fpsElement = document.getElementById('fps-value');
        if (fpsElement) {
            fpsElement.textContent = this.fps;
            
            // Color code based on performance
            if (this.fps >= 50) {
                fpsElement.style.color = '#4caf50'; // Green - Good
            } else if (this.fps >= 30) {
                fpsElement.style.color = '#ff9800'; // Orange - OK
            } else {
                fpsElement.style.color = '#f44336'; // Red - Poor
            }
        }

        // Update frame time display
        const msElement = document.getElementById('ms-value');
        if (msElement) {
            const frameTime = deltaTime.toFixed(1);
            msElement.textContent = `${frameTime}ms`;
        }

        // Update memory display
        const memElement = document.getElementById('mem-value');
        if (memElement) {
            if (performance.memory) {
                // Chrome-specific memory API
                const memoryMB = (performance.memory.usedJSHeapSize / 1048576).toFixed(1);
                memElement.textContent = `${memoryMB}MB`;
            } else {
                // Estimate based on node count
                const estimatedMB = (this.nodes.length * 0.1 + 50).toFixed(1);
                memElement.textContent = `~${estimatedMB}MB`;
            }
        }
    }

    updateStatus(message) {
        const statusText = document.getElementById('status-text');
        if (statusText) {
            statusText.textContent = message;
            setTimeout(() => {
                if (document.getElementById('status-text')) {
                    document.getElementById('status-text').textContent = `PAT with PAW Simulator - ${this.numSignatures} signatures`;
                }
            }, 3000);
        }
    }

    hideLoading() {
        setTimeout(() => {
            const loadingDiv = document.getElementById('loading');
            if (loadingDiv) {
                loadingDiv.style.display = 'none';
            }
        }, 1000);
    }
}

// Initialize the simulator when the page loads
document.addEventListener('DOMContentLoaded', () => {
    // Browser compatibility checks
    const checkCompatibility = () => {
        const errors = [];
        
        // Check for WebGL support
        try {
            const canvas = document.createElement('canvas');
            const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
            if (!gl) {
                errors.push('WebGL not supported');
            }
        } catch (e) {
            errors.push('WebGL not supported');
        }
        
        // Check for required browser features
        if (!window.requestAnimationFrame) {
            errors.push('requestAnimationFrame not supported');
        }
        
        if (!window.URL || !window.URL.createObjectURL) {
            errors.push('URL.createObjectURL not supported');
        }
        
        // Check for Canvas blob support
        const canvas = document.createElement('canvas');
        if (!canvas.toBlob) {
            errors.push('Canvas.toBlob not supported');
        }
        
        return errors;
    };
    
    const compatErrors = checkCompatibility();
    
    if (compatErrors.length > 0) {
        const loadingDiv = document.getElementById('loading');
        if (loadingDiv) {
            loadingDiv.innerHTML = `
                <div style="color: #ff0000;">
                    <h3>Browser Compatibility Issues</h3>
                    <p>The following features are not supported in your browser:</p>
                    <ul>
                        ${compatErrors.map(err => `<li>${err}</li>`).join('')}
                    </ul>
                    <p>Please use a modern browser like Chrome, Firefox, Safari, or Edge.</p>
                </div>
            `;
        }
    } else {
        // All checks passed, initialize the simulator
        new PATWebSimulator();
    }
});
