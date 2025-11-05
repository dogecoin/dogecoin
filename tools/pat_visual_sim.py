#!/usr/bin/env python3
"""
PAT Visual Simulator - Interactive Post-Quantum Aggregation Technique Demonstration

This tool provides an interactive visualization of PAT's logarithmic signature aggregation,
quantum attack simulations, and multi-chain performance comparisons using Pygame and Numba.

Features:
- Real-time logarithmic aggregation visualization
- Quantum attack probability simulation
- Multi-chain performance comparison
- Educational tooltips and interactive controls
- Performance monitoring (FPS, energy usage)

Usage: python tools/pat_visual_sim.py
"""

import pygame
import pygame_gui
import numpy as np
import numba
from numba import jit
import math
import time
import psutil
import os
import sys
from enum import Enum
from typing import List, Tuple, Dict, Optional

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.append(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'src'))

from pat.pat_aggregator import AggregationStrategy, ThreatLevel
from pat.extensions.quantum_sims import QuantumAttackSimulator
from pat.extensions.multi_chain import CrossChainBenchmark

# Constants
WINDOW_WIDTH = 1200
WINDOW_HEIGHT = 800
TREE_CENTER_X = WINDOW_WIDTH // 2
TREE_CENTER_Y = 150
MAX_SIGNATURES = 10000
MIN_SIGNATURES = 10

class VisualizationMode(Enum):
    LOGARITHMIC = "logarithmic"
    THRESHOLD = "threshold"
    MERKLE_BATCH = "merkle_batch"
    STACKED_MULTI = "stacked_multi"

class ChainType(Enum):
    DOGECOIN = "dogecoin"
    LITECOIN = "litecoin"
    SOLANA = "solana"

@numba.jit(nopython=True)
def fast_hash_simulation(signatures: np.ndarray, rounds: int = 1000) -> np.ndarray:
    """
    Fast hash simulation using Numba for performance visualization.

    Args:
        signatures: Array of signature data
        rounds: Number of hash rounds to simulate

    Returns:
        Processed signature array
    """
    result = signatures.copy()
    for _ in range(rounds):
        # Simulate SHA-256 style operations
        result = np.bitwise_xor(result, np.roll(result, 1))
        result = (result * 1103515245 + 12345) % (2**32)  # Linear congruential generator
        result = result.astype(np.uint32)
    return result

class PATVisualizer:
    """
    Interactive PAT visualization using Pygame and Numba.
    """

    def __init__(self):
        pygame.init()
        pygame.display.set_caption("PAT Visual Simulator - Post-Quantum Aggregation Technique")

        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        self.clock = pygame.time.Clock()
        self.font = pygame.font.SysFont('Arial', 16)
        self.title_font = pygame.font.SysFont('Arial', 24, bold=True)
        self.tooltip_font = pygame.font.SysFont('Arial', 12)

        # GUI Manager
        self.manager = pygame_gui.UIManager((WINDOW_WIDTH, WINDOW_HEIGHT))

        # Simulation parameters
        self.num_signatures = 100
        self.strategy = VisualizationMode.LOGARITHMIC
        self.threat_level = ThreatLevel.LOW
        self.selected_chain = ChainType.DOGECOIN
        self.show_attacks = True

        # Performance tracking
        self.fps_history = []
        self.energy_usage = 0
        self.start_time = time.time()

        # Initialize simulators
        try:
            self.quantum_sim = QuantumAttackSimulator()
            self.multi_chain = CrossChainBenchmark()
        except ImportError:
            print("Warning: Could not import simulation modules. Using fallback mode.")
            self.quantum_sim = None
            self.multi_chain = None

        self.create_ui()
        self.create_tooltips()

    def create_ui(self):
        """Create the user interface elements."""
        # Signature count slider
        self.slider_rect = pygame.Rect(50, 50, 300, 30)
        self.slider = pygame_gui.elements.UIHorizontalSlider(
            relative_rect=self.slider_rect,
            start_value=self.num_signatures,
            value_range=(MIN_SIGNATURES, MAX_SIGNATURES),
            manager=self.manager
        )
        self.slider_label = pygame_gui.elements.UILabel(
            relative_rect=pygame.Rect(50, 20, 200, 25),
            text=f"Signatures: {self.num_signatures}",
            manager=self.manager
        )

        # Strategy dropdown
        self.strategy_dropdown = pygame_gui.elements.UIDropDownMenu(
            options_list=[mode.value for mode in VisualizationMode],
            starting_option=self.strategy.value,
            relative_rect=pygame.Rect(400, 20, 150, 30),
            manager=self.manager
        )
        self.strategy_label = pygame_gui.elements.UILabel(
            relative_rect=pygame.Rect(400, 50, 150, 25),
            text="Aggregation Strategy",
            manager=self.manager
        )

        # Threat level dropdown
        self.threat_dropdown = pygame_gui.elements.UIDropDownMenu(
            options_list=[level.value for level in ThreatLevel],
            starting_option=self.threat_level.value,
            relative_rect=pygame.Rect(600, 20, 150, 30),
            manager=self.manager
        )
        self.threat_label = pygame_gui.elements.UILabel(
            relative_rect=pygame.Rect(600, 50, 150, 25),
            text="Threat Level",
            manager=self.manager
        )

        # Chain selection buttons
        self.chain_buttons = {}
        chains = [
            (ChainType.DOGECOIN, "🐕", pygame.Rect(800, 20, 80, 30)),
            (ChainType.LITECOIN, "🪙", pygame.Rect(890, 20, 80, 30)),
            (ChainType.SOLANA, "☀️", pygame.Rect(980, 20, 80, 30))
        ]

        for chain_type, emoji, rect in chains:
            button = pygame_gui.elements.UIButton(
                relative_rect=rect,
                text=emoji,
                manager=self.manager
            )
            self.chain_buttons[chain_type] = button

        # Attack toggle
        self.attack_toggle = pygame_gui.elements.UIButton(
            relative_rect=pygame.Rect(50, 700, 150, 30),
            text="Toggle Attacks",
            manager=self.manager
        )

    def create_tooltips(self):
        """Create tooltip data for interactive elements."""
        self.tooltips = {
            'slider': "Adjust number of signatures to aggregate (10-10,000)",
            'strategy': "Choose aggregation strategy: logarithmic (best compression), threshold, merkle_batch, or stacked_multi",
            'threat': "Select quantum threat level: determines hybrid crypto switching",
            'dogecoin': "Dogecoin: Scrypt PoW, 10 TPS baseline, 34k PAT compression",
            'litecoin': "Litecoin: Scrypt PoW, 10 TPS baseline, MWEB privacy integration",
            'solana': "Solana: PoH consensus, 1000 TPS baseline, 10x PAT improvement",
            'attacks': "Toggle visualization of quantum attack probabilities (red highlights)"
        }

    def draw_tree_visualization(self):
        """Draw the logarithmic aggregation tree."""
        if self.strategy == VisualizationMode.LOGARITHMIC:
            self.draw_logarithmic_tree()
        elif self.strategy == VisualizationMode.THRESHOLD:
            self.draw_threshold_tree()
        elif self.strategy == VisualizationMode.MERKLE_BATCH:
            self.draw_merkle_tree()
        else:  # STACKED_MULTI
            self.draw_stacked_tree()

    def draw_logarithmic_tree(self):
        """Draw logarithmic aggregation as a binary tree."""
        # Calculate tree depth
        depth = int(math.log2(self.num_signatures)) + 1
        max_width = 2**(depth - 1)

        # Draw from bottom up (leaves to root)
        y_spacing = 80
        x_scale = min(600, WINDOW_WIDTH - 200) / max_width

        # Generate signature positions
        positions = {}
        for level in range(depth):
            level_size = 2**level
            y = TREE_CENTER_Y + (depth - 1 - level) * y_spacing

            for i in range(level_size):
                if level == 0:  # Leaves
                    total_leaves = min(self.num_signatures, level_size)
                    if i < total_leaves:
                        x = TREE_CENTER_X - (total_leaves - 1) * x_scale / 2 + i * x_scale
                        positions[(level, i)] = (x, y)
                        self.draw_signature_node(x, y, f"S{i}", self.get_attack_color(i))
                else:  # Internal nodes
                    left_child = positions.get((level-1, i*2))
                    right_child = positions.get((level-1, i*2 + 1))

                    if left_child and right_child:
                        x = (left_child[0] + right_child[0]) / 2
                        positions[(level, i)] = (x, y)
                        self.draw_aggregation_node(x, y, f"A{level}_{i}")

                        # Draw connection lines
                        pygame.draw.line(self.screen, (100, 100, 100), left_child, (x, y), 2)
                        pygame.draw.line(self.screen, (100, 100, 100), right_child, (x, y), 2)

    def draw_threshold_tree(self):
        """Draw threshold aggregation visualization."""
        # Simplified threshold visualization
        threshold = max(3, int(math.sqrt(self.num_signatures)))
        groups = self.num_signatures // threshold

        y = TREE_CENTER_Y
        x_spacing = min(400, WINDOW_WIDTH - 200) / max(groups, 1)

        for i in range(groups):
            x = TREE_CENTER_X - (groups - 1) * x_spacing / 2 + i * x_spacing
            self.draw_aggregation_node(x, y, f"T{i}", color=(0, 150, 0))

            # Draw threshold group members
            for j in range(min(threshold, self.num_signatures - i * threshold)):
                member_x = x - (threshold - 1) * 15 / 2 + j * 15
                member_y = y + 60
                self.draw_signature_node(member_x, member_y, f"S{i*threshold+j}",
                                       self.get_attack_color(i*threshold+j))

    def draw_merkle_tree(self):
        """Draw Merkle batch aggregation visualization."""
        # Simplified Merkle tree visualization
        batch_size = min(8, self.num_signatures)
        batches = (self.num_signatures + batch_size - 1) // batch_size

        y = TREE_CENTER_Y
        x_spacing = min(500, WINDOW_WIDTH - 200) / max(batches, 1)

        for i in range(batches):
            x = TREE_CENTER_X - (batches - 1) * x_spacing / 2 + i * x_spacing
            self.draw_aggregation_node(x, y, f"M{i}", color=(0, 0, 150))

            # Draw batch members
            batch_members = min(batch_size, self.num_signatures - i * batch_size)
            for j in range(batch_members):
                member_x = x - (batch_members - 1) * 12 / 2 + j * 12
                member_y = y + 50
                self.draw_signature_node(member_x, member_y, f"S{i*batch_size+j}",
                                       self.get_attack_color(i*batch_size+j))

    def draw_stacked_tree(self):
        """Draw stacked multi aggregation (no compression)."""
        y = TREE_CENTER_Y
        x_spacing = min(800, WINDOW_WIDTH - 200) / max(self.num_signatures, 1)

        for i in range(self.num_signatures):
            x = TREE_CENTER_X - (self.num_signatures - 1) * x_spacing / 2 + i * x_spacing
            self.draw_signature_node(x, y, f"S{i}", self.get_attack_color(i), size=8)

    def draw_signature_node(self, x, y, label, color=(0, 100, 200), size=12):
        """Draw a signature node."""
        pygame.draw.circle(self.screen, color, (int(x), int(y)), size)
        pygame.draw.circle(self.screen, (255, 255, 255), (int(x), int(y)), size, 1)
        text = self.font.render(label, True, (255, 255, 255))
        self.screen.blit(text, (x - text.get_width()//2, y - text.get_height()//2))

    def draw_aggregation_node(self, x, y, label, color=(150, 150, 150)):
        """Draw an aggregation node."""
        rect = pygame.Rect(x - 15, y - 10, 30, 20)
        pygame.draw.rect(self.screen, color, rect)
        pygame.draw.rect(self.screen, (255, 255, 255), rect, 1)
        text = self.font.render(label, True, (255, 255, 255))
        self.screen.blit(text, (x - text.get_width()//2, y - text.get_height()//2))

    def get_attack_color(self, signature_index):
        """Get color based on quantum attack probability."""
        if not self.show_attacks or self.quantum_sim is None:
            return (0, 100, 200)  # Normal blue

        try:
            # Simulate attack probability for this signature
            attack_prob = self.quantum_sim.simulate_grover_attack(self.num_signatures)['success_probability']
            if attack_prob > 1e-10:  # High risk
                return (200, 50, 50)  # Red
            elif attack_prob > 1e-20:  # Medium risk
                return (200, 150, 50)  # Orange
            else:  # Low risk
                return (0, 100, 200)  # Blue
        except:
            return (0, 100, 200)

    def draw_performance_info(self):
        """Draw performance and simulation information."""
        info_y = 600

        # FPS
        fps = self.clock.get_fps()
        self.fps_history.append(fps)
        if len(self.fps_history) > 100:
            self.fps_history.pop(0)
        avg_fps = sum(self.fps_history) / len(self.fps_history)

        fps_text = f"FPS: {fps:.1f} (Avg: {avg_fps:.1f})"
        self.draw_text(fps_text, 50, info_y, (255, 255, 255))

        # Energy usage (simulated)
        current_time = time.time()
        energy_text = f"Simulated Energy: {self.energy_usage:.2f} mWh"
        self.draw_text(energy_text, 50, info_y + 25, (255, 255, 255))

        # Strategy info
        strategy_info = f"Strategy: {self.strategy.value} | Signatures: {self.num_signatures}"
        self.draw_text(strategy_info, 50, info_y + 50, (255, 255, 255))

        # Chain info
        if self.multi_chain:
            try:
                chain_data = self.multi_chain.get_chain_data(self.selected_chain.value)
                chain_info = f"{self.selected_chain.value.title()}: {chain_data.get('tps', 'N/A')} TPS baseline"
                self.draw_text(chain_info, 50, info_y + 75, (255, 255, 255))
            except:
                chain_info = f"{self.selected_chain.value.title()}: Data unavailable"
                self.draw_text(chain_info, 50, info_y + 75, (255, 255, 255))

    def draw_text(self, text, x, y, color=(255, 255, 255)):
        """Draw text on screen."""
        surface = self.font.render(text, True, color)
        self.screen.blit(surface, (x, y))

    def draw_title(self):
        """Draw the main title."""
        title = "PAT Visual Simulator - Post-Quantum Aggregation Technique"
        title_surface = self.title_font.render(title, True, (255, 255, 0))
        self.screen.blit(title_surface, (WINDOW_WIDTH//2 - title_surface.get_width()//2, 10))

    def show_tooltip(self, mouse_pos, tooltip_key):
        """Show tooltip for hovered element."""
        if tooltip_key in self.tooltips:
            tooltip_text = self.tooltips[tooltip_key]
            tooltip_surface = self.tooltip_font.render(tooltip_text, True, (0, 0, 0))
            tooltip_bg = pygame.Rect(mouse_pos[0] + 10, mouse_pos[1] - 30,
                                   tooltip_surface.get_width() + 10, tooltip_surface.get_height() + 6)
            pygame.draw.rect(self.screen, (255, 255, 200), tooltip_bg)
            pygame.draw.rect(self.screen, (0, 0, 0), tooltip_bg, 1)
            self.screen.blit(tooltip_surface, (mouse_pos[0] + 15, mouse_pos[1] - 27))

    def run_simulation(self):
        """Run the main simulation loop."""
        running = True
        tooltip_active = None

        while running:
            time_delta = self.clock.tick(60) / 1000.0

            mouse_pos = pygame.mouse.get_pos()

            # Check for tooltips
            tooltip_active = None
            if self.slider_rect.collidepoint(mouse_pos):
                tooltip_active = 'slider'
            elif self.strategy_dropdown.rect.collidepoint(mouse_pos):
                tooltip_active = 'strategy'
            elif self.threat_dropdown.rect.collidepoint(mouse_pos):
                tooltip_active = 'threat'
            elif self.attack_toggle.rect.collidepoint(mouse_pos):
                tooltip_active = 'attacks'
            else:
                for chain_type, button in self.chain_buttons.items():
                    if button.rect.collidepoint(mouse_pos):
                        tooltip_active = chain_type.value
                        break

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

                self.manager.process_events(event)

                # Handle slider changes
                if event.type == pygame_gui.UI_HORIZONTAL_SLIDER_MOVED:
                    if event.ui_element == self.slider:
                        self.num_signatures = int(event.value)
                        self.slider_label.set_text(f"Signatures: {self.num_signatures}")

                # Handle dropdown changes
                if event.type == pygame_gui.UI_DROP_DOWN_MENU_CHANGED:
                    if event.ui_element == self.strategy_dropdown:
                        self.strategy = VisualizationMode(event.text)
                    elif event.ui_element == self.threat_dropdown:
                        self.threat_level = ThreatLevel(event.text)

                # Handle button presses
                if event.type == pygame_gui.UI_BUTTON_PRESSED:
                    for chain_type, button in self.chain_buttons.items():
                        if event.ui_element == button:
                            self.selected_chain = chain_type
                            break
                    if event.ui_element == self.attack_toggle:
                        self.show_attacks = not self.show_attacks

            # Update energy usage (simulated)
            self.energy_usage += time_delta * 0.001  # Simulated energy consumption

            # Clear screen
            self.screen.fill((20, 20, 40))

            # Draw components
            self.draw_title()
            self.draw_tree_visualization()
            self.draw_performance_info()

            # Draw tooltip if active
            if tooltip_active:
                self.show_tooltip(mouse_pos, tooltip_active)

            # Update GUI
            self.manager.update(time_delta)
            self.manager.draw_ui(self.screen)

            pygame.display.flip()

        pygame.quit()

def main():
    """Main entry point."""
    print("🚀 Starting PAT Visual Simulator...")
    print("Controls:")
    print("  - Slider: Adjust number of signatures (10-10,000)")
    print("  - Strategy dropdown: Choose aggregation method")
    print("  - Threat dropdown: Select quantum threat level")
    print("  - Chain buttons: Select blockchain for TPS comparison")
    print("  - Toggle Attacks: Show/hide quantum attack visualization")
    print("  - Hover for tooltips with educational information")
    print()

    visualizer = PATVisualizer()
    visualizer.run_simulation()

if __name__ == "__main__":
    main()
