#!/usr/bin/env python3
"""
Unit Tests for Multi-Chain PAT Integration

Tests cross-chain functionality with mocked RPC calls and network isolation.
Focuses on CrossChainBenchmark class and multi-chain interoperability.

Coverage Goals:
- CrossChainBenchmark.benchmark_all_chains() returns expected dict keys
- Fee savings analysis with simulated >0 savings
- RPC call mocking for all chain integrators
- Error handling and edge cases
- Multi-chain signature aggregation

Run with: python -m unittest tests.test_multi_chain
"""

import unittest
import sys
import os
from unittest.mock import Mock, patch, MagicMock, mock_open
from typing import Dict, Any

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from pat.extensions.multi_chain import (
    CrossChainBenchmark, LitecoinIntegrator, SolanaIntegrator,
    EthereumIntegrator, RPCClient, ChainType, PatError
)


class TestCrossChainBenchmark(unittest.TestCase):
    """Comprehensive tests for CrossChainBenchmark class."""

    def setUp(self) -> None:
        """Set up test fixtures before each test method."""
        self.benchmark = CrossChainBenchmark()

    def tearDown(self) -> None:
        """Clean up test fixtures after each test method."""
        pass

    @patch('pat.extensions.multi_chain.LitecoinIntegrator.simulate_pat_transaction')
    @patch('pat.extensions.multi_chain.SolanaIntegrator.simulate_svm_batch')
    @patch('pat.extensions.multi_chain.EthereumIntegrator.estimate_pat_contract_gas')
    def test_benchmark_all_chains_returns_expected_keys(self,
                                                        mock_eth_gas: Mock,
                                                        mock_sol_batch: Mock,
                                                        mock_ltc_tx: Mock) -> None:
        """Test benchmark_all_chains returns dict with expected chain keys."""
        # Mock Litecoin results
        mock_ltc_tx.side_effect = [
            {"compression_ratio": 10.5, "txid": "ltc_tx_10", "fee": 0.001},
            {"compression_ratio": 50.2, "txid": "ltc_tx_50", "fee": 0.001}
        ]

        # Mock Solana results
        mock_sol_batch.return_value = {
            "tps": 1500.0,
            "compression_ratio": 25.0,
            "fee_per_tx_sol": 0.000005,
            "total_fee_sol": 0.05
        }

        # Mock Ethereum results
        mock_eth_gas.side_effect = [
            {"total_cost_eth": 0.001, "total_gas": 21000},
            {"total_cost_eth": 0.005, "total_gas": 105000}
        ]

        # Run benchmark
        results = self.benchmark.benchmark_all_chains([10, 50])

        # Verify structure
        self.assertIsInstance(results, dict)
        self.assertIn("litecoin", results)
        self.assertIn("solana", results)
        self.assertIn("ethereum", results)

        # Verify Litecoin results
        self.assertIsInstance(results["litecoin"], dict)
        self.assertIn(10, results["litecoin"])
        self.assertIn(50, results["litecoin"])
        self.assertEqual(results["litecoin"][10]["compression_ratio"], 10.5)

        # Verify Solana results
        self.assertIsInstance(results["solana"], dict)
        self.assertEqual(results["solana"]["tps"], 1500.0)
        self.assertEqual(results["solana"]["compression_ratio"], 25.0)

        # Verify Ethereum results
        self.assertIsInstance(results["ethereum"], dict)
        self.assertIn(10, results["ethereum"])
        self.assertIn(50, results["ethereum"])
        self.assertEqual(results["ethereum"][10]["total_cost_eth"], 0.001)

    @patch('pat.extensions.multi_chain.LitecoinIntegrator.simulate_pat_transaction')
    @patch('pat.extensions.multi_chain.SolanaIntegrator.simulate_svm_batch')
    @patch('pat.extensions.multi_chain.EthereumIntegrator.estimate_pat_contract_gas')
    def test_analyze_fee_savings_positive_savings(self,
                                                 mock_eth_gas: Mock,
                                                 mock_sol_batch: Mock,
                                                 mock_ltc_tx: Mock) -> None:
        """Test fee savings analysis produces positive savings."""
        # Mock benchmark results
        mock_ltc_tx.return_value = {"compression_ratio": 20.0}
        mock_sol_batch.return_value = {
            "tps": 1000.0,
            "fee_per_tx_sol": 0.00001,
            "total_fee_sol": 0.1
        }
        mock_eth_gas.return_value = {"total_cost_eth": 0.01, "total_gas": 100000}

        # Get benchmark results
        benchmark_results = self.benchmark.benchmark_all_chains([100])

        # Analyze fee savings
        savings = self.benchmark.analyze_fee_savings(benchmark_results)

        # Verify savings structure
        self.assertIsInstance(savings, dict)
        self.assertIn("litecoin", savings)
        self.assertIn("solana", savings)
        self.assertIn("ethereum", savings)

        # Verify positive savings
        for chain, data in savings.items():
            if "savings_percent" in data:
                self.assertGreater(data["savings_percent"], 0.0,
                                 f"{chain} should show positive fee savings")

    @patch('pat.extensions.multi_chain.LitecoinIntegrator.simulate_pat_transaction')
    def test_benchmark_handles_litecoin_errors_gracefully(self, mock_ltc_tx: Mock) -> None:
        """Test benchmark handles Litecoin errors gracefully."""
        # Mock Litecoin failure
        mock_ltc_tx.side_effect = PatError("RPC connection failed")

        # Should still complete other benchmarks
        results = self.benchmark.benchmark_all_chains([10])

        self.assertIn("litecoin", results)
        self.assertIn("error", results["litecoin"][10])

    @patch('pat.extensions.multi_chain.SolanaIntegrator.simulate_svm_batch')
    def test_benchmark_handles_solana_errors_gracefully(self, mock_sol_batch: Mock) -> None:
        """Test benchmark handles Solana errors gracefully."""
        # Mock Solana failure
        mock_sol_batch.side_effect = Exception("RPC timeout")

        results = self.benchmark.benchmark_all_chains([10])

        self.assertIn("solana", results)
        self.assertIn("error", results["solana"])

    @patch('pat.extensions.multi_chain.EthereumIntegrator.estimate_pat_contract_gas')
    def test_benchmark_handles_ethereum_errors_gracefully(self, mock_eth_gas: Mock) -> None:
        """Test benchmark handles Ethereum errors gracefully."""
        # Mock Ethereum failure
        mock_eth_gas.side_effect = PatError("Gas estimation failed")

        results = self.benchmark.benchmark_all_chains([10])

        self.assertIn("ethereum", results)
        self.assertIn("error", results["ethereum"][10])

    def test_benchmark_default_signature_counts(self) -> None:
        """Test benchmark uses default signature counts when none provided."""
        # Patch all integrators to avoid actual network calls
        with patch.object(self.benchmark.litecoin, 'simulate_pat_transaction') as mock_ltc, \
             patch.object(self.benchmark.solana, 'simulate_svm_batch') as mock_sol, \
             patch.object(self.benchmark.ethereum, 'estimate_pat_contract_gas') as mock_eth:

            mock_ltc.return_value = {"compression_ratio": 1.0}
            mock_sol.return_value = {"tps": 100.0}
            mock_eth.return_value = {"total_cost_eth": 0.001}

            results = self.benchmark.benchmark_all_chains()

            # Should have tested default counts: [10, 100, 1000]
            self.assertEqual(mock_ltc.call_count, 3)  # Called for 10, 100, 1000
            self.assertEqual(mock_eth.call_count, 3)  # Called for 10, 100, 1000
            self.assertEqual(mock_sol.call_count, 1)  # Called once for batch processing

    @patch('pat.extensions.multi_chain.requests.post')
    def test_solana_rpc_slot_call(self, mock_post: Mock) -> None:
        """Test Solana RPC slot retrieval."""
        # Mock successful response
        mock_response = Mock()
        mock_response.raise_for_status.return_value = None
        mock_response.json.return_value = {"result": 12345}
        mock_post.return_value = mock_response

        solana = SolanaIntegrator(network="devnet")
        slot = solana.get_slot()

        self.assertEqual(slot, 12345)
        mock_post.assert_called_once()

    @patch('pat.extensions.multi_chain.requests.post')
    def test_solana_rpc_error_handling(self, mock_post: Mock) -> None:
        """Test Solana RPC error handling."""
        # Mock failed response
        mock_post.side_effect = Exception("Connection failed")

        solana = SolanaIntegrator(network="devnet")
        slot = solana.get_slot()

        # Should return 0 on error
        self.assertEqual(slot, 0)

    @patch('pat.extensions.multi_chain.subprocess.run')
    def test_litecoin_cli_fallback(self, mock_subprocess: Mock) -> None:
        """Test Litecoin CLI fallback when RPC fails."""
        # Mock CLI success
        mock_result = Mock()
        mock_result.returncode = 0
        mock_result.stdout = '{"blocks": 12345}'
        mock_subprocess.run.return_value = mock_result

        litecoin = LitecoinIntegrator(testnet=True)

        # Mock RPC failure
        with patch.object(litecoin.rpc, 'call_cli') as mock_cli:
            mock_cli.side_effect = [PatError("CLI failed", "NETWORK_ERROR")]
            with patch.object(litecoin.rpc, 'call_rpc') as mock_rpc:
                mock_rpc.return_value = {"blocks": 12345}

                result = litecoin.get_blockchain_info()
                self.assertEqual(result["blocks"], 12345)
                mock_rpc.assert_called_once()

    @patch('pat.extensions.multi_chain.requests.post')
    def test_ethereum_gas_price_fallback(self, mock_post: Mock) -> None:
        """Test Ethereum gas price fallback on error."""
        ethereum = EthereumIntegrator(network="sepolia")

        # Mock web3 gas price failure
        with patch.object(ethereum, 'web3', None):
            gas_price = ethereum.get_gas_price()
            # Should return fallback value
            self.assertEqual(gas_price, 20000000000)  # 20 gwei

    def test_analyze_fee_savings_empty_results(self) -> None:
        """Test fee savings analysis handles empty results."""
        savings = self.benchmark.analyze_fee_savings({})

        # Should return empty dict
        self.assertEqual(savings, {})

    def test_analyze_fee_savings_missing_chain_data(self) -> None:
        """Test fee savings analysis handles missing chain data."""
        results = {"litecoin": {"error": "Connection failed"}}
        savings = self.benchmark.analyze_fee_savings(results)

        # Should not crash, may return partial results
        self.assertIsInstance(savings, dict)

    @patch('pat.extensions.multi_chain.LitecoinIntegrator.simulate_pat_transaction')
    @patch('pat.extensions.multi_chain.SolanaIntegrator.simulate_svm_batch')
    @patch('pat.extensions.multi_chain.EthereumIntegrator.estimate_pat_contract_gas')
    def test_cross_chain_compression_comparison(self,
                                                mock_eth_gas: Mock,
                                                mock_sol_batch: Mock,
                                                mock_ltc_tx: Mock) -> None:
        """Test cross-chain compression ratios are reasonable."""
        # Mock realistic compression ratios
        mock_ltc_tx.return_value = {"compression_ratio": 15.2}
        mock_sol_batch.return_value = {
            "compression_ratio": 25.0,
            "tps": 1000.0,
            "fee_per_tx_sol": 0.00001
        }
        mock_eth_gas.return_value = {"total_gas": 50000, "total_cost_eth": 0.001}

        results = self.benchmark.benchmark_all_chains([50])

        # All chains should show meaningful compression
        ltc_compression = results["litecoin"][50]["compression_ratio"]
        sol_compression = results["solana"]["compression_ratio"]

        self.assertGreater(ltc_compression, 10.0, "Litecoin should show good compression")
        self.assertGreater(sol_compression, 20.0, "Solana should show batch compression")

    def test_integrator_initialization_parameters(self) -> None:
        """Test integrator initialization with various parameters."""
        # Test Litecoin testnet
        ltc_testnet = LitecoinIntegrator(testnet=True)
        self.assertTrue(ltc_testnet.testnet)
        self.assertEqual(ltc_testnet.rpc_port, 19332)

        # Test Solana devnet
        sol_devnet = SolanaIntegrator(network="devnet")
        self.assertEqual(sol_devnet.network, "devnet")
        self.assertIn("devnet", sol_devnet.rpc_url)

        # Test Ethereum sepolia
        eth_sepolia = EthereumIntegrator(network="sepolia")
        self.assertEqual(eth_sepolia.network, "sepolia")

    def test_rpc_client_initialization(self) -> None:
        """Test RPC client initialization and configuration."""
        rpc = RPCClient(ChainType.LITECOIN, rpc_port=19332, cli_path="litecoin-cli")

        self.assertEqual(rpc.chain_type, ChainType.LITECOIN)
        self.assertEqual(rpc.rpc_port, 19332)
        self.assertEqual(rpc.cli_path, "litecoin-cli")
        self.assertEqual(rpc.rpc_id, 1)

    @patch('pat.extensions.multi_chain.requests.post')
    def test_rpc_client_call_rpc_success(self, mock_post: Mock) -> None:
        """Test RPC client successful call."""
        mock_response = Mock()
        mock_response.raise_for_status.return_value = None
        mock_response.json.return_value = {"result": "success"}
        mock_post.return_value = mock_response

        rpc = RPCClient(ChainType.BITCOIN)
        result = rpc.call_rpc("getblockcount")

        self.assertEqual(result["result"], "success")
        self.assertEqual(rpc.rpc_id, 2)  # Should increment

    @patch('pat.extensions.multi_chain.requests.post')
    def test_rpc_client_call_rpc_timeout(self, mock_post: Mock) -> None:
        """Test RPC client timeout handling."""
        # Mock requests.post to raise timeout exception
        from requests.exceptions import Timeout
        mock_post.side_effect = Timeout("Connection timed out")

        rpc = RPCClient(ChainType.BITCOIN)

        # Should raise PatError (which wraps the underlying exception)
        with self.assertRaises(PatError):
            rpc.call_rpc("getblockcount")


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)
