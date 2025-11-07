#!/usr/bin/env python3
"""
Dogecoin Testnet Integration Test Script
=========================================

This script tests the TestnetIntegrator class and dogecoin-cli connectivity.
Run this to verify your Dogecoin testnet setup before running full PAT benchmarks.

Usage:
    cd pat/src
    python test_dogecoin_integration.py

Requirements:
- dogecoin-cli installed and in PATH
- dogecoin.conf configured for testnet
- dogecoind running (optional for full test)
"""

import sys
import os

# Add the current directory to Python path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    from pat_benchmark import TestnetIntegrator, PATAggregator, AggregationStrategy
except ImportError as e:
    print(f"❌ Failed to import PAT modules: {e}")
    print("Make sure you're running this from the pat/src directory")
    sys.exit(1)

def test_basic_connection():
    """Test basic dogecoin-cli connectivity"""
    print("🐕 DOGECOIN TESTNET INTEGRATION TEST")
    print("=" * 50)

    # Initialize testnet integrator
    testnet = TestnetIntegrator()

    # Run connection tests
    results = testnet.test_dogecoin_connection()

    print("\n" + "=" * 50)
    if results["errors"]:
        print("❌ CONNECTION TEST FAILED")
        print("To fix these issues:")
        print("1. Install dogecoin-cli: make install (from project root)")
        print("2. Start dogecoind: dogecoind -testnet -daemon")
        print("3. Wait for blockchain sync to complete")
        return False
    else:
        print("✅ CONNECTION TEST PASSED")
        return True

def test_pat_transaction_simulation():
    """Test PAT transaction creation and simulation"""
    print("\n🐾 TESTING PAT TRANSACTION SIMULATION")
    print("=" * 50)

    try:
        # Initialize components
        testnet = TestnetIntegrator()
        aggregator = PATAggregator()

        # Create a test message
        message = b"PAT Integration Test Message"

        # Test PAT transaction creation with 10 signatures as requested
        print("Creating PAT transaction with 10 signatures (as requested)...")
        result = testnet.create_pat_transaction(aggregator, message, num_signatures=10)

        if result["success"]:
            print("✅ PAT transaction simulation successful!")
            details = result["details"]
            print(f"   📊 Compression ratio: {details['compression_ratio']:.1f}x")
            print(f"   ⏱️  Total time: {details['total_time']:.3f}s")
            print(f"   📏 PAT signature size: {details['pat_signature_size']} bytes")
            print(f"   📏 Individual signature size: {details['individual_signature_size']} bytes")
            print(f"   🏷️  Mock TXID: {details['txid']}")
            print(f"   📡 Broadcast time: {details['broadcast_time']:.3f}s (simulated)")
            return True
        else:
            print(f"❌ PAT transaction failed: {result['error']}")
            return False

    except Exception as e:
        print(f"❌ PAT test failed with exception: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    """Run all integration tests"""
    print("Testing Dogecoin testnet integration for PAT signatures...\n")

    # Test 1: Basic connection
    connection_ok = test_basic_connection()

    if not connection_ok:
        print("\n⚠️  Basic connection tests failed.")
        print("Some tests may still work in simulation mode.")
        print("For full testnet integration, ensure dogecoind is running.")

    # Test 2: PAT simulation (always works)
    pat_ok = test_pat_transaction_simulation()

    print("\n" + "=" * 50)
    print("📊 FINAL TEST RESULTS:")
    print(f"   Basic Connection: {'✅ PASS' if connection_ok else '❌ FAIL'}")
    print(f"   PAT Simulation: {'✅ PASS' if pat_ok else '❌ FAIL'}")

    if connection_ok and pat_ok:
        print("\n🎉 ALL TESTS PASSED! Ready for PAT benchmarking.")
        print("Run: python pat_benchmark.py --benchmark integration")
    elif pat_ok:
        print("\n⚠️  PAT simulation works, but testnet connection failed.")
        print("PAT benchmarks will work in simulation mode only.")
        print("For real testnet testing, fix the connection issues above.")
    else:
        print("\n❌ CRITICAL FAILURE: PAT simulation failed.")
        print("Check the error messages above.")

    return 0 if pat_ok else 1

if __name__ == "__main__":
    sys.exit(main())
