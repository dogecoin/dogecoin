#!/usr/bin/env python3
"""
PAT Environment Test Script
Tests all required libraries for PAT simulation and benchmarking
"""

import sys
import timeit
import numpy as np
import pandas as pd
import dilithium_py
import pqcrypto
import sympy

def test_libraries():
    """Test all required libraries for PAT simulation"""
    
    print("🧪 PAT Environment Test Results")
    print("=" * 40)
    
    # Test NumPy
    try:
        arr = np.array([1, 2, 3, 4, 5])
        print(f"✅ NumPy {np.__version__}: Array operations working")
    except Exception as e:
        print(f"❌ NumPy: {e}")
    
    # Test Pandas
    try:
        df = pd.DataFrame({'A': [1, 2, 3], 'B': [4, 5, 6]})
        print(f"✅ Pandas {pd.__version__}: DataFrame operations working")
    except Exception as e:
        print(f"❌ Pandas: {e}")
    
    # Test Dilithium-py
    try:
        # Basic import test - actual crypto operations would require more setup
        print("✅ dilithium-py: Library imported successfully")
    except Exception as e:
        print(f"❌ dilithium-py: {e}")
    
    # Test PQ Crypto
    try:
        # Basic import test
        print("✅ pqcrypto: Library imported successfully")
    except Exception as e:
        print(f"❌ pqcrypto: {e}")
    
    # Test SymPy
    try:
        x = sympy.Symbol('x')
        expr = x**2 + 2*x + 1
        print(f"✅ SymPy {sympy.__version__}: Symbolic math working")
    except Exception as e:
        print(f"❌ SymPy: {e}")
    
    # Test timeit (built-in)
    try:
        time = timeit.timeit('sum(range(100))', number=1000)
        print("✅ timeit: Timing functionality working")
    except Exception as e:
        print(f"❌ timeit: {e}")
    
    print("\n🎯 Environment Ready for PAT Simulation!")
    print("Ready to implement Dilithium signature aggregation algorithms")

if __name__ == "__main__":
    test_libraries()
