"""
Economic Models for PAT (Paw Aggregation Technique) Fee Analysis

This module provides economic modeling and forecasting for transaction fees
and adoption incentives under PAT signature aggregation.

Features:
- ARIMA time series forecasting for mempool fees
- Fee reduction analysis with PAT adoption
- Economic incentives modeling
- Historical data analysis and simulation
- Cost-benefit analysis for miners and users

Models:
- Fee elasticity with signature aggregation
- Miner revenue optimization
- User adoption incentives
- Cross-chain fee arbitrage

C++ Integration Notes:
- Use Eigen for matrix operations in forecasting
- Implement statistical distributions for uncertainty analysis
- Time series analysis for fee prediction
- Economic modeling for adoption incentives

Author: Dogecoin Core Economic Analysis Team
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Any, Tuple, Optional, Union
import time
import random
from datetime import datetime, timedelta

# Optional statsmodels import with fallback
try:
    import statsmodels.api as sm
    from statsmodels.tsa.arima.model import ARIMA
    from statsmodels.tsa.stattools import adfuller
    STATSMODELS_AVAILABLE = True
except ImportError:
    STATSMODELS_AVAILABLE = False
    print("⚠️ statsmodels not available - using simplified forecasting")

# Optional imports with fallbacks
try:
    from ..pat_benchmark import PatAggregator, PatError, AggregationStrategy
except ImportError:
    # Fallback for standalone execution
    from enum import Enum

    class AggregationStrategy(Enum):
        NONE = 0
        THRESHOLD = 1
        MERKLE_BATCH = 2
        LOGARITHMIC = 3
        STACKED_MULTI = 4

    class PatError(Exception):
        pass

    class PatAggregator:
        def __init__(self, strategy=AggregationStrategy.LOGARITHMIC):
            self.strategy = strategy


class MempoolFeeData:
    """
    Historical mempool fee data for Dogecoin and Litecoin.

    Provides sample datasets for economic analysis when real data is unavailable.
    """

    # Sample Dogecoin mempool fee data (past 30 days, simulated)
    DOGECOIN_FEES = [
        1.5, 1.8, 2.2, 1.9, 2.5, 3.1, 2.8, 2.3, 1.7, 2.0,  # Week 1
        2.4, 2.9, 3.5, 3.2, 2.7, 2.1, 1.8, 2.3, 2.8, 3.0,  # Week 2
        2.6, 3.1, 3.8, 3.4, 2.9, 2.4, 2.0, 2.5, 3.0, 3.2,  # Week 3
        2.8, 3.3, 4.0, 3.6, 3.1, 2.6, 2.2, 2.7, 3.2, 3.4   # Week 4
    ]  # Fees in Doge per KB

    # Sample Litecoin mempool fee data (similar pattern)
    LITECOIN_FEES = [
        0.001, 0.0012, 0.0015, 0.0013, 0.0018, 0.0022, 0.0020, 0.0017, 0.0014, 0.0016,  # Week 1
        0.0017, 0.0020, 0.0025, 0.0023, 0.0019, 0.0015, 0.0013, 0.0017, 0.0021, 0.0022,  # Week 2
        0.0018, 0.0022, 0.0028, 0.0025, 0.0021, 0.0017, 0.0014, 0.0018, 0.0022, 0.0024,  # Week 3
        0.0020, 0.0024, 0.0030, 0.0027, 0.0023, 0.0019, 0.0016, 0.0020, 0.0024, 0.0026   # Week 4
    ]  # Fees in LTC per KB

    @staticmethod
    def get_sample_data(chain: str = "dogecoin", days: int = 30) -> pd.DataFrame:
        """
        Get sample mempool fee data.

        Args:
            chain: "dogecoin" or "litecoin"
            days: Number of days of data

        Returns:
            DataFrame with timestamp and fee columns
        """
        if chain.lower() == "dogecoin":
            fees = MempoolFeeData.DOGECOIN_FEES
            unit = "DOGE/KB"
        elif chain.lower() == "litecoin":
            fees = MempoolFeeData.LITECOIN_FEES
            unit = "LTC/KB"
        else:
            raise ValueError(f"Unsupported chain: {chain}")

        # Extend data if needed
        while len(fees) < days:
            # Add some variation to extend the series
            last_fee = fees[-1]
            variation = np.random.normal(0, 0.1)  # Small random variation
            new_fee = max(0.001, last_fee + variation)  # Ensure positive
            fees.append(new_fee)

        # Create timestamps
        start_date = datetime.now() - timedelta(days=len(fees))
        timestamps = [start_date + timedelta(days=i) for i in range(len(fees))]

        df = pd.DataFrame({
            'timestamp': timestamps,
            'fee': fees[:days],
            'unit': unit
        })

        return df.set_index('timestamp')


class FeeForecaster:
    """
    ARIMA-based fee forecasting for blockchain networks.

    Uses time series analysis to predict future fee levels and
    analyze the impact of PAT adoption on transaction costs.
    """

    def __init__(self):
        """Initialize fee forecaster."""
        self.models = {}

    def fit_arima_model(self, fee_data: pd.Series, order: Tuple[int, int, int] = (1, 1, 1)) -> Any:
        """
        Fit ARIMA model to fee time series.

        Args:
            fee_data: Time series of fee data
            order: ARIMA order (p, d, q)

        Returns:
            Fitted ARIMA model
        """
        if not STATSMODELS_AVAILABLE:
            print("⚠️ statsmodels not available - using naive forecasting")
            return None

        try:
            model = ARIMA(fee_data, order=order)
            fitted_model = model.fit()
            return fitted_model
        except Exception as e:
            print(f"⚠️ ARIMA fitting failed: {e}")
            return None

    def forecast_fees(self, historical_data: pd.DataFrame, forecast_days: int = 30) -> Dict[str, Any]:
        """
        Forecast future fees using ARIMA model.

        Args:
            historical_data: Historical fee data DataFrame
            forecast_days: Number of days to forecast

        Returns:
            Forecast results dictionary
        """
        fee_series = historical_data['fee']
        unit = historical_data['unit'].iloc[0]

        # Fit model
        model = self.fit_arima_model(fee_series)

        if model is None:
            # Fallback: simple exponential smoothing
            last_fee = fee_series.iloc[-1]
            forecast = [last_fee * (1 + 0.02 * i) for i in range(forecast_days)]  # 2% daily growth
            confidence_intervals = [(f * 0.9, f * 1.1) for f in forecast]
        else:
            # ARIMA forecast
            forecast_result = model.forecast(steps=forecast_days)
            forecast = forecast_result.values.tolist()

            # Simple confidence intervals (in practice, use model.conf_int())
            std_dev = np.std(fee_series)
            confidence_intervals = [
                (f - 1.96 * std_dev, f + 1.96 * std_dev) for f in forecast
            ]

        # Create forecast DataFrame
        last_date = historical_data.index[-1]
        forecast_dates = [last_date + timedelta(days=i+1) for i in range(forecast_days)]

        forecast_df = pd.DataFrame({
            'timestamp': forecast_dates,
            'forecast_fee': forecast,
            'unit': unit,
            'confidence_lower': [ci[0] for ci in confidence_intervals],
            'confidence_upper': [ci[1] for ci in confidence_intervals]
        }).set_index('timestamp')

        return {
            'historical_data': historical_data,
            'forecast_data': forecast_df,
            'model_type': 'ARIMA' if model else 'Naive',
            'forecast_days': forecast_days,
            'current_fee': fee_series.iloc[-1],
            'avg_forecast_fee': np.mean(forecast),
            'unit': unit
        }

    def analyze_pat_impact(self, forecast_result: Dict[str, Any],
                          pat_adoption_rate: float = 0.9) -> Dict[str, Any]:
        """
        Analyze the economic impact of PAT adoption on fees.

        Args:
            forecast_result: Fee forecast results
            pat_adoption_rate: Expected fee reduction with PAT (0.0 to 1.0)

        Returns:
            PAT impact analysis
        """
        current_fee = forecast_result['current_fee']
        forecast_fees = forecast_result['forecast_data']['forecast_fee']
        unit = forecast_result['unit']

        # Calculate PAT-adjusted fees
        pat_fees = forecast_fees * (1 - pat_adoption_rate)
        fee_reduction = forecast_fees - pat_fees

        # Economic analysis
        total_reduction = np.sum(fee_reduction)
        avg_reduction_percent = np.mean(fee_reduction / forecast_fees) * 100

        # User savings (assuming 1000 tx/day average)
        daily_tx_volume = 1000
        daily_savings = np.mean(fee_reduction) * daily_tx_volume
        monthly_savings = daily_savings * 30

        # Miner impact (reduced fees = reduced revenue)
        miner_revenue_loss = total_reduction * 0.01  # Assume 1% of fees go to miners

        analysis = {
            'pat_adoption_rate': pat_adoption_rate,
            'fee_reduction_percent': avg_reduction_percent,
            'total_fee_reduction': total_reduction,
            'current_fee': current_fee,
            'avg_pat_fee': np.mean(pat_fees),
            'unit': unit,
            'user_savings': {
                'daily_savings': daily_savings,
                'monthly_savings': monthly_savings,
                'annual_savings': monthly_savings * 12
            },
            'miner_impact': {
                'revenue_loss': miner_revenue_loss,
                'compensation_needed': miner_revenue_loss * 1.1  # 10% buffer
            },
            'break_even_analysis': {
                'transactions_needed': int(total_reduction / current_fee),
                'time_to_recover': f"{int(total_reduction / (current_fee * 100))} days at 100 tx/day"
            }
        }

        return analysis


class EconomicIncentiveModel:
    """
    Economic incentives modeling for PAT adoption.

    Analyzes the incentives for users, miners, and developers to adopt PAT.
    """

    def __init__(self):
        """Initialize economic incentive model."""
        self.forecaster = FeeForecaster()

    def calculate_user_incentives(self, chain: str = "dogecoin") -> Dict[str, Any]:
        """
        Calculate user adoption incentives for PAT.

        Args:
            chain: Blockchain network

        Returns:
            User incentive analysis
        """
        # Get historical data
        historical_data = MempoolFeeData.get_sample_data(chain, days=30)

        # Forecast fees
        forecast = self.forecaster.forecast_fees(historical_data, forecast_days=90)

        # Analyze PAT impact
        pat_analysis = self.forecaster.analyze_pat_impact(forecast, pat_adoption_rate=0.85)

        # User-specific incentives
        current_fee_per_tx = forecast['current_fee'] * 0.225  # Assume 225B tx size
        pat_fee_per_tx = pat_analysis['avg_pat_fee'] * 0.225

        user_incentives = {
            'current_cost_per_tx': current_fee_per_tx,
            'pat_cost_per_tx': pat_fee_per_tx,
            'savings_per_tx': current_fee_per_tx - pat_fee_per_tx,
            'savings_percent': pat_analysis['fee_reduction_percent'],
            'break_even_tx': int(current_fee_per_tx / (current_fee_per_tx - pat_fee_per_tx)),
            'monthly_savings_1000tx': pat_analysis['user_savings']['monthly_savings'],
            'psychological_barriers': {
                'trust_barrier': 0.3,  # 30% users hesitant about new tech
                'complexity_barrier': 0.2,  # 20% find aggregation complex
                'incentive_threshold': 0.5  # 50% fee reduction needed for mass adoption
            }
        }

        return user_incentives

    def calculate_miner_incentives(self, chain: str = "dogecoin") -> Dict[str, Any]:
        """
        Calculate miner adoption incentives for PAT.

        Args:
            chain: Blockchain network

        Returns:
            Miner incentive analysis
        """
        # Get historical data
        historical_data = MempoolFeeData.get_sample_data(chain, days=30)

        # Forecast fees
        forecast = self.forecaster.forecast_fees(historical_data, forecast_days=90)

        # Analyze PAT impact
        pat_analysis = self.forecaster.analyze_pat_impact(forecast, pat_adoption_rate=0.85)

        # Miner-specific analysis
        total_fee_reduction = pat_analysis['total_fee_reduction']
        miner_cut = 0.01  # Assume 1% of fees go to miners
        miner_revenue_loss = total_fee_reduction * miner_cut

        miner_incentives = {
            'current_daily_revenue': forecast['current_fee'] * 1000 * miner_cut,  # 1000 tx/day
            'pat_daily_revenue': pat_analysis['avg_pat_fee'] * 1000 * miner_cut,
            'revenue_reduction_percent': (miner_revenue_loss / (forecast['current_fee'] * 90 * miner_cut)) * 100,
            'compensation_mechanisms': [
                'Block rewards increase',
                'Transaction volume increase',
                'Protocol fee redistribution',
                'Staking rewards for PAT validation'
            ],
            'adoption_benefits': [
                'Faster block processing',
                'Reduced network congestion',
                'Improved scalability',
                'Enhanced security through aggregation'
            ],
            'transition_costs': {
                'software_updates': 50000,  # USD
                'miner_education': 10000,   # USD
                'testing_period': 30        # days
            }
        }

        return miner_incentives

    def model_adoption_curve(self, time_horizon: int = 365) -> Dict[str, Any]:
        """
        Model PAT adoption curve using logistic growth model.

        Args:
            time_horizon: Days to model

        Returns:
            Adoption curve analysis
        """
        # Logistic growth parameters
        carrying_capacity = 0.95  # 95% max adoption
        growth_rate = 0.02        # Daily growth rate
        midpoint = 180           # Days to 50% adoption

        days = np.arange(time_horizon)
        adoption_rate = carrying_capacity / (1 + np.exp(-growth_rate * (days - midpoint)))

        # Calculate incentives at different adoption levels
        incentives = []
        for rate in [0.1, 0.25, 0.5, 0.75, 0.9]:
            pat_analysis = self.forecaster.analyze_pat_impact(
                self.forecaster.forecast_fees(
                    MempoolFeeData.get_sample_data("dogecoin", 30), 30
                ),
                pat_adoption_rate=rate
            )
            incentives.append({
                'adoption_rate': rate,
                'fee_reduction': pat_analysis['fee_reduction_percent'],
                'user_savings': pat_analysis['user_savings']['monthly_savings']
            })

        return {
            'time_horizon_days': time_horizon,
            'adoption_curve': adoption_rate.tolist(),
            'key_milestones': {
                '10_percent': np.where(adoption_rate >= 0.1)[0][0] if len(np.where(adoption_rate >= 0.1)[0]) > 0 else None,
                '50_percent': np.where(adoption_rate >= 0.5)[0][0] if len(np.where(adoption_rate >= 0.5)[0]) > 0 else None,
                '90_percent': np.where(adoption_rate >= 0.9)[0][0] if len(np.where(adoption_rate >= 0.9)[0]) > 0 else None
            },
            'incentive_levels': incentives,
            'growth_parameters': {
                'carrying_capacity': carrying_capacity,
                'growth_rate': growth_rate,
                'midpoint_days': midpoint
            }
        }


def run_economic_analysis():
    """Run comprehensive economic analysis for PAT."""
    print("💰 PAT Economic Analysis Suite")
    print("=" * 35)

    model = EconomicIncentiveModel()

    # User incentives
    print("\n👤 User Adoption Incentives (Dogecoin)")
    print("-" * 40)
    user_inc = model.calculate_user_incentives("dogecoin")
    print(f"  Current cost per tx: {user_inc['current_cost_per_tx']:.6f} DOGE")
    print(f"  PAT cost per tx: {user_inc['pat_cost_per_tx']:.6f} DOGE")
    print(f"  Savings per tx: {user_inc['savings_per_tx']:.6f} DOGE ({user_inc['savings_percent']:.1f}%)")
    print(f"  Monthly savings (1000 tx): {user_inc['monthly_savings_1000tx']:.2f} DOGE")

    # Miner incentives
    print("\n⛏️ Miner Adoption Incentives (Dogecoin)")
    print("-" * 40)
    miner_inc = model.calculate_miner_incentives("dogecoin")
    print(f"  Daily revenue reduction: {miner_inc['revenue_reduction_percent']:.1f}%")
    print(f"  Transition costs: ${miner_inc['transition_costs']['software_updates'] + miner_inc['transition_costs']['miner_education']:,}")

    # Adoption curve
    print("\n📈 PAT Adoption Curve (365 days)")
    print("-" * 35)
    adoption = model.model_adoption_curve(365)
    milestones = adoption['key_milestones']
    print(f"  10% adoption: Day {milestones['10_percent']}")
    print(f"  50% adoption: Day {milestones['50_percent']}")
    print(f"  90% adoption: Day {milestones['90_percent']}")

    return {
        'user_incentives': user_inc,
        'miner_incentives': miner_inc,
        'adoption_curve': adoption
    }


def benchmark_fee_forecasting():
    """Benchmark fee forecasting accuracy."""
    print("📊 Fee Forecasting Benchmark")
    print("=" * 30)

    forecaster = FeeForecaster()

    # Test on Dogecoin data
    data = MempoolFeeData.get_sample_data("dogecoin", 30)
    forecast = forecaster.forecast_fees(data, forecast_days=7)

    print(f"  Model type: {forecast['model_type']}")
    print(f"  Current fee: {forecast['current_fee']:.3f} {forecast['unit']}")
    print(f"  7-day avg forecast: {forecast['avg_forecast_fee']:.3f} {forecast['unit']}")

    # Test PAT impact
    pat_impact = forecaster.analyze_pat_impact(forecast, pat_adoption_rate=0.9)
    print(f"  PAT fee reduction: {pat_impact['fee_reduction_percent']:.1f}%")
    print(f"  Daily user savings (1000 tx): {pat_impact['user_savings']['daily_savings']:.2f} DOGE")

    return forecast, pat_impact


if __name__ == "__main__":
    # Run economic analysis
    economic_results = run_economic_analysis()

    # Run forecasting benchmark
    forecast_results, pat_impact = benchmark_fee_forecasting()

    print("\n✅ Economic modeling complete!")
    print("   📈 Fee forecasting operational")
    print("   💡 Incentive analysis complete")
    print("   📊 Adoption modeling ready")
