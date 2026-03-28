"""
Unit V — Data Quality Tests
Runs automatically in GitHub Actions CI pipeline (Job 1: validate-data)
Also run manually: python test_data_quality.py
"""

import pandas as pd
import sys
import os

PASS = "✅ PASS"
FAIL = "❌ FAIL"
results = []

def check(name, condition, detail=""):
    status = PASS if condition else FAIL
    results.append((status, name, detail))
    print(f"  {status}  {name}" + (f"  [{detail}]" if detail else ""))
    return condition

print("\n" + "="*60)
print("  Healthcare Dataset — Data Quality Tests")
print("="*60)

# Load
try:
    df = pd.read_csv("healthcare_dataset.csv")
    print(f"\n  Loaded: {len(df):,} rows × {len(df.columns)} columns\n")
except FileNotFoundError:
    print("❌ healthcare_dataset.csv not found — skipping tests")
    sys.exit(0)

# ── Test Suite ──────────────────────────────────────────────────
print("── Row Count ──────────────────────────────────────────────")
check("Dataset has at least 1000 rows", len(df) >= 1000, f"{len(df):,} rows")

print("\n── Required Columns ──────────────────────────────────────")
required = ["Name","Age","Gender","Blood Type","Medical Condition",
            "Date of Admission","Discharge Date","Admission Type",
            "Hospital","Doctor","Billing Amount","Insurance Provider",
            "Medication","Test Results"]
for col in required:
    check(f"Column '{col}' exists", col in df.columns)

print("\n── Null Checks ───────────────────────────────────────────")
critical_cols = ["Medical Condition","Billing Amount","Admission Type","Test Results"]
for col in critical_cols:
    if col in df.columns:
        nulls = df[col].isnull().sum()
        check(f"'{col}' has no nulls", nulls == 0, f"{nulls} nulls")

print("\n── Value Validation ──────────────────────────────────────")
if "Admission Type" in df.columns:
    valid_types = {"Emergency","Elective","Urgent"}
    actual = set(df["Admission Type"].unique())
    check("Admission Type has valid values", actual.issubset(valid_types), str(actual))

if "Test Results" in df.columns:
    valid_results = {"Normal","Abnormal","Inconclusive"}
    actual = set(df["Test Results"].unique())
    check("Test Results has valid values", actual.issubset(valid_results), str(actual))

if "Gender" in df.columns:
    valid_gender = {"Male","Female"}
    actual = set(df["Gender"].unique())
    check("Gender has valid values", actual.issubset(valid_gender), str(actual))

if "Medical Condition" in df.columns:
    valid_cond = {"Arthritis","Asthma","Cancer","Diabetes","Hypertension","Obesity"}
    actual = set(df["Medical Condition"].unique())
    check("Medical Condition has valid values", actual.issubset(valid_cond), str(actual))

print("\n── Numeric Validation ────────────────────────────────────")
if "Billing Amount" in df.columns:
    billing = pd.to_numeric(df["Billing Amount"], errors="coerce")
    check("Billing Amount is numeric", billing.notna().all())
    check("Billing Amount > 0", (billing > 0).all(), f"min={billing.min():.2f}")
    check("Billing Amount < 1,000,000", (billing < 1_000_000).all(), f"max={billing.max():.2f}")

if "Age" in df.columns:
    age = pd.to_numeric(df["Age"], errors="coerce")
    check("Age is numeric", age.notna().all())
    check("Age between 0 and 120", ((age >= 0) & (age <= 120)).all(),
          f"range: {age.min()}-{age.max()}")

# ── Summary ─────────────────────────────────────────────────────
print("\n" + "="*60)
passed = sum(1 for r in results if r[0] == PASS)
failed = sum(1 for r in results if r[0] == FAIL)
total  = len(results)
print(f"  Results: {passed}/{total} passed  |  {failed} failed")
print("="*60)

if failed > 0:
    print(f"\n  ⚠️  {failed} test(s) failed — check dataset quality")
    sys.exit(1)
else:
    print("\n  🎉 All tests passed — dataset is clean!")
    sys.exit(0)
