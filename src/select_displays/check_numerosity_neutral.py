"""
check numerosity distribution for displays that has been res
"""

import os
import pandas as pd
import re

OUT_DIR = "../../displays_netural/displays/"     # where the 5 processed CSVs are

# ---------------- file pattern ----------------
pattern = re.compile(r"^ws1_neutral_angle(\d+)_drctn0_processed\.csv$")

# angles we expect
target_angles = {40, 60, 90, 120, 170}


files = sorted([f for f in os.listdir(OUT_DIR) if f.endswith(".csv")])

all_counts = []  # for combined summary

for fname in files:
    m = pattern.match(fname)
    if not m:
        continue

    angle = int(m.group(1))
    if angle not in target_angles:
        continue

    in_path = os.path.join(OUT_DIR, fname)
    df = pd.read_csv(in_path)

    # If sector_angle col might be missing or wrong, force it from filename:
    df["sector_angle"] = angle

    if "numerosity_limited" not in df.columns:
        raise ValueError(f"{fname} missing column: numerosity_limited")

    # ---- simple count per numerosity_limited ----
    counts = (
        df["numerosity_limited"]
        .value_counts(dropna=False)
        .sort_index()
        .reset_index()
    )
    counts.columns = ["numerosity_limited", "count"]
    counts.insert(0, "sector_angle", angle)
    counts.insert(0, "source_file", fname)

    all_counts.append(counts)