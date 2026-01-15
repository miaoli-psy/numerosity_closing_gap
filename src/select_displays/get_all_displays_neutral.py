"""
restrict positions on each display (from the algorithm)
to the defined sector
"""

import os
import re
import pandas as pd
import ast
from src.common.process_dataframe import insert_new_col, insert_new_col_from_n_cols
from src.common.process_displays import limit_posi_to_sector, get_len, remove_near_origin, \
    remove_outside_radius

# set current working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

RAW_DIR = "../../displays_netural/raw/"
OUT_DIR = "../../displays_netural/displays/"

os.makedirs(OUT_DIR, exist_ok=True)

# ---------- file pattern ----------
# filenames: ws1_neutral_aanglexx_drctn0.csv
pattern = re.compile(r"^ws1_neutral_angle(\d+)_drctn(\d+)\.csv$")

# angles we care about
target_angles = {40, 60, 90, 120, 170}

# ---------- columns ----------
needed_cols = [
    "extraposis", "centralposis",
    "numerosity"
]

# ---------- process each file ----------
for fname in sorted(os.listdir(RAW_DIR)):

    m = pattern.match(fname)

    angle = int(m.group(1))
    direction = int(m.group(2))

    in_path = os.path.join(RAW_DIR, fname)

    # read
    df = pd.read_csv(in_path, usecols=needed_cols)

    # add col visual field = 0
    df["visual_field"] = 0

    df["sector_angle"] = angle

    # ensure position columns are lists
    df["centralposis"] = df["centralposis"].apply(ast.literal_eval)
    df["extraposis"] = df["extraposis"].apply(ast.literal_eval)

    # remove fovea region
    df["centralposis"] = df["centralposis"].apply(remove_near_origin)
    df["extraposis"] = df["extraposis"].apply(remove_near_origin)

    # remove outside radius
    df["centralposis"] = df["centralposis"].apply(remove_outside_radius)
    df["extraposis"] = df["extraposis"].apply(remove_outside_radius)

    # limit extra positions to sector
    insert_new_col_from_n_cols(
        df,
        ["sector_angle", "visual_field", "extraposis"],
        "extraposis_limited",
        limit_posi_to_sector
    )

    # count extra positions after limiting
    insert_new_col(df, "extraposis_limited", "len_extra_posi", get_len)

    # recompute numerosity
    df["numerosity_limited"] = df["numerosity"] / 3 + df["len_extra_posi"]

    # write out (keep original name but add suffix to avoid overwriting raw)
    out_name = fname.replace(".csv", "_processed.csv")
    out_path = os.path.join(OUT_DIR, out_name)
    df.to_csv(out_path, index=False)

    print(f"[OK] angle={angle}, drctn={direction} -> {out_path}")
