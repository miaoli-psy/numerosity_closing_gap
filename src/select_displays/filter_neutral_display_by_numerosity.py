"""
save displays only with the reference numerosity we need
"""
import os
import re
import pandas as pd
import sys
import ast
import numpy as np

sys.path.append(r'D:\OneDrive\Programming\crwdngnmrsty_displays')
from src.properties import Properties

IN_DIR = "../../displays_netural/displays/"
OUT_DIR = "../../displays_netural/displays_filtered/"
os.makedirs(OUT_DIR, exist_ok=True)

pattern = re.compile(r"^ws1_neutral_angle(\d+)_drctn0_processed\.csv$")
keep_map = {40: 9, 60: 13, 90: 18, 120: 24, 170: 30}


def _to_list(x):
    """Convert a string like '[(1,2),(3,4)]' or '[[1,2],[3,4]]' into python list."""
    if isinstance(x, str):
        return ast.literal_eval(x)
    return x


def build_all_posis(df: pd.DataFrame) -> pd.DataFrame:
    """
    Ensure centralposis and extraposis_limited are lists,
    then create all_posis = centralposis + extraposis_limited.
    """
    if "centralposis" not in df.columns:
        raise ValueError("Missing required column: centralposis")
    if "extraposis_limited" not in df.columns:
        raise ValueError("Missing required column: extraposis_limited")

    df = df.copy()
    df["centralposis"] = df["centralposis"].apply(_to_list)
    df["extraposis_limited"] = df["extraposis_limited"].apply(_to_list)

    def _merge(row):
        c = row["centralposis"]
        e = row["extraposis_limited"]
        c = c if isinstance(c, list) else []
        e = e if isinstance(e, list) else []
        return c + e

    df["all_posis"] = df.apply(_merge, axis=1)
    return df


def add_properties(df: pd.DataFrame) -> pd.DataFrame:
    """
    For each row, compute Properties(all_posis) and add columns.
    Column names use your matching script convention.
    """
    df = df.copy()

    dens, ch, sp, ecc, occ = [], [], [], [], []

    for pos_list in df["all_posis"]:
        if not isinstance(pos_list, list) or len(pos_list) == 0:
            dens.append(np.nan)
            ch.append(np.nan)
            sp.append(np.nan)
            ecc.append(np.nan)
            occ.append(np.nan)
            continue

        P = Properties(pos_list)

        dens.append(P.density)
        ch.append(P.convexhull)
        sp.append(P.average_spacing)
        ecc.append(P.averge_eccentricity)
        occ.append(P.occupancy_area)

    df["density"] = dens
    df["convexhull"] = ch
    df["average_spacing"] = sp
    df["average_eccentricity"] = ecc
    df["occupancy_area"] = occ

    return df


for fname in sorted(os.listdir(IN_DIR)):
    m = pattern.match(fname)
    if not m:
        continue

    angle = int(m.group(1))
    if angle not in keep_map:
        continue

    target_num = keep_map[angle]

    in_path = os.path.join(IN_DIR, fname)
    df = pd.read_csv(in_path)

    if "numerosity_limited" not in df.columns:
        raise ValueError(f"{fname} has no column 'numerosity_limited'")

    before_n = len(df)
    df_filt = df[df["numerosity_limited"] == target_num].copy()
    after_n = len(df_filt)
    print(f"{fname}: {before_n} → {after_n} rows kept (numerosity_limited = {target_num})")

    # build all_posis from two cols
    df_filt = build_all_posis(df_filt)

    # compute and add properties
    df_filt = add_properties(df_filt)

    # drop unwanted columns before saving
    cols_to_drop = ["numerosity", "extraposis", "len_extra_posi"]
    df_filt = df_filt.drop(columns=[c for c in cols_to_drop if c in df_filt.columns])

    out_name = fname.replace("_processed.csv", f"_num{target_num}_withproperties.csv")
    out_path = os.path.join(OUT_DIR, out_name)
    df_filt.to_csv(out_path, index=False)
