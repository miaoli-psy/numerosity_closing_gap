"""
Generate PsychoPy condition files (merged) where the original reference display （in closinggap1）
is replaced by the matched neutral display.

- Input matched files: matched_sector*.csv OR swap_matched*.csv (80 rows each)
- Neutral files: <matched_stem>_neutral_matched_angleXX.csv  (80 rows each)
- Output: merged condition files in folder "merged2"
- Column suffix:
    matched: _t or _r depending on arrangement
    neutral: _n
"""

import os
import re
import ast
import pandas as pd


from src.common.process_displays import mirror_coords

save_merged = True         # main output

# ---------------- working directory ----------------
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# ---------------- paths ----------------
# folder containing matched/swap_matched csvs (80-row files)
MATCH_DIR = "../../displays/selected/displays_psychopy/"

# folder containing your neutral matched outputs (80-row files)
NEUTRAL_DIR = "../../displays_netural/selected/neutral_matched_outputs/"

MERGED_DIR = os.path.join(script_dir, "merged2")
os.makedirs(MERGED_DIR, exist_ok=True)


# ---------------- patterns ----------------
SECTOR_FROM_NAME = re.compile(r"(?:sector|angle)(\d+)", re.IGNORECASE)

# ---------------- left/right assignment ----------------
def assign_left_right(row):
    if row["visual_field"] == 0:
        return pd.Series({
            "right_centralposis": row["centralposis"],
            "left_centralposis": row["centralposis_mirrored"],
            "right_extraposis": row["extraposis_limited"],
            "left_extraposis": row["extraposis_limited_mirrored"],
        })
    else:
        return pd.Series({
            "right_centralposis": row["centralposis_mirrored"],
            "left_centralposis": row["centralposis"],
            "right_extraposis": row["extraposis_limited_mirrored"],
            "left_extraposis": row["extraposis_limited"],
        })

def to_list(x):
    return ast.literal_eval(x) if isinstance(x, str) else x

def infer_suffix_for_matched(df):
    if "arrangement" not in df.columns:
        return ""
    a = str(df["arrangement"].iloc[0]).lower()
    if a == "tangential":
        return "_t"
    if a == "radial":
        return "_r"
    return ""

def process_df(df, kind, swap_flag):
    df = df.copy()

    # coords to list
    df["centralposis"] = df["centralposis"].apply(to_list)
    df["extraposis_limited"] = df["extraposis_limited"].apply(to_list)

    # mirror
    df["centralposis_mirrored"] = df["centralposis"].apply(mirror_coords)
    df["extraposis_limited_mirrored"] = df["extraposis_limited"].apply(mirror_coords)

    # left/right
    df[["right_centralposis", "left_centralposis", "right_extraposis", "left_extraposis"]] = \
        df.apply(assign_left_right, axis=1)

    # merge posis
    df["left_allposis"] = df.apply(lambda r: r["left_centralposis"] + r["left_extraposis"], axis=1)
    df["right_allposis"] = df.apply(lambda r: r["right_centralposis"] + r["right_extraposis"], axis=1)

    # type + swap
    df["type"] = "ref" if kind == "neutral" else "match"
    df["swap"] = swap_flag

    # suffix
    suffix = "_n" if kind == "neutral" else infer_suffix_for_matched(df)

    df.columns = [c + suffix for c in df.columns]

    useful = [
        "arrangement", "sector_angle", "numerosity_limited",
        "density", "convexhull", "occupancy_area",
        "average_eccentricity", "average_spacing",
        "right_centralposis", "right_extraposis",
        "left_centralposis", "left_extraposis",
        "left_allposis", "right_allposis",
        "type", "swap"
    ]

    cols = [c + suffix for c in useful if (c + suffix) in df.columns]
    return df[cols], suffix

def find_neutral(matched_fname, sector_angle):
    stem = os.path.splitext(matched_fname)[0]
    pref = f"{stem}_neutral_matched_angle{sector_angle}.csv"
    if os.path.exists(os.path.join(NEUTRAL_DIR, pref)):
        return pref

    for f in os.listdir(NEUTRAL_DIR):
        if f.endswith(".csv") and "neutral" in f.lower() and f"angle{sector_angle}" in f.lower():
            return f
    return None

def make_merged_filename(sector_angle: int, swap_flag: str):
    """
    swap_flag: 'no'  -> tangential probe
               'yes' -> radial probe
    """
    if swap_flag == "no":
        return f"merged_probe_tangential_sector{sector_angle}.csv"
    else:
        return f"merged_probe_radial_sector{sector_angle}.csv"

# ---------------- main loop ----------------
for matched_fname in sorted(os.listdir(MATCH_DIR)):
    if not matched_fname.endswith(".csv"):
        continue
    if not (matched_fname.startswith("matched_sector") or matched_fname.startswith("swap_matched")):
        continue

    m = SECTOR_FROM_NAME.search(matched_fname)
    if not m:
        print(f"[SKIP] cannot parse angle: {matched_fname}")
        continue
    sector_angle = int(m.group(1))
    swap_flag = "yes" if matched_fname.startswith("swap_") else "no"

    neutral_fname = find_neutral(matched_fname, sector_angle)
    if neutral_fname is None:
        print(f"[ERROR] no neutral for {matched_fname}")
        continue

    df_mat = pd.read_csv(os.path.join(MATCH_DIR, matched_fname))
    df_neu = pd.read_csv(os.path.join(NEUTRAL_DIR, neutral_fname))

    if df_mat.shape[0] != df_neu.shape[0]:
        print(f"[SKIP] row mismatch {matched_fname} vs {neutral_fname}")
        continue

    df_neu_proc, _ = process_df(df_neu, "neutral", swap_flag)
    df_mat_proc, _ = process_df(df_mat, "matched", swap_flag)

    df_merged = pd.concat([df_neu_proc, df_mat_proc], axis=1)

    merged_name = make_merged_filename(sector_angle, swap_flag)

    merged_path = os.path.join(MERGED_DIR, merged_name)
    df_merged.to_csv(merged_path, index=False)

    print(f"[MERGED] {matched_fname} + {neutral_fname} -> {merged_path}")