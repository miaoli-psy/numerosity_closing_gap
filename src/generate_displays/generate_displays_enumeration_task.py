"""
得到psychopy需要的enumeration task的condition file
RM range 3-6 with our custom algorithm.
"""

import os
import pandas as pd
import numpy as np
import ast

# === functions ===
def rotate_point(x, y, theta_deg):
    theta_rad = np.radians(theta_deg)
    cos_theta = np.cos(theta_rad)
    sin_theta = np.sin(theta_rad)
    x_new = cos_theta * x - sin_theta * y
    y_new = sin_theta * x + cos_theta * y
    return (x_new, y_new)

def rotate_positions(pos_list, angle_deg):
    return [rotate_point(x, y, angle_deg) for x, y in pos_list]

def all_points_on_screen(pos_list, margin=10):
    return all(np.hypot(x, y) <= (540 - margin) for (x, y) in pos_list)
rows = []

# === CONFIGURATION ===
save_to_csv = False

# paths
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

INPUT_DIR = "../../displays/selected/select_angle5_enumeration_task/"
input_files = [
    "ws1_tangential_angle5_drctn0.csv",
    "ws1_radial_angle5_drctn0.csv"
]

PROCESSED_DIR = os.path.join(script_dir, "merged_numtask")
os.makedirs(PROCESSED_DIR, exist_ok=True)
output_file = "processed_combined_angle5.csv"
output_path = os.path.join(PROCESSED_DIR, output_file)

# === Position set columns to extract ===
pos_cols = [
    "n3_posis_close", "n3_posis_far",
    "n4_posis_close", "n4_posis_far", "n4_posis_both",
    "n5_posis_close", "n5_posis_far"
]

# === Collect reformatted rows from both files ===
rows = []
for filename in input_files:
    file_path = os.path.join(INPUT_DIR, filename)
    df = pd.read_csv(file_path)
    df = df[df["numerosity"] == 6]  # keep only rows with numerosity == 6  # remove rows with numerosity == 3

    for _, row in df.iterrows():
        for col in pos_cols:
            val = row.get(col, None)
            if pd.isna(val) or val == "None":
                continue
            try:
                posis = ast.literal_eval(val)
            except Exception:
                continue
            if not all_points_on_screen(posis):
                continue
            rows.append({
                    "winsize": row["winsize"],
                    "protectzonetype": row["protectzonetype"],
                    "posis": posis,
                "numerosity": len(posis),
                "type": col.split("_")[-1]  # keep only 'close', 'far', or 'both'
            })

        # Add n6_posis_both = n3_posis_close + n3_posis_far
        try:
            close = ast.literal_eval(row["n3_posis_close"])
            far = ast.literal_eval(row["n3_posis_far"])
            combined = close + far
            if not all_points_on_screen(combined):
                continue
            rows.append({
                "winsize": row["winsize"],
                "protectzonetype": row["protectzonetype"],
                "posis": combined,
                "numerosity": len(combined),
                "type": "both"
            })
        except:
            continue
# === Build new DataFrame and export ===
long_df = pd.DataFrame(rows)

# === Sampling rules ===
sampled_df_list = []
for ptype in long_df['protectzonetype'].unique():
    sub_df = long_df[long_df['protectzonetype'] == ptype]
    sampled_df_list.extend([
        sub_df[(sub_df['numerosity'] == 3) & (sub_df['type'] == 'close')].sample(n=30, random_state=42),
        sub_df[(sub_df['numerosity'] == 3) & (sub_df['type'] == 'far')].sample(n=30, random_state=42),
        sub_df[(sub_df['numerosity'] == 4) & (sub_df['type'] == 'close')].sample(n=20, random_state=42),
        sub_df[(sub_df['numerosity'] == 4) & (sub_df['type'] == 'far')].sample(n=20, random_state=42),
        sub_df[(sub_df['numerosity'] == 4) & (sub_df['type'] == 'both')].sample(n=20, random_state=42),
        sub_df[(sub_df['numerosity'] == 5) & (sub_df['type'] == 'close')].sample(n=30, random_state=42),
        sub_df[(sub_df['numerosity'] == 5) & (sub_df['type'] == 'far')].sample(n=30, random_state=42),
        sub_df[(sub_df['numerosity'] == 6)].sample(n=60, random_state=42)
    ])

sampled_df = pd.concat(sampled_df_list, ignore_index=True)

# === Generate rotated versions after sampling ===
for i in range(1, 25):
    angle = (i - 1) * 15
    sampled_df[f'rotated_posis_{i}'] = sampled_df['posis'].apply(lambda pos: rotate_positions(pos, angle))

if save_to_csv:
    sampled_df.to_csv(output_path, index=False)
    print(f"Saved to {output_path}")

    # Split into 4 random chunks of 120 rows each
    shuffled = sampled_df.sample(frac=1, random_state=123).reset_index(drop=True)
    for i in range(4):
        chunk = shuffled.iloc[i * 120:(i + 1) * 120]
        chunk_file = os.path.join(PROCESSED_DIR, f"processed_chunk_{i+1}.csv")
        chunk.to_csv(chunk_file, index=False)
        print(f"Saved chunk {i+1} to {chunk_file}")


# the sampled_df is the file that contains 3-6 numerosity for both radial and tangential conditions
# for each arrangement, 240 rows/trials (30 num3_far, 30 num3_close. 20 num4_far, 20 num4_close, 20 num4_both
# 30 num5_far. 30 num5_close, 60 num6)

