"""
 生成psychopy需要的condition file。存在文件夹merged中
 添加left or right visual field。
 添加完整了allposis
 添加type=ref/match, swap=yes/no
 根据 arrangement 添加后缀（_t 或 _r）
"""

import os
import pandas as pd
import ast
import re
from collections import defaultdict

from src.common.draw_display import drawEllipse_full
from src.common.process_displays import mirror_coords

draw = False
save_to_csv = True

# working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

#  paths
INPUT_DIR = "../../displays/selected/displays_psychopy/"
PROCESSED_DIR = os.path.join(script_dir, "processed")
MERGED_DIR = os.path.join(script_dir, "merged")

# output dir
os.makedirs(PROCESSED_DIR, exist_ok=True)
os.makedirs(MERGED_DIR, exist_ok=True)

# left/right assignment logic
def assign_left_right(row):
    if row['visual_field'] == 0:
        return pd.Series({
            'right_centralposis': row['centralposis'],
            'left_centralposis': row['centralposis_mirrored'],
            'right_extraposis': row['extraposis_limited'],
            'left_extraposis': row['extraposis_limited_mirrored'],
        })
    else:
        return pd.Series({
            'right_centralposis': row['centralposis_mirrored'],
            'left_centralposis': row['centralposis'],
            'right_extraposis': row['extraposis_limited_mirrored'],
            'left_extraposis': row['extraposis_limited'],
        })

# process each CSV
for filename in os.listdir(INPUT_DIR):
    if filename.endswith(".csv"):
        filepath = os.path.join(INPUT_DIR, filename)
        df = pd.read_csv(filepath)

        # str to list
        df['centralposis'] = df['centralposis'].apply(ast.literal_eval)
        df['extraposis_limited'] = df['extraposis_limited'].apply(ast.literal_eval)

        # mirror coordinates
        df['centralposis_mirrored'] = df['centralposis'].apply(mirror_coords)
        df['extraposis_limited_mirrored'] = df['extraposis_limited'].apply(mirror_coords)

        # Assign left and right positions
        df[['right_centralposis', 'left_centralposis', 'right_extraposis', 'left_extraposis']] = \
            df.apply(assign_left_right, axis=1)

        if draw:
            sample_row = df.sample(n=1, random_state=0).iloc[0]
            drawEllipse_full(sample_row['right_centralposis'], sample_row['right_extraposis'], ka=0.25, kb=0.1)
            drawEllipse_full(sample_row['left_centralposis'], sample_row['left_extraposis'], ka=0.25, kb=0.1)

        # merge positions
        df['left_allposis'] = df.apply(lambda row: row['left_centralposis'] + row['left_extraposis'], axis=1)
        df['right_allposis'] = df.apply(lambda row: row['right_centralposis'] + row['right_extraposis'], axis=1)

        # add useful cols type and swap
        if 'reference' in filename:
            df['type'] = 'ref'
        else:
            df['type'] = 'match'

        if 'swap' in filename:
            df['swap'] = 'yes'
        else:
            df['swap'] = 'no'

        # suffix based on 'arrangement' column value
        first_arrangement = df['arrangement'].iloc[0]
        if first_arrangement == 'tangential':
            suffix = '_t'
        elif first_arrangement == 'radial':
            suffix = '_r'
        else:
            suffix = ''

        # Add suffix to all column names
        df.columns = [col + suffix for col in df.columns]

        useful_cols = [col + suffix for col in [
            'arrangement', 'sector_angle', 'numerosity_limited',
            'density', 'convexhull', 'occupancy_area',
            'average_eccentricity', 'average_spacing',
            'right_centralposis', 'right_extraposis',
            'left_centralposis', 'left_extraposis',
            'left_allposis', 'right_allposis',
            'type', 'swap']]

        # Filter columns
        df_filtered = df[useful_cols]

        # save
        if save_to_csv:
            out_path = os.path.join(PROCESSED_DIR, f"processed_{filename}")
            df_filtered.to_csv(out_path, index=False)
            print(f"Saved: {out_path}")


merge_pattern = re.compile(
    r'processed_(?P<swap>swap_)?(?P<type>matched|reference)_sector(?P<sector>\d+)(?:_[^_]*)*?_(?P<time>\d{8}_\d{4})\.csv'
)

# Group files
grouped = defaultdict(dict)  # key = (swap, sector, time) → {'matched': ..., 'reference': ...}

for file in os.listdir(PROCESSED_DIR):
    match = merge_pattern.match(file)
    if match:
        swap = 'swap' if match.group('swap') else 'noswap'
        file_type = match.group('type')  # matched or reference
        sector = match.group('sector')
        time = match.group('time')
        key = (swap, sector, time)
        grouped[key][file_type] = file

# Merge and save
for key, files in grouped.items():
    if 'matched' in files and 'reference' in files:
        ref_path = os.path.join(PROCESSED_DIR, files['reference'])
        match_path = os.path.join(PROCESSED_DIR, files['matched'])

        df_ref = pd.read_csv(ref_path)
        df_match = pd.read_csv(match_path)

        if df_ref.shape[0] != df_match.shape[0]:
            print(f"❌ Skipped (row mismatch): {files['reference']} + {files['matched']}")
            continue

        df_merged = pd.concat([df_ref, df_match], axis=1)

        swap, sector, time = key
        merged_filename = f"merged_{swap}_sector{sector}_{time}.csv"
        merged_path = os.path.join(MERGED_DIR, merged_filename)
        df_merged.to_csv(merged_path, index=False)

        print(f"🔗 Merged:\n  {files['reference']}\n  {files['matched']}\n→ {merged_filename}")