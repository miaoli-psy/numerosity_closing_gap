import os
import pandas as pd
import ast
import re

from src.common.draw_display import drawEllipse_full
from src.common.process_displays import mirror_coords

draw = False
save_to_csv = True

# working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

#  paths
INPUT_DIR = "../../displays/selected/displays_psychopy/"
OUTPUT_DIR = os.path.join(script_dir, "processed")

# output dir
os.makedirs(OUTPUT_DIR, exist_ok=True)

# cols
useful_cols = [
    'arrangement', 'sector_angle', 'numerosity_limited',
    'density', 'convexhull', 'occupancy_area',
    'average_eccentricity', 'average_spacing',
    'right_centralposis', 'right_extraposis',
    'left_centralposis', 'left_extraposis',
    'left_allposis', 'right_allposis'
]

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

        # Filter columns
        df_filtered = df[useful_cols]

        # save
        if save_to_csv:
            out_path = os.path.join(OUTPUT_DIR, f"processed_{filename}")
            df_filtered.to_csv(out_path, index=False)
            print(f"✅ Saved: {out_path}")
