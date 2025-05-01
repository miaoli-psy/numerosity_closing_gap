'''
从算法里生成的raw displays存在RAW_DISPLAY_NEW_PATH，和已经在RAW_DISPLAY_OLD_PATH进行整合,
然后overwrite到RAW_DISPLAY_OLD_PATH中。得到全部的raw displays
'''

import os
import pandas as pd

# Set working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

RAW_DISPLAY_NEW_PATH = '../../../../Programming/crwdngnmrsty_displays/src/'
RAW_DISPLAY_OLD_PATH = '../../displays/displays_n5000/'

# Get list of CSVs in NEW directory
for filename in os.listdir(RAW_DISPLAY_NEW_PATH):
    if not filename.endswith('.csv'):
        continue

    new_file_path = os.path.join(RAW_DISPLAY_NEW_PATH, filename)
    old_file_path = os.path.join(RAW_DISPLAY_OLD_PATH, filename)

    if not os.path.exists(old_file_path):
        print(f"Skipping {filename}, no matching file in OLD path.")
        continue

    # Read both CSVs
    df_new = pd.read_csv(new_file_path)
    df_old = pd.read_csv(old_file_path)

    # Concatenate vertically
    df_combined = pd.concat([df_old, df_new], ignore_index=True)

    # Drop duplicate rows ignoring the first column 'n'
    # cols_to_check = df_combined.columns[1:]  # all columns except 'n'
    # df_combined = df_combined.drop_duplicates(subset=cols_to_check, keep='first')

    # Reset the first column 'n' to n_rows
    df_combined.iloc[:, 0] = range(1, len(df_combined) + 1)

    # Save back to OLD path, overwriting the old file
    df_combined.to_csv(old_file_path, index=False)

    print(f"Merged and saved: {filename}")