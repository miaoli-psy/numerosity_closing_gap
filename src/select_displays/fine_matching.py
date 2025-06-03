# 对selected文件夹所有的成对displays group进行对比，得到density 等的均值，T值，和P值

import os
import re
import pandas as pd
from scipy.stats import ttest_ind
from datetime import datetime

# working path
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

PATH = '../../displays/selected/'
directory = os.path.join(script_dir, PATH)

# Get all .csv files
csv_files = [f for f in os.listdir(directory) if f.endswith('.csv')]

#Pattern to extract type, sector and timestamp
pattern = re.compile(r'(?P<type>(swap_)?(matched|reference))_sector(?P<sector>\d+)(?:_[^_]*)*?_(?P<time>\d{8}_\d{4})\.csv')

# Organize files by (type, sector, timestamp)
file_groups = {}
for f in csv_files:
    m = pattern.match(f)
    if not m:
        continue
    file_type = m.group('type')
    sector = int(m.group('sector'))
    timestamp = m.group('time') or ''
    key = (file_type, sector, timestamp)
    file_groups[key] = f

# Build matched-reference pairs with same timestamp
pairs = []
for sector in [40, 60, 90, 120, 170]:
    for prefix in ['', 'swap_']:
        for timestamp in sorted({k[2] for k in file_groups if k[1] == sector and k[0].startswith(prefix)}):
            m_key = (f'{prefix}matched', sector, timestamp)
            r_key = (f'{prefix}reference', sector, timestamp)
            if m_key in file_groups and r_key in file_groups:
                pairs.append((file_groups[m_key], file_groups[r_key]))

# Load all data and add metadata
all_dfs = []
for f in set(f for pair in pairs for f in pair):
    df = pd.read_csv(os.path.join(directory, f))
    df['is_ref'] = 1 if 'reference' in f else 0
    df['is_swap'] = 1 if 'swap' in f else 0
    df['source_file'] = f
    all_dfs.append(df)

all_combined = pd.concat(all_dfs, ignore_index=True)

# Perform t-tests and save summary
features = ['density', 'convexhull', 'occupancy_area', 'average_spacing', 'average_eccentricity']
summary_rows = []

for ref_file, match_file in pairs:
    ref_df = all_combined[all_combined['source_file'] == ref_file]
    match_df = all_combined[all_combined['source_file'] == match_file]

    row = {
        'ref_file': ref_file,
        'match_file': match_file,
    }

    for feat in features:
        t_stat, p_val = ttest_ind(ref_df[feat], match_df[feat], equal_var=False)
        row[f'{feat}_t'] = t_stat
        row[f'{feat}_p'] = p_val
        row[f'{feat}_p>0.05'] = p_val > 0.05

    for feat in features:
        row[f'{feat}_mean_ref'] = ref_df[feat].mean()
        row[f'{feat}_std_ref'] = ref_df[feat].std()
        row[f'{feat}_mean_match'] = match_df[feat].mean()
        row[f'{feat}_std_match'] = match_df[feat].std()

    summary_rows.append(row)

summary_df = pd.DataFrame(summary_rows)
summary_df.to_csv('summary.csv', index=False)
print("Summary saved to summary.csv with timestamp-aligned pair comparisons.")