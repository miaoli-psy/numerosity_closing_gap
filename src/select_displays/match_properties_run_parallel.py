# file: select_balanced_displays_optimized.py

import pandas as pd
import numpy as np
import os
from scipy.stats import ttest_ind
from joblib import Parallel, delayed
from tqdm import tqdm
from datetime import datetime

# Set working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# === SELECT SECTOR ANGLE HERE ===
sector_input = 40# 40, 60, 90, 120, 170

# timestamp for unique filenames
timestamp = datetime.now().strftime('%Y%m%d_%H%M')

# Read displays
file = 'displays_withproperties.csv'
all_displays = pd.read_csv(os.path.join(script_dir, file))

n_trials = 10000
n_jobs = -1  # Use all available cores

# Define sector settings
sector_settings = {
    40: {'ref_arrangement': 'radial', 'ref_num': 9, 'match_arrangement': 'tangential'},
    60: {'ref_arrangement': 'radial', 'ref_num': 13, 'match_arrangement': 'tangential'},
    90: {'ref_arrangement': 'radial', 'ref_num': 18, 'match_arrangement': 'tangential'},
    120: {'ref_arrangement': 'radial', 'ref_num': 24, 'match_arrangement': 'tangential'},
    170: {'ref_arrangement': 'radial', 'ref_num': 30, 'match_arrangement': 'tangential'}
}

if sector_input not in sector_settings:
    raise ValueError(f"Unsupported sector_angle {sector_input}. Choose from: {list(sector_settings.keys())}")

config = sector_settings[sector_input]

# Helper function to perform matching
def match_properties(reference_group, match_group, n_trials=5000, ref_name='ref', match_name='match'):
    grouped_match = match_group.groupby('numerosity_limited')

    ref_density_std = reference_group['density'].std()
    ref_convexhull_std = reference_group['convexhull'].std()
    ref_spacing_std = reference_group['average_spacing'].std()
    ref_eccentricity_std = reference_group['average_eccentricity'].std()
    ref_occupancy_std = reference_group['occupancy_area'].std()

    def try_match(i):
        try:
            sampled_match = grouped_match.apply(lambda x: x.sample(n=5, random_state=i)).reset_index(drop=True)
            if len(reference_group) >= len(sampled_match):
                ref_sampled = reference_group.sample(n=len(sampled_match), random_state=i).reset_index(drop=True)

                norm_density_diff = abs(ref_sampled['density'].mean() - sampled_match['density'].mean()) / ref_density_std
                norm_convexhull_diff = abs(ref_sampled['convexhull'].mean() - sampled_match['convexhull'].mean()) / ref_convexhull_std
                norm_spacing_diff = abs(ref_sampled['average_spacing'].mean() - sampled_match['average_spacing'].mean()) / ref_spacing_std
                norm_ecc_diff = abs(ref_sampled['average_eccentricity'].mean() - sampled_match['average_eccentricity'].mean()) / ref_eccentricity_std
                norm_occupancy_diff = abs(ref_sampled['occupancy_area'].mean() - sampled_match['occupancy_area'].mean()) / ref_occupancy_std

                combined_diff = norm_density_diff + norm_convexhull_diff + norm_spacing_diff + norm_ecc_diff + norm_occupancy_diff

                t_density = ttest_ind(ref_sampled['density'], sampled_match['density'], equal_var=False)[1]
                t_convexhull = ttest_ind(ref_sampled['convexhull'], sampled_match['convexhull'], equal_var=False)[1]
                t_spacing = ttest_ind(ref_sampled['average_spacing'], sampled_match['average_spacing'], equal_var=False)[1]
                t_ecc = ttest_ind(ref_sampled['average_eccentricity'], sampled_match['average_eccentricity'], equal_var=False)[1]
                t_occupancy = ttest_ind(ref_sampled['occupancy_area'], sampled_match['occupancy_area'], equal_var=False)[1]

                if t_density > 0.08 and t_convexhull > 0.08 and t_spacing > 0.08 and t_ecc > 0.08 and t_occupancy > 0.08:
                    return (combined_diff, ref_sampled, sampled_match, 'full')
                elif t_density > 0.08 and t_convexhull > 0.08 and t_spacing > 0.08:
                    reduced_diff = norm_density_diff + norm_convexhull_diff + norm_spacing_diff
                    return (reduced_diff, ref_sampled, sampled_match, 'reduced')
                elif t_density > 0.08:
                    return (norm_density_diff, ref_sampled, sampled_match, 'density_only')
        except:
            return None

        return None

    # results = Parallel(n_jobs=n_jobs)(
    #     delayed(try_match)(i) for i in tqdm(range(n_trials), desc=f"Matching {ref_name} vs {match_name}")
    # )

    results = []
    for i in tqdm(range(n_trials), desc=f"Matching {ref_name} vs {match_name}"):
        res = try_match(i)
        if res is not None:
            results.append(res)

    if not results:
        raise ValueError(
            f"No valid matching found for {ref_name} and {match_name} after all trials.")

    best_result = min(results, key=lambda x: x[0])
    return best_result[1], best_result[2]

    valid_results = [res for res in results if res is not None]

    if not valid_results:
        raise ValueError(f"No valid matching found for {ref_name} and {match_name} after all trials.")

    best_result = min(valid_results, key=lambda x: x[0])
    return best_result[1], best_result[2]

# --- Run Matching for Selected Sector ---

sector_data = all_displays[all_displays['sector_angle'] == sector_input]

# Reference group
reference_group = sector_data[
    (sector_data['arrangement'] == config['ref_arrangement']) &
    (sector_data['numerosity_limited'] == config['ref_num'])
]

# Matching group (ref +/- 1~4, excluding ref)
matching_numerosities = [config['ref_num'] + i for i in [-4, -3, -2, -1, 1, 2, 3, 4]]
matching_group = sector_data[
    (sector_data['arrangement'] == config['match_arrangement']) &
    (sector_data['numerosity_limited'].isin(matching_numerosities))
]

# Normal match
best_ref, best_match = match_properties(
    reference_group,
    matching_group,
    n_trials=n_trials,
    ref_name=f'ref_sector{sector_input}',
    match_name=f'match_sector{sector_input}'
)

best_ref.to_csv(f'reference_sector{sector_input}_{timestamp}.csv', index=False)
best_match.to_csv(f'matched_sector{sector_input}_{timestamp}.csv', index=False)

# Swapped match
swap_reference_group = sector_data[
    (sector_data['arrangement'] == config['match_arrangement']) &
    (sector_data['numerosity_limited'] == config['ref_num'])
]

swap_matching_group = sector_data[
    (sector_data['arrangement'] == config['ref_arrangement']) &
    (sector_data['numerosity_limited'].isin(matching_numerosities))
]

best_swap_ref, best_swap_match = match_properties(
    swap_reference_group,
    swap_matching_group,
    n_trials=n_trials,
    ref_name=f'swap_ref_sector{sector_input}',
    match_name=f'swap_match_sector{sector_input}'
)

best_swap_ref.to_csv(f'swap_reference_sector{sector_input}_{timestamp}.csv', index=False)
best_swap_match.to_csv(f'swap_matched_sector{sector_input}_{timestamp}.csv', index=False)
