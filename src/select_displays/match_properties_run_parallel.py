# file: select_balanced_displays_optimized.py

import pandas as pd
import numpy as np
import os
from scipy.stats import ttest_ind
from joblib import Parallel, delayed
from tqdm import tqdm

# Set working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# === SELECT SECTOR ANGLE HERE ===
sector_input = 120  # 40, 60, 90, 120, 170

# Read displays
file = 'displays_withproperties.csv'
all_displays = pd.read_csv(os.path.join(script_dir, file))

n_trials = 5000
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
    grouped_match = match_group.groupby('numerosity_limited') # groups the matched displays by numerosity, sample 5 from each

    def try_match(i):
        try:
            sampled_match = grouped_match.apply(lambda x: x.sample(n=5, random_state=i)).reset_index(drop=True)
            if len(reference_group) >= len(sampled_match):
                ref_sampled = reference_group.sample(n=len(sampled_match), random_state=i).reset_index(drop=True)

                density_diff = abs(ref_sampled['density'].mean() - sampled_match['density'].mean())
                convexhull_diff = abs(ref_sampled['convexhull'].mean() - sampled_match['convexhull'].mean())
                spacing_diff = abs(ref_sampled['average_spacing'].mean() - sampled_match['average_spacing'].mean())
                eccentricity_diff = abs(ref_sampled['average_eccentricity'].mean() - sampled_match['average_eccentricity'].mean())

                combined_diff = density_diff + convexhull_diff + spacing_diff + eccentricity_diff

                t_density = ttest_ind(ref_sampled['density'], sampled_match['density'], equal_var=False)[1]
                t_convexhull = ttest_ind(ref_sampled['convexhull'], sampled_match['convexhull'], equal_var=False)[1]
                t_spacing = ttest_ind(ref_sampled['average_spacing'], sampled_match['average_spacing'], equal_var=False)[1]
                t_ecc = ttest_ind(ref_sampled['average_eccentricity'], sampled_match['average_eccentricity'], equal_var=False)[1]

                if t_density > 0.05 and t_convexhull > 0.05 and t_spacing > 0.05 and t_ecc > 0.05:
                    return (combined_diff, ref_sampled, sampled_match, 'full')
                elif t_density > 0.05 and t_convexhull > 0.05 and t_spacing > 0.05:
                    reduced_diff = density_diff + convexhull_diff + spacing_diff
                    return (reduced_diff, ref_sampled, sampled_match, 'reduced')
                elif t_density > 0.05:
                    return (density_diff, ref_sampled, sampled_match, 'density_only')
        except:
            return None

        return None

    results = Parallel(n_jobs=n_jobs)(
        delayed(try_match)(i) for i in tqdm(range(n_trials), desc=f"Matching {ref_name} vs {match_name}")
    )

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

best_ref.to_csv(f'reference_sector{sector_input}.csv', index=False)
best_match.to_csv(f'matched_sector{sector_input}.csv', index=False)

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

best_swap_ref.to_csv(f'swap_reference_sector{sector_input}.csv', index=False)
best_swap_match.to_csv(f'swap_matched_sector{sector_input}.csv', index=False)
