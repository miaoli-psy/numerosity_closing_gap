import pandas as pd
import numpy as np
from scipy.stats import ttest_ind
from datetime import datetime
from tqdm import tqdm

# === CONFIG ===
input_file = "displays_withproperties.csv"
sector_angle = 40
reference_n_total = 80
display_n_per_group = 10
top_k_match_candidates = 30
match_trials_per_reference = 100
max_reference_trials = 5000
p_thresh = 0.08
properties = ['density', 'convexhull', 'average_spacing', 'average_eccentricity', 'occupancy_area']
timestamp = datetime.now().strftime("%Y%m%d_%H%M")
numerosity_dict = {40: 9, 60: 13, 90: 18, 120: 24, 170: 30}
ref_num = numerosity_dict[sector_angle]
match_numerosities = [ref_num + i for i in [-4, -3, -2, -1, 1, 2, 3, 4]]
eps = 1e-6

# === LOAD DATA ===
df = pd.read_csv(input_file)
df = df[df['sector_angle'] == sector_angle]
print('here, got df')

def property_distance(row, ref_mean):
    return np.sqrt(np.sum([(row[prop] - ref_mean[prop]) ** 2 for prop in properties]))

def evaluate_match(ref, match):
    p_vals = {}
    for prop in properties:
        _, p = ttest_ind(ref[prop], match[prop], equal_var=False)
        p_vals[prop] = p
    n_pass = sum(p > p_thresh for p in p_vals.values())
    ecc_diff = abs(ref['average_eccentricity'].mean() - match['average_eccentricity'].mean())
    return n_pass, p_vals, ecc_diff

def find_matched_group(ref_group, match_pool, ref_label, match_label):
    ref_mean = {prop: ref_group[prop].mean() for prop in properties}
    candidate_groups = {}

    for num in match_numerosities:
        sub = match_pool[match_pool['numerosity_limited'] == num].copy()
        sub['dist'] = sub.apply(lambda row: property_distance(row, ref_mean), axis=1)
        candidate_groups[num] = sub.sort_values(by="dist").head(top_k_match_candidates).drop(columns="dist")

    results = []
    for _ in range(match_trials_per_reference):
        try:
            matched_parts = [candidate_groups[num].sample(n=display_n_per_group) for num in match_numerosities]
            matched_group = pd.concat(matched_parts, ignore_index=True)
            n_pass, p_vals, ecc_diff = evaluate_match(ref_group, matched_group)

            radial_ecc = ref_group['average_eccentricity'].mean() if ref_label == 'radial' else matched_group['average_eccentricity'].mean()
            tangential_ecc = matched_group['average_eccentricity'].mean() if ref_label == 'radial' else ref_group['average_eccentricity'].mean()

            success = (n_pass == 5) or (n_pass == 4 and radial_ecc > tangential_ecc)

            results.append({
                'ref': ref_group,
                'match': matched_group,
                'p_vals': p_vals,
                'n_pass': n_pass,
                'ecc_diff': ecc_diff,
                'radial_ecc': radial_ecc,
                'tangential_ecc': tangential_ecc,
                'success': success
            })

            if success:
                return results[-1]  # return best match early
        except:
            continue
    return results  # return all fallback attempts

def run_full_match(direction_name, ref_pool, match_pool, ref_label, match_label, filename_prefix, no_prefix=False):
    print(f"\n🔎 Matching for: {direction_name}")
    fallback_results = []

    for i in tqdm(range(max_reference_trials), desc="Reference sampling"):
        ref_group = ref_pool.sample(n=reference_n_total, random_state=100 + i).reset_index(drop=True)
        result = find_matched_group(ref_group, match_pool, ref_label, match_label)

        if isinstance(result, dict) and result['success']:
            selected = result
            print(f"\n✅ Match success with p-values:")
            break
        elif isinstance(result, list):
            fallback_results.extend(result)
    else:
        if fallback_results:
            fallback_results.sort(key=lambda x: (-x['n_pass'], not (x['radial_ecc'] > x['tangential_ecc']), x['ecc_diff']))
            selected = fallback_results[0]
            print(f"\n⚠️ Fallback match selected with p-values:")
        else:
            raise RuntimeError("❌ No valid match or fallback found.")

    for prop in properties:
        ref_mean = selected['ref'][prop].mean()
        match_mean = selected['match'][prop].mean()
        print(f"{prop:<22} p = {selected['p_vals'][prop]:.4f} | mean_ref = {ref_mean:.3f} | mean_match = {match_mean:.3f}")

    symbol = '>' if selected['radial_ecc'] > selected['tangential_ecc'] else '<'
    print(f"average_eccentricity: radial = {selected['radial_ecc']:.4f} {symbol} tangential = {selected['tangential_ecc']:.4f}")

    ref_name = f"{filename_prefix}reference_sector{sector_angle}_{timestamp}.csv"
    match_name = f"{filename_prefix}matched_sector{sector_angle}_{timestamp}.csv"

    if no_prefix:
        ref_name = f"reference_sector{sector_angle}_{timestamp}.csv"
        match_name = f"matched_sector{sector_angle}_{timestamp}.csv"

    selected['ref'].to_csv(ref_name, index=False)
    selected['match'].to_csv(match_name, index=False)

# === MATCHING 1: radial → tangential ===
ref_pool_1 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'] == ref_num)]
match_pool_1 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'].isin(match_numerosities))]
run_full_match("radial → tangential", ref_pool_1, match_pool_1, ref_label="radial", match_label="tangential", filename_prefix="", no_prefix=True)

# === MATCHING 2: tangential → radial ===
ref_pool_2 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'] == ref_num)]
match_pool_2 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'].isin(match_numerosities))]
run_full_match("tangential → radial", ref_pool_2, match_pool_2, ref_label="tangential", match_label="radial", filename_prefix="swap_")
