import pandas as pd
import numpy as np
from scipy.stats import ttest_ind
from datetime import datetime
from tqdm import tqdm
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing

# === CONFIG ===
input_file = "displays_withproperties.csv"
sector_angle = 60
reference_n_total = 80
display_n_per_group = 10
top_k_match_candidates = 30
match_trials_per_reference = 100
max_reference_trials = 1000
p_thresh = 0.08
properties = ['density', 'convexhull', 'average_spacing', 'average_eccentricity', 'occupancy_area']
timestamp = datetime.now().strftime("%Y%m%d_%H%M")
numerosity_dict = {40: 9, 60: 13, 90: 18, 120: 24, 170: 30}
ref_num = numerosity_dict[sector_angle]
match_numerosities = [ref_num + i for i in [-4, -3, -2, -1, 1, 2, 3, 4]]
eps = 1e-6

available_cpus = multiprocessing.cpu_count()
n_jobs = min(32, available_cpus)  # 你可以改成 8、12、30 等任何你想用的核数
print(f"🧠 Detected {available_cpus} CPU cores. Using {n_jobs} cores for parallel matching.")

# === LOAD DATA ===
df = pd.read_csv(input_file)
df = df[df['sector_angle'] == sector_angle]
print('✅ Loaded and filtered df')


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

            # success = (n_pass == 5) or (n_pass == 4 and radial_ecc > tangential_ecc)

            # Strict success (all p-values > threshold)
            strict_success = all(p > p_thresh for p in p_vals.values())

            # Fallback criteria: 4 pass + radial_ecc > tangential_ecc
            fallback_success = (n_pass == 4 and radial_ecc > tangential_ecc)

            # Final label
            success = strict_success

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
                return results[-1]  # Return first success early
        except:
            continue
    return results  # fallback list


# === Parallel worker function ===
def _parallel_worker(i, ref_pool, match_pool, ref_label, match_label):
    np.random.seed(100 + i)
    ref_group = ref_pool.sample(n=reference_n_total, random_state=100 + i).reset_index(drop=True)
    return find_matched_group(ref_group, match_pool, ref_label, match_label)


# === Parallel match executor ===
def run_full_match_parallel(direction_name, ref_pool, match_pool, ref_label, match_label, filename_prefix, no_prefix=False, n_jobs=8):
    print(f"\n🔎 Matching for: {direction_name}")
    fallback_results = []

    with ProcessPoolExecutor(max_workers=n_jobs) as executor:
        futures = [executor.submit(_parallel_worker, i, ref_pool, match_pool, ref_label, match_label)
                   for i in range(max_reference_trials)]

        for future in tqdm(as_completed(futures), total=max_reference_trials, desc="Reference sampling"):
            result = future.result()

            # ✅ 严格成功：所有 p > threshold
            if isinstance(result, dict) and result['success']:
                print(f"\n✅ Match success with p-values (all p > {p_thresh}):")

                for prop in properties:
                    ref_mean = result['ref'][prop].mean()
                    match_mean = result['match'][prop].mean()
                    print(f"{prop:<22} p = {result['p_vals'][prop]:.4f} | mean_ref = {ref_mean:.3f} | mean_match = {match_mean:.3f}")

                symbol = '>' if result['radial_ecc'] > result['tangential_ecc'] else '<'
                print(f"average_eccentricity: radial = {result['radial_ecc']:.4f} {symbol} tangential = {result['tangential_ecc']:.4f}")

                ref_name = f"{filename_prefix}reference_sector{sector_angle}_{timestamp}.csv"
                match_name = f"{filename_prefix}matched_sector{sector_angle}_{timestamp}.csv"
                if no_prefix:
                    ref_name = f"reference_sector{sector_angle}_{timestamp}.csv"
                    match_name = f"matched_sector{sector_angle}_{timestamp}.csv"

                result['ref'].to_csv(ref_name, index=False)
                result['match'].to_csv(match_name, index=False)

                executor.shutdown(wait=False, cancel_futures=True)  # 🔥 主动关闭还未执行的任务
                return  # ✅ 提前终止函数

            elif isinstance(result, list):
                fallback_results.extend(result)

    # === fallback 匹配 ===
    fallback_candidates = [r for r in fallback_results if r['n_pass'] == 4 and r['radial_ecc'] > r['tangential_ecc']]
    if fallback_candidates:
        fallback_candidates.sort(key=lambda x: x['ecc_diff'])
        selected = fallback_candidates[0]

        print(f"\n⚠️ Fallback match selected (4 pass + radial_ecc > tangential):")
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
        return  # ✅ fallback 成功也提前退出

    # === fallback 失败 ===
    # === fallback 匹配 ===
    fallback_candidates = [r for r in fallback_results if
                           r['n_pass'] == 4 and r['radial_ecc'] > r['tangential_ecc']]
    if fallback_candidates:
        fallback_candidates.sort(key=lambda x: x['ecc_diff'])
        selected = fallback_candidates[0]

        print(f"\n⚠️ Fallback match selected (4 pass + radial_ecc > tangential):")
    else:
        # 🚨 fallback 失败，执行最终保障策略：选出 ecc_diff 最小的一组
        print(
            f"\n🔁 All strict and fallback matches failed. Using closest match by average_eccentricity difference.")
        fallback_results.sort(key=lambda x: x['ecc_diff'])
        selected = fallback_results[0]

    # ✅ 统一输出保存逻辑
    for prop in properties:
        ref_mean = selected['ref'][prop].mean()
        match_mean = selected['match'][prop].mean()
        print(
            f"{prop:<22} p = {selected['p_vals'][prop]:.4f} | mean_ref = {ref_mean:.3f} | mean_match = {match_mean:.3f}")

    symbol = '>' if selected['radial_ecc'] > selected['tangential_ecc'] else '<'
    print(
        f"average_eccentricity: radial = {selected['radial_ecc']:.4f} {symbol} tangential = {selected['tangential_ecc']:.4f}")

    ref_name = f"{filename_prefix}reference_sector{sector_angle}_{timestamp}.csv"
    match_name = f"{filename_prefix}matched_sector{sector_angle}_{timestamp}.csv"
    if no_prefix:
        ref_name = f"reference_sector{sector_angle}_{timestamp}.csv"
        match_name = f"matched_sector{sector_angle}_{timestamp}.csv"

    selected['ref'].to_csv(ref_name, index=False)
    selected['match'].to_csv(match_name, index=False)
    return


# === MAIN EXECUTION ===
if __name__ == "__main__":
    # MATCHING 1: radial → tangential
    ref_pool_1 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'] == ref_num)]
    match_pool_1 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'].isin(match_numerosities))]
    run_full_match_parallel("radial → tangential", ref_pool_1, match_pool_1, ref_label="radial", match_label="tangential", filename_prefix="", no_prefix=True, n_jobs=n_jobs)

    # MATCHING 2: tangential → radial
    ref_pool_2 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'] == ref_num)]
    match_pool_2 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'].isin(match_numerosities))]
    run_full_match_parallel("tangential → radial", ref_pool_2, match_pool_2, ref_label="tangential", match_label="radial", filename_prefix="swap_",n_jobs=n_jobs)
