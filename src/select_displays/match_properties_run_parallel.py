import os
import pandas as pd
import numpy as np
from joblib import executor
from scipy.stats import ttest_ind
from datetime import datetime
from tqdm import tqdm
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing
import psutil


# === CONFIG ===
reference_n_total = 160
display_n_per_group = 20
top_k_match_candidates = 30
match_trials_per_reference = 100
max_reference_trials = 5000
p_thresh = 0.05
properties = ['density', 'convexhull', 'average_spacing', 'average_eccentricity', 'occupancy_area']
timestamp = datetime.now().strftime("%Y%m%d_%H%M")
numerosity_dict = {40: 9, 60: 13, 90: 18, 120: 24, 170: 30}

hard_limit = 12

def estimate_safe_n_jobs(estimated_memory_per_process_mb=700, memory_safety_margin=0.5, hard_limit=hard_limit):
    # 可用总内存（MB）
    total_memory = psutil.virtual_memory().total / (1024**2)
    memory_for_pool = total_memory * memory_safety_margin

    # 💡 内存允许开的最大进程数（转换为单位相同的“个”）
    max_jobs_by_memory = int(memory_for_pool / estimated_memory_per_process_mb)

    # 检测到的逻辑核心数
    cpu_count = multiprocessing.cpu_count()

    # 核心安全上限：受限于逻辑核数、内存约束、硬限制
    safe_jobs = min(cpu_count, max_jobs_by_memory, hard_limit)

    print(f"Total RAM: {total_memory:.0f}MB → memory allows ~{max_jobs_by_memory} processes")
    print(f"Detected CPU cores: {cpu_count}")
    print(f"Hard limit: {hard_limit}")
    print(f"Safe to use: {safe_jobs} cores")
    return max(1, safe_jobs)

def property_distance(row, ref_mean):
    return np.sqrt(np.sum([(row[prop] - ref_mean[prop]) ** 2 for prop in properties]))

def evaluate_match(ref, match):
    p_vals = {prop: ttest_ind(ref[prop], match[prop], equal_var=False)[1] for prop in properties}
    n_pass = sum(p > p_thresh for p in p_vals.values())
    ecc_diff = abs(ref['average_eccentricity'].mean() - match['average_eccentricity'].mean())
    return n_pass, p_vals, ecc_diff

def find_matched_group(ref_group, match_pool, ref_label, match_label, match_numerosities):
    ref_mean = {prop: ref_group[prop].mean() for prop in properties}
    candidate_groups = {
        num: match_pool[match_pool['numerosity_limited'] == num].copy()
            .assign(dist=lambda df: df.apply(lambda row: property_distance(row, ref_mean), axis=1))
            .sort_values(by="dist").head(top_k_match_candidates).drop(columns="dist")
        for num in match_numerosities
    }

    results = []
    for _ in range(match_trials_per_reference):
        try:
            matched_parts = [candidate_groups[num].sample(n=display_n_per_group) for num in match_numerosities]
            matched_group = pd.concat(matched_parts, ignore_index=True)
            n_pass, p_vals, ecc_diff = evaluate_match(ref_group, matched_group)

            radial_ecc = ref_group['average_eccentricity'].mean() if ref_label == 'radial' else matched_group['average_eccentricity'].mean()
            tangential_ecc = matched_group['average_eccentricity'].mean() if ref_label == 'radial' else ref_group['average_eccentricity'].mean()
            strict_success = all(p > p_thresh for p in p_vals.values())

            results.append({
                'ref': ref_group,
                'match': matched_group,
                'p_vals': p_vals,
                'n_pass': n_pass,
                'ecc_diff': ecc_diff,
                'radial_ecc': radial_ecc,
                'tangential_ecc': tangential_ecc,
                'success': strict_success
            })

            if strict_success:
                return results[-1]
        except Exception:
            continue
    return results

def _parallel_worker(i, ref_pool, match_pool, ref_label, match_label, match_numerosities):
    ref_group = ref_pool.sample(n=reference_n_total).reset_index(drop=True)
    return find_matched_group(ref_group, match_pool, ref_label, match_label, match_numerosities)

def run_full_match_parallel(direction_name, ref_pool, match_pool, ref_label, match_label, filename_prefix, no_prefix, sector_angle, match_numerosities, n_jobs):
    print(f"\n🔎 Matching for: {direction_name}")
    fallback_results = []
    selected = None

    with ProcessPoolExecutor(max_workers=n_jobs) as executor:
        futures = [executor.submit(_parallel_worker, i, ref_pool, match_pool, ref_label, match_label, match_numerosities)
            for i in range(max_reference_trials)]

        progress_bar = tqdm(as_completed(futures), total=max_reference_trials,
                            desc="Reference sampling")

        for future in progress_bar:
            result = future.result()
            if isinstance(result, dict) and result['success']:
                selected = result
                progress_bar.close()
                print(f"\n✅ Match success with p-values (all p > {p_thresh}):", flush=True)
                executor.shutdown(wait=False, cancel_futures=True)  # 立刻中断未完成任务
                break

            elif isinstance(result, list):
                fallback_results.extend(result)
        else:
            progress_bar.close()

    if selected is None:
        # fallback 优先用 spacing 和 eccentricity 的 p 值最大作为目标
        print(f"No strict match. Using fallback with max p(spacing) + p(eccentricity)")
        fallback_results.sort(
            key=lambda x: -(x['p_vals']['average_spacing'] + x['p_vals']['average_eccentricity'])
        )
        selected = fallback_results[0]
        print(f"Fallback match selected (max p-values for spacing & eccentricity)")

    for prop in properties:
        ref_mean = selected['ref'][prop].mean()
        match_mean = selected['match'][prop].mean()
        print(f"{prop:<22} p = {selected['p_vals'][prop]:.4f} | mean_ref = {ref_mean:.3f} | mean_match = {match_mean:.3f}")
    symbol = '>' if selected['radial_ecc'] > selected['tangential_ecc'] else '<'
    print(f"average_eccentricity: radial = {selected['radial_ecc']:.4f} {symbol} tangential = {selected['tangential_ecc']:.4f}")

    ref_name = f"{filename_prefix}reference_sector{sector_angle}_nperg{display_n_per_group}_{timestamp}.csv"
    match_name = f"{filename_prefix}matched_sector{sector_angle}_nperg{display_n_per_group}_{timestamp}.csv"
    if no_prefix:
        ref_name = f"reference_sector{sector_angle}_nperg{display_n_per_group}_{timestamp}.csv"
        match_name = f"matched_sector{sector_angle}_nperg{display_n_per_group}_{timestamp}.csv"

    selected['ref'].to_csv(ref_name, index=False)
    selected['match'].to_csv(match_name, index=False)

def load_sector_displays(csv_path, sector_angle, usecols=None, chunksize=5000, cache_dir="cache", refresh_cache=False):
    os.makedirs(cache_dir, exist_ok=True)
    cache_file = os.path.join(cache_dir, f"displays_sector_{sector_angle}.pkl")

    if not refresh_cache and os.path.exists(cache_file):
        print(f"📂 Loading from cache: {cache_file}")
        return pd.read_pickle(cache_file)

    print(f"📥 Reading CSV and filtering sector_angle = {sector_angle}...")
    filtered_chunks = []
    for chunk in pd.read_csv(csv_path, usecols=usecols, chunksize=chunksize):
        filtered = chunk[chunk['sector_angle'] == sector_angle]
        if not filtered.empty:
            filtered_chunks.append(filtered)
    if not filtered_chunks:
        print(f"⚠️ No rows found for sector_angle = {sector_angle}")
        return pd.DataFrame()
    df = pd.concat(filtered_chunks, ignore_index=True)
    df.to_pickle(cache_file)
    print(f"✅ Loaded {len(df)} rows → Cached to {cache_file}")
    return df

if __name__ == "__main__":
    csv_file = "displays_withproperties.csv"
    sector_angle = 40
    ref_num = numerosity_dict[sector_angle]
    match_numerosities = [ref_num + i for i in [-4, -3, -2, -1, 1, 2, 3, 4]]
    n_jobs = estimate_safe_n_jobs()

    columns_needed = [
        'sector_angle', 'arrangement', 'numerosity_limited',
        'density', 'convexhull', 'average_spacing',
        'average_eccentricity', 'occupancy_area'
    ]

    df = load_sector_displays(
        csv_file,
        sector_angle=sector_angle,
        usecols=columns_needed,
        cache_dir="cache",
        refresh_cache=False
    )

    ref_pool_1 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'] == ref_num)]
    match_pool_1 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'].isin(match_numerosities))]
    run_full_match_parallel("radial → tangential", ref_pool_1, match_pool_1, "radial", "tangential", "", True, sector_angle, match_numerosities, n_jobs)

    ref_pool_2 = df[(df['arrangement'] == 'tangential') & (df['numerosity_limited'] == ref_num)]
    match_pool_2 = df[(df['arrangement'] == 'radial') & (df['numerosity_limited'].isin(match_numerosities))]
    run_full_match_parallel("tangential → radial", ref_pool_2, match_pool_2, "tangential", "radial", "swap_", False, sector_angle, match_numerosities, n_jobs)
