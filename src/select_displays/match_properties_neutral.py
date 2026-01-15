"""
select neutral displays that match the properties with the radial
and tangnetial displays. 80 neutral displays were selected for each
sector angle.



先按 angle 固定 neutral 的 numerosity；
然后对每个 experimental numerosity group 抽同样多 neutral；
用 density 和 convex hull 做 Welch t-test；
p 都大于阈值才接受，否则 fallback；
每个 angle 和条件独立执行；
neutral 不重复使用。
"""
import os
import re
import numpy as np
import pandas as pd
from scipy.stats import ttest_ind

# set current working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# =========================
# CONFIG
# =========================
REF_DIR = r"../../displays/selected/displays_psychopy/"  # closinggap1中的radial and tangential displays”
NEUTRAL_DIR = r"../../displays_netural/displays_filtered/"
OUT_DIR = r"../../displays_netural/selected/neutral_matched_outputs/"
os.makedirs(OUT_DIR, exist_ok=True)

# 从文件名解析 angle（
# 1) reference 文件名里如果像 matched_sector40_*.csv
REF_ANGLE_PATTERN = re.compile(r"sector(\d+)")
# 2) neutral 文件名里如果像 ws1_neutral_angle40_*.csv
NEU_ANGLE_PATTERN = re.compile(r"neutral_angle(\d+)|neutral.*angle(\d+)")

# neutral 对应的固定 numerosity
NEU_NUM_MAP = {40: 9, 60: 13, 90: 18, 120: 24, 170: 30}

# 要匹配的属性
MATCH_PROPS = ["density", "convexhull"]
P_THRESH = 0.05

# 每个 reference numerosity group 的采样尝试次数
MAX_TRIALS_PER_GROUP = 5000
RANDOM_SEED = 123

# =========================
# HELPERS
# =========================
def parse_angle_from_ref(fname: str):
    m = REF_ANGLE_PATTERN.search(fname)
    return int(m.group(1)) if m else None

def parse_angle_from_neutral(fname: str):
    m = NEU_ANGLE_PATTERN.search(fname)
    if not m:
        return None
    # regex 有两个 group，取非空那个
    g1, g2 = m.group(1), m.group(2)
    return int(g1) if g1 is not None else int(g2)

def welch_pvals(a: pd.DataFrame, b: pd.DataFrame, props= MATCH_PROPS):
    p = {}
    for prop in props:
        _, pv = ttest_ind(a[prop], b[prop], equal_var=False)
        p[prop] = pv
    return p

def score_pvals(pvals: dict) -> float:
    # fallback 打分：越大越好（保守用 min）
    return float(min(pvals.values()))

def match_one_group(ref_group: pd.DataFrame, neutral_pool: pd.DataFrame, n_needed: int,
                    max_trials=MAX_TRIALS_PER_GROUP, p_thresh=P_THRESH, rng=None):
    """
    从 neutral_pool 中抽 n_needed 行，使 density & convexhull 对 ref_group 不显著（p>p_thresh）。
    返回：selected_df, best_pvals, success(bool)
    """
    if rng is None:
        rng = np.random.default_rng()

    best = None
    best_p = None
    best_score = -1.0

    # 若候选不足，直接报错
    if len(neutral_pool) < n_needed:
        raise ValueError(f"Neutral pool too small: need {n_needed}, but have {len(neutral_pool)}")

    for _ in range(max_trials):
        sample_idx = rng.choice(neutral_pool.index.to_numpy(), size=n_needed, replace=False)
        cand = neutral_pool.loc[sample_idx]

        pvals = welch_pvals(ref_group, cand, MATCH_PROPS)
        ok = all(p > p_thresh for p in pvals.values())

        sc = score_pvals(pvals)
        if sc > best_score:
            best = cand
            best_p = pvals
            best_score = sc

        if ok:
            return cand, pvals, True

    # fallback：返回最好的那个
    return best, best_p, False


def match_neutral_to_reference(ref_df: pd.DataFrame, neutral_df: pd.DataFrame, angle: int,
                              out_prefix: str, rng=None):
    if rng is None:
        rng = np.random.default_rng(RANDOM_SEED)

    need_cols = {"numerosity_limited", "density", "convexhull"}
    miss_ref = need_cols - set(ref_df.columns)
    miss_neu = need_cols - set(neutral_df.columns)
    if miss_ref:
        raise ValueError(f"Reference missing cols: {sorted(miss_ref)}")
    if miss_neu:
        raise ValueError(f"Neutral missing cols: {sorted(miss_neu)}")

    grp_sizes = ref_df.groupby("numerosity_limited").size().sort_index()

    remaining = neutral_df.sample(frac=1.0, random_state=RANDOM_SEED).reset_index(drop=True)
    remaining.index = np.arange(len(remaining))

    selected_blocks = []

    print(f"\n=== Matching neutral for angle {angle} ===")

    for num, n_needed in grp_sizes.items():
        ref_group = ref_df[ref_df["numerosity_limited"] == num].copy()

        sel, pvals, success = match_one_group(
            ref_group=ref_group,
            neutral_pool=remaining,
            n_needed=int(n_needed),
            rng=rng
        )

        print(f"  Group numerosity {num:>3}: "
              f"n={n_needed:>2} | "
              f"p_density={pvals['density']:.4f}, "
              f"p_convexhull={pvals['convexhull']:.4f} | "
              f"{'OK' if success else 'fallback'}")

        sel = sel.copy()
        sel["matched_to_ref_numerosity_limited"] = num
        selected_blocks.append(sel)

        remaining = remaining.drop(index=sel.index)

    neutral_selected = pd.concat(selected_blocks, ignore_index=True)

    overall_p = welch_pvals(ref_df, neutral_selected, MATCH_PROPS)
    print(f"  OVERALL (80 vs 80): "
          f"p_density={overall_p['density']:.4f}, "
          f"p_convexhull={overall_p['convexhull']:.4f}")

    out_neu = os.path.join(OUT_DIR, f"{out_prefix}_neutral_matched_angle{angle}.csv")
    neutral_selected.to_csv(out_neu, index=False)

    print(f"[OK] wrote neutral matched (80 rows) -> {out_neu}")

    return neutral_selected

# =========================
# MAIN: match for all angles/files
# =========================
def main():
    rng = np.random.default_rng(RANDOM_SEED)

    # 找 reference 文件
    ref_files = [
        f for f in os.listdir(REF_DIR)
        if f.endswith(".csv")
           and (f.startswith("matched_sector") or f.startswith("swap_matched"))]

    if not ref_files:
        raise FileNotFoundError(f"No reference csv found in {REF_DIR}")

    for ref_fname in sorted(ref_files):
        angle = parse_angle_from_ref(ref_fname)
        if angle is None:
            continue
        if angle not in NEU_NUM_MAP:
            continue  # 只处理这5个 angle

        # 找对应 neutral 文件
        neutral_candidates = []
        for nf in os.listdir(NEUTRAL_DIR):
            if not nf.endswith(".csv"):
                continue
            if "neutral" not in nf.lower():
                continue
            a2 = parse_angle_from_neutral(nf)
            if a2 == angle:
                neutral_candidates.append(nf)

        if not neutral_candidates:
            raise FileNotFoundError(f"No neutral csv found for angle {angle} in {NEUTRAL_DIR}")

        # 如果有多个候选，默认用文件名最“长”的那个（通常带 withproperties）
        neutral_candidates = sorted(neutral_candidates, key=len, reverse=True)
        neu_fname = neutral_candidates[0]

        ref_path = os.path.join(REF_DIR, ref_fname)
        neu_path = os.path.join(NEUTRAL_DIR, neu_fname)

        ref_df = pd.read_csv(ref_path)
        neutral_df = pd.read_csv(neu_path)

        # 若 reference 不是刚好 80 行，这里 assert
        if len(ref_df) != 80:
            print(f"[WARN] {ref_fname} has {len(ref_df)} rows (expected 80)")

        out_prefix = os.path.splitext(ref_fname)[0]
        match_neutral_to_reference(ref_df, neutral_df, angle, out_prefix=out_prefix, rng=rng)

if __name__ == "__main__":
    main()
