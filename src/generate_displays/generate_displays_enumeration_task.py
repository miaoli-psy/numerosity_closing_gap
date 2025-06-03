"""
得到psychopy需要的enumeration task的condition file
每个arrangement保留3,4,5,6各10行每个arrangement
"""

import os
import pandas as pd
import numpy as np
import ast

# === CONFIGURATION ===
save_to_csv = True

# 文件路径设置
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

INPUT_DIR = "../../displays/selected/select_angle15_enumeration_task/"
filename = "displays_angle15.csv"
file_path = os.path.join(INPUT_DIR, filename)

PROCESSED_DIR = os.path.join(script_dir, "merged_numtask")
os.makedirs(PROCESSED_DIR, exist_ok=True)
output_file = f"processed_{filename}"
output_path = os.path.join(PROCESSED_DIR, output_file)

# === 旋转函数 ===
def rotate_point(x, y, theta_deg):
    theta_rad = np.radians(theta_deg)
    cos_theta = np.cos(theta_rad)
    sin_theta = np.sin(theta_rad)
    x_new = cos_theta * x - sin_theta * y
    y_new = sin_theta * x + cos_theta * y
    return (x_new, y_new)

def rotate_positions(pos_list, angle_deg):
    return [rotate_point(x, y, angle_deg) for x, y in pos_list]

# === 检查所有点是否在屏幕可见范围内（考虑圆半径 9.15）===
def all_points_on_screen(pos_list, margin=9.15):
    return all(
        -960 + margin <= x <= 960 - margin and
        -540 + margin <= y <= 540 - margin
        for (x, y) in pos_list
    )

# === STEP 1: 读取原始数据 ===
print(f"loading：{file_path}")
df = pd.read_csv(file_path)

# 删除 visual_field == 180 的行
df = df[df['visual_field'] != 180]

# 解析 central 和 extra 坐标
df['centralposis'] = df['centralposis'].apply(ast.literal_eval)
df['extraposis_limited'] = df['extraposis_limited'].apply(ast.literal_eval)

# 生成 allposis1
df['allposis1'] = df.apply(lambda row: row['centralposis'] + row['extraposis_limited'], axis=1)

# 生成 allposis2 ~ allposis24
for i in range(2, 25):
    angle = (i - 1) * 15
    df[f'allposis{i}'] = df['allposis1'].apply(lambda posis: rotate_positions(posis, angle))

# === STEP 2: 过滤所有有任何越界坐标的行 ===
for i in range(1, 25):
    df = df[df[f'allposis{i}'].apply(all_points_on_screen)]

# === STEP 3: 抽样每类 numerosity ===
filtered_df = pd.DataFrame()
for arr in ['radial', 'tangential']:
    subset = df[(df['arrangement'] == arr) & (df['numerosity_limited'].isin([3, 4, 5, 6]))]
    sampled = subset.groupby('numerosity_limited').apply(
        lambda g: g.sample(n=10, random_state=42)
    ).reset_index(drop=True)
    filtered_df = pd.concat([filtered_df, sampled], ignore_index=True)

df = filtered_df

# 删除 visual_field 列
if 'visual_field' in df.columns:
    df = df.drop(columns=['visual_field'])

# === STEP 4: 保存清洗后的 CSV 文件 ===
if save_to_csv:
    df.to_csv(output_path, index=False)
    print(f"saved：{output_path}")