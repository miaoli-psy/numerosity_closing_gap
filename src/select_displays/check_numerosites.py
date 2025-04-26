# read displays from get_all_displays.py

import os
import pandas as pd


# set current working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# read all displays
file = 'displays.csv'
all_displays = pd.read_csv(os.path.join(script_dir, file))

# check numerosities

pivot = pd.pivot_table(
    all_displays,
    index=['sector_angle', 'numerosity_limited'],
    columns=['arrangement', 'visual_field'],
    values=['numerosity'],  # Using any column, just need something to count
    aggfunc='count',
    fill_value=0
)
