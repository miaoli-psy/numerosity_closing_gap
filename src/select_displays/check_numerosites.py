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

# remove displays if numerosity cannot be used
all_displays = all_displays[all_displays['sector_angle'].isin([40, 60, 90, 120, 170])]


# set the filter - based on sector_angle and numerosity
filter = (
    # For sector_angle 40, keep numerosity 5-17
        ((all_displays['sector_angle'] == 40) &
         (all_displays['numerosity'] >= 5) &
         (all_displays['numerosity'] <= 13)) |

    # For sector_angle 60, keep numerosity 8-17
        ((all_displays['sector_angle'] == 60) &
         (all_displays['numerosity'] >= 8) &
         (all_displays['numerosity'] <= 17)) |


        # For sector_angle 90, keep numerosity 13-22
        ((all_displays['sector_angle'] == 90) &
         (all_displays['numerosity'] >= 13) &
         (all_displays['numerosity'] <= 22)) |

        # For sector_angle 120, keep numerosity 18-28
        ((all_displays['sector_angle'] == 120) &
         (all_displays['numerosity'] >= 18) &
         (all_displays['numerosity'] <= 28)) |

        # For sector_angle 170, keep numerosity 25-39
        ((all_displays['sector_angle'] == 170) &
         (all_displays['numerosity'] >= 25) &
         (all_displays['numerosity'] <= 39))
)

# Apply the filter
all_displays = all_displays[filter]
