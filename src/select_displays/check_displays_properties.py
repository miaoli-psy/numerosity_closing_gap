import sys
import os
import pandas as pd
import ast
from src.common.process_dataframe import insert_new_col

sys.path.append(r'D:\OneDrive\Programming\crwdngnmrsty_displays')

from src.properties import Properties

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# read all displays
file = 'displays.csv'
all_displays = pd.read_csv(os.path.join(script_dir, file))

# remove displays if numerosity cannot be used
all_displays = all_displays[all_displays['sector_angle'].isin([40, 60, 90, 120, 170])]


# set the filter - based on sector_angle and numerosity
filter = (
    # For sector_angle 40, keep numerosity 5-17
        ((all_displays['sector_angle'] == 40) &
         (all_displays['numerosity_limited'] >= 5) &
         (all_displays['numerosity_limited'] <= 13)) |

    # For sector_angle 60, keep numerosity 8-17
        ((all_displays['sector_angle'] == 60) &
         (all_displays['numerosity_limited'] >= 8) &
         (all_displays['numerosity_limited'] <= 17)) |


        # For sector_angle 90, keep numerosity 13-22
        ((all_displays['sector_angle'] == 90) &
         (all_displays['numerosity_limited'] >= 13) &
         (all_displays['numerosity_limited'] <= 22)) |

        # For sector_angle 120, keep numerosity 18-28
        ((all_displays['sector_angle'] == 120) &
         (all_displays['numerosity_limited'] >= 18) &
         (all_displays['numerosity_limited'] <= 28)) |

        # For sector_angle 170, keep numerosity 25-39
        ((all_displays['sector_angle'] == 170) &
         (all_displays['numerosity_limited'] >= 25) &
         (all_displays['numerosity_limited'] <= 39))
)

# Apply the filter
all_displays = all_displays[filter]


all_displays['centralposis'] = all_displays['centralposis'].apply(ast.literal_eval)
all_displays['extraposis_limited'] = all_displays['extraposis_limited'].apply(ast.literal_eval)


all_displays['all_posis_new'] = all_displays.apply(
    lambda row: row['centralposis'] + row['extraposis_limited'], axis=1)


def calculate_properties(positions_list):
    try:
        props = Properties(positions_list)
        return {
            'density': props.density,
            'convexhull': props.convexhull,
            'occupancy_area': props.occupancy_area,
            'average_eccentricity': props.averge_eccentricity,
            'average_spacing': props.average_spacing,
        }
    except Exception as e:
        print(f"Error calculating properties: {e}")
        return {
            'density': None,
            'convexhull': None,
            'occupancy_area': None,
            'average_eccentricity': None,
            'average_spacing': None,
        }

# takes about 30min
properties_df = all_displays['all_posis_new'].apply(calculate_properties).apply(pd.Series)

all_displays = pd.concat([all_displays, properties_df], axis=1)


# all_displays.to_csv("displays_withproperties.csv", index=False)