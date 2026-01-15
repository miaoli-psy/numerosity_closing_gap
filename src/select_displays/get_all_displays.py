
import os
import pandas as pd
import ast
from src.common.process_dataframe import insert_new_col, insert_new_col_from_n_cols
from src.common.process_displays import limit_posi_to_sector, get_len, remove_near_origin, \
    remove_outside_radius

# n_cores
n_cores = 1

# set current working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# read displays .csv
PATH = "../../displays/displays/angle170/"
dir_list = os.listdir(PATH)

# needed cols
needed_cols = ['sector_angle', 'visual_field', 'extraposis', 'centralposis', 'numerosity', 'arrangement']

displays_df = [pd.read_csv(os.path.join(PATH, file), usecols = needed_cols)
               for file in dir_list if file.endswith(".csv")]

# into one
displays = pd.concat(displays_df, ignore_index = True)

# position str to list
# displays['allposis'] = displays['allposis'].apply(ast.literal_eval)
# displays['centralposis'] = displays['centralposis'].apply(ast.literal_eval)
# displays['extraposis'] = displays['extraposis'].apply(ast.literal_eval)

# def parallel_literal_eval(column_data, desc="Parsing", max_workers=None):
#     max_workers = max_workers or multiprocessing.cpu_count()
#     with ProcessPoolExecutor(max_workers=max_workers) as executor:
#         results = list(tqdm(executor.map(ast.literal_eval, column_data), total=len(column_data), desc=desc))
#     return results

# displays['centralposis'] = parallel_literal_eval(displays['centralposis'], desc="Parsing centralposis", max_workers = n_cores)
# displays['extraposis'] = parallel_literal_eval(displays['extraposis'], desc="Parsing extraposis", max_workers = n_cores)

# convert position strings to lists
displays['centralposis'] = displays['centralposis'].apply(ast.literal_eval)
displays['extraposis'] = displays['extraposis'].apply(ast.literal_eval)

# remove fovea region
displays['centralposis'] = displays['centralposis'].apply(remove_near_origin)
displays['extraposis'] = displays['extraposis'].apply(remove_near_origin)

# no discs outside 540
displays['centralposis'] = displays['centralposis'].apply(remove_outside_radius)
displays['extraposis'] = displays['extraposis'].apply(remove_outside_radius)

# extra_posits might be outside the sector - limited it
insert_new_col_from_n_cols(displays, ['sector_angle', 'visual_field', 'extraposis'], 'extraposis_limited', limit_posi_to_sector)

# add number of extra positions after limited them into sector
insert_new_col(displays, 'extraposis_limited', 'len_extra_posi', get_len)

# cal numerosity
displays['numerosity_limited'] = displays['numerosity']/3 + displays['len_extra_posi']


displays.to_csv("displays170.csv", index=False)