
import os
import pandas as pd
import ast
from src.common.process_dataframe import insert_new_col, insert_new_col_from_n_cols
from src.common.process_displays import limit_posi_to_sector, get_len

# set current working directory
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# read displays .csv
PATH = "../../displays/displays/"
dir_list = os.listdir(PATH)

# needed cols
needed_cols = ['sector_angle', 'visual_field', 'extraposis', 'centralposis', 'numerosity', 'arrangement']

displays_df = [pd.read_csv(os.path.join(PATH, file), usecols = needed_cols)
               for file in dir_list if file.endswith(".csv")]

# into one
displays = pd.concat(displays_df, ignore_index = True)

# position str to list
# displays['allposis'] = displays['allposis'].apply(ast.literal_eval)
displays['centralposis'] = displays['centralposis'].apply(ast.literal_eval)
displays['extraposis'] = displays['extraposis'].apply(ast.literal_eval)


# extra_posits might be outside the sector - limited it
insert_new_col_from_n_cols(displays, ['sector_angle', 'visual_field', 'extraposis'], 'extraposis_limited', limit_posi_to_sector)

# add number of extra positions after limited them into sector
insert_new_col(displays, 'extraposis_limited', 'len_extra_posi', get_len)

# cal numerosity
displays['numerosity_limited'] = displays['numerosity']/3 + displays['len_extra_posi']


# displays.to_csv("displays.csv", index=False)