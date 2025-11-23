import os
import pandas as pd
import numpy as np

from src.common.process_dataframe import insert_new_col_from_n_cols

def get_ref_type(swap, cond_file):

    if swap == "no":
        return "radial"
    elif swap == "yes":
        return "tangential"

    # also check if cond_file is a valid string before searching it.
    elif pd.notna(cond_file) and isinstance(cond_file, str):
        if 'radial_ref' in cond_file:
            return 'radial'
        elif 'tangential_ref' in cond_file:
            return 'tangential'

    return None

def get_probe_type(ref_type):
    if ref_type == "radial":
        return "tangential"
    elif ref_type == "tangential":
        return "radial"
    else:
        return None


def get_stimulus_from_response(reference_type, cond_file, swap_type, response_key):
    """
    Determines if the participant chose the probe or reference stimulus
    based on their keypress and the on-screen locations.
    Returns 'chose_probe' or 'chose_reference'.
    """
    """
    Determines the participant's choice using the best available information.
    It prioritizes the 'reference_type' column and falls back to the 'cond_file' column.
    Returns 'chose_probe' or 'chose_reference'.
    """
    probe_identity = None

    # --- INTELLIGENT DECISION POINT ---
    # 1. First, try to use the 'reference_type' column.
    #    pd.notna() checks for missing values like None or NaN.
    if pd.notna(reference_type) and isinstance(reference_type, str):
        if reference_type == 'radial':
            probe_identity = 'tangential'
        elif reference_type == 'tangential':
            probe_identity = 'radial'

    # 2. If that didn't work, fall back to parsing the 'cond_file' column.
    elif pd.notna(cond_file) and isinstance(cond_file, str):
        if 'radial_ref' in cond_file:
            probe_identity = 'tangential'
        elif 'tangential_ref' in cond_file:
            probe_identity = 'radial'

    # If we couldn't determine the probe's identity, we can't proceed for this row.
    if probe_identity is None:
        return None

    # --- The rest of the logic is the same as before ---
    # Determine the probe's on-screen location
    probe_location = None
    if swap_type == 'r→t':
        probe_location = 'right' if probe_identity == 'tangential' else 'left'
    elif swap_type == 't→r':
        probe_location = 'left' if probe_identity == 'tangential' else 'right'
    else:
        return None

    # Compare response to probe location
    if response_key == probe_location:
        return 'chose_probe'
    else:
        return 'chose_reference'


if __name__ == '__main__':
    to_csv = False
    PATH = "../../data/closinggap_2afc/"

    files = os.listdir(PATH)

    # read raw data files
    csv_files = [os.path.join(PATH, file) for file in files if file.endswith(".csv")]
    dataframe_list = [pd.read_csv(file_name) for file_name in csv_files]

    # combine
    totalData = pd.concat(dataframe_list, ignore_index=True)

    # col names
    # all_column_names = totalData.columns.tolist()

    # keep cols
    columns_to_keep = [
        'numerosity_limited_t',
        'numerosity_limited_r',
        'spacing_in_deg',
        'cond_file',
        'key_resp_2.keys',
        'key_resp_2.rt',
        'sector_angle_r',
        'swap_r',
        'blocks.thisN',
        'thisTrialN',
        'swap_type',
        'participant',
        'sex',
        'age']

    totalData = totalData[columns_to_keep]

    # add reference_type (reference display arrangement)
    insert_new_col_from_n_cols(totalData, ["swap_r", "cond_file"], "reference_type", get_ref_type)

    # add probe_type
    insert_new_col_from_n_cols(totalData, ["reference_type"], "probe_type", get_probe_type)

    # add participant response
    insert_new_col_from_n_cols(totalData, ["reference_type", "cond_file","swap_type", "key_resp_2.keys"], "choice_display_more", get_stimulus_from_response)

    # remove practice trials
    totalData = totalData.dropna(subset=['choice_display_more'])

    # add reference_num and probe_num

    # Condition to check
    is_tangential_ref = (totalData['reference_type'] == 'tangential')

    # 'reference_num'
    totalData['reference_num'] = np.where(
        is_tangential_ref,
        totalData['numerosity_limited_t'],
        totalData['numerosity_limited_r']
    )
    totalData['probe_num'] = np.where(
        is_tangential_ref,
        totalData['numerosity_limited_r'],
        totalData['numerosity_limited_t']
    )

    # clean up
    cols_to_drop = [
        'swap_r',
        'swap_type',
        'cond_file',
        'numerosity_limited_t',
        'numerosity_limited_r',
        'key_resp_2.keys'
    ]

    used_cols = [col for col in cols_to_drop if col in totalData.columns]
    totalData = totalData.drop(columns=used_cols)

    # rename
    totalData = totalData.rename(columns={'key_resp_2.rt': 'RT'})
    totalData = totalData.rename(columns={'sector_angle_r': 'sector_angle'})

    # sector_angle replace Nan with 0 --> RM blocks will has sector_angle of 0
    totalData['sector_angle'] = totalData['sector_angle'].fillna(0)

    if to_csv:
        totalData.to_csv('data_closinggap_2afc.csv', index = False)

