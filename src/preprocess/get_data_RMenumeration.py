import os
import pandas as pd

from src.common.process_dataframe import insert_new_col_from_n_cols


def get_resp_n(input_string):
    return int(input_string.split('_')[-1])

def get_deviaion_score(reportN, numerosity):
    return reportN - numerosity


if __name__=='__main__':
    to_csv = False
    PATH = "../../data/enumeration/"

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
        'protectzonetype',
        'numerosity',
        'type',
        'key_resp.rt',
        'key_resp.keys',
        'rotation_index',
        'blocks_rm.thisN',
        'participant',
        'sex',
        'age']

    totalData = totalData[columns_to_keep]

    # remove rows with NaN
    totalData = totalData.dropna(subset= ['key_resp.keys'])

    # add col deviation score
    insert_new_col_from_n_cols(totalData,
                              ["key_resp.keys"],
                              "reportedN", get_resp_n)

    insert_new_col_from_n_cols(totalData,
                              ["reportedN", "numerosity"],
                              "deviation", get_deviaion_score)
    

    if to_csv:
        totalData.to_csv('data_RMenumeration.csv', index = False)

