import pandas as pd

def insert_new_col(input_df: pd.DataFrame, old_col: str, new_col: str, func_name):
    if old_col in input_df.columns:
        col_index = input_df.columns.get_loc(old_col)
        input_df.insert(col_index, new_col, input_df[old_col].map(func_name))
    else:
        raise Exception(f"Warning: missing {old_col}")

def insert_new_col_from_three_cols(input_df: pd.DataFrame, col1: str, col2: str, col3: str, new_col: str, func):
    cols = input_df.columns
    if all(c in cols for c in [col1, col2, col3]):
        input_df[new_col] = input_df.apply(lambda x: func(x[col1], x[col2], x[col3]), axis=1)
    else:
        raise Exception(f"Missing one of the required columns: {col1}, {col2}, or {col3}")

def insert_new_col_from_n_cols(input_df: pd.DataFrame, old_cols: list, new_col: str, func):
    missing = [col for col in old_cols if col not in input_df.columns]
    if missing:
        raise Exception(f"Missing columns: {', '.join(missing)}")

    input_df[new_col] = input_df[old_cols].apply(lambda row: func(*row), axis=1)

