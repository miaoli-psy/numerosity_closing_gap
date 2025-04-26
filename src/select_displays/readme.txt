run select_displays.R --> 从最原始的算法中生成的raw displays选择需要的条件，得到需要条件下的.csv
run get_all_displays.py --> 确保所有的点都在sector里，因为base posi保证在里面，但是extra没有sector的限制-所以要重新确认全部在sector里
run check_numerosities.py --> 查看在点的位置全部都留在sector之后，点数量的分布
