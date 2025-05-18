run combine_raw.py --> 更新displays_n5000或displays_n200000中的raw displays （旧的+新运行的）

run select_displays.R --> 从最原始的算法中生成的raw displays选择需要的条件，得到需要条件下的.csv
run get_all_displays.py --> 确保所有的点都在sector里，因为base posi保证在里面，但是extra没有sector的限制-所以要重新确认全部在sector里
run check_numerosities.py --> 查看在点的位置全部都留在sector之后，点数量的分布
run check_displays_properties.py -->只留下可能的numerosity和sector_angle。得到displays_withproperties.csv 每一行都包含各种properties
run match_properties_ran_parallel.py --> 生成需要的displays，confounding factors are matched - if no successful match, use fallback strategy

run fine_matching.py --> 把match_properties_ran_parallel.py中生成的display进行对比，统计检验，输出每组（e.g. tangental 40 vs. matching radial 40）
                        对于每个propoerties的均值，t和p
