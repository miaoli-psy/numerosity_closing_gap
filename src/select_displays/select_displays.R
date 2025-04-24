#从生成的原始displays中，选出需要的sector_angle和direction。每一个条件下只保留
#需要的nuemrosity（这些是numerosity分布中出现得最多的几个）

setwd("d:/OneDrive/projects/numerosity_closing_gap/displays/displays_n5000/")

folder_path <- "d:/OneDrive/projects/numerosity_closing_gap/displays/displays_n5000/"

# Parameters to filter (can be vectors)
arrangement <- c("radial", "tangential")  
sector_angle <- c(30, 60, 90, 120, 170)                
direction <- c(0, 180)    

# Allowed numerosity per sector angle
numerosity_filter <- list(
  `30` = c(6, 9, 12, 15),
  `60` = c(12, 15, 18, 9),
  `90` = c(18, 21, 24, 15, 27),
  `120` = c(24, 27, 21, 30, 18),
  `170` = c(33, 36, 30, 39, 27)
)

# List all csv files in the folder
all_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize matched files
matched_files <- c

# Check all combinations of A, B, C
for (A in arrangement) {
  for (B in sector_angle) {
    for (C in direction) {
      file <- sprintf("ws1_%s_angle%d_drctn%d\\.csv", A, B, C)
      matched <- grep(file, all_files, value = TRUE)
      matched_files <- c(matched_files, matched)
    }
  }
}

save_folder <-"d:/OneDrive/projects/numerosity_closing_gap/displays/displays/"

# Read files
# add cols, filter numerosity, and save
data_list <- lapply(matched_files, function(file_path) {
  df <- read.csv(file_path)
  
  # 提取文件名中的条件信息
  file_name <- basename(file_path)
  match <- regexec("ws1_(radial|tangential)_angle(\\d+)_drctn(\\d+)\\.csv", file_name)
  parsed <- regmatches(file_name, match)[[1]]
  
  arrangement <- parsed[2]
  angle <- as.numeric(parsed[3])
  direction <- as.numeric(parsed[4])
  
  # new cols 
  df$arrangement <- arrangement
  df$sector_angle <- angle
  df$visual_field <- direction
  
  # Filter numerosity based on sector_angle
  if (as.character(angle) %in% names(numerosity_filter)) {
    allowed_values <- numerosity_filter[[as.character(angle)]]
    df <- df[df$numerosity %in% allowed_values, ]
  }
  
  # Save to new location
  save_path <- file.path(save_folder, file_name)
  write.csv(df, save_path, row.names = FALSE)
  
  return(df)
})


