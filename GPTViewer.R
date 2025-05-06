# Load required libraries
library(stringr)

# List all lec files that match "lecXX.qmd" and NOT already viewer files
lec_files <- list.files(pattern = "^lec\\d+\\.qmd$", full.names = FALSE)

# Create viewer files for each
for (lec_file in lec_files) {
  # Extract base name without extension
  base <- tools::file_path_sans_ext(lec_file)
  viewer_name <- paste0(base, "_viewer.qmd")
  
  # Skip if the viewer file already exists
  if (file.exists(viewer_name)) next
  
  # Generate iframe content
  iframe_html <- sprintf(
    '---
title: "Lecture %s"
format: html
page-layout: full
---

<iframe src="%s.html" width="100%%" height="800px" style="border: none;"></iframe>',
    str_extract(base, "\\d+"),
    base
  )
  
  # Write the file
  writeLines(iframe_html, con = viewer_name)
  cat("Created:", viewer_name, "\n")
}
