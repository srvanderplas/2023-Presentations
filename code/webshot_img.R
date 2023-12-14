output_file <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")

webshot2::webshot(
  url = sprintf(
    "file:////%s",
    normalizePath(sub("\\.qmd$", ".html", output_file))
  ),
  file = sprintf("%s.png", output_file),
  vwidth = 1920,
  vheight = 1005
)
