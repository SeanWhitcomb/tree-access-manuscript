args <- commandArgs(trailingOnly = TRUE)

files <- list.files(
  path = ".",
  pattern = "\\.docx$",
  recursive = TRUE,
  full.names = TRUE
)

files <- files[!grepl("custom-reference\\.docx$", files)]

for (f in files) {
  flextable::repair_docx(f)
}