# Read the gene_info file
library(tidyr)
gene_info <- read.table("Homo_sapiens.gene_info", header = TRUE, sep = "\t", quote = "", comment.char = "", stringsAsFactors = FALSE)
gene_info_split <- separate_rows(gene_info, Synonyms, sep = "\\|")

# Load the gene information file
gene_info <- read.delim("Homo_sapiens.gene_info", stringsAsFactors = FALSE)

# Load the 'h.all.v2023.1.Hs.symbols.gmt' file
gmt_data <- read.delim("h.all.v2023.1.Hs.symbols.gmt", stringsAsFactors = FALSE, header = FALSE)

# Function to replace gene names with GeneID
replace_gene_names <- function(gene_names, gene_info) {
  gene_ids <- character(length(gene_names))
  for (i in seq_along(gene_names)) {
    gene_name <- gene_names[i]
    match_idx <- which(gene_info$Symbol == gene_name | grepl(paste0("\\b", gene_name, "\\b"), gene_info$Synonyms))
    if (length(match_idx) > 0) {
      gene_ids[i] <- gene_info$GeneID[match_idx[1]]
    } else {
      gene_ids[i] <- gene_name
    }
  }
  return(gene_ids)
}

# Apply the function to replace gene names with GeneID in each column of 'gmt_data'
replaced_data <- lapply(gmt_data[, -c(1, 2)], replace_gene_names, gene_info)

# Combine the results into a data frame
replaced_data_df <- as.data.frame(replaced_data)

# Add the first two columns from 'gmt_data' to the replaced data frame
replaced_data_df <- cbind(gmt_data[, 1:2], replaced_data_df)

# Print the updated data frame
#print(replaced_data_df)
# Save the updated data frame as a GMT file
write.table(replaced_data_df, file = "updated_h.all.v2023.1.Hs.symbols.gmt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)





