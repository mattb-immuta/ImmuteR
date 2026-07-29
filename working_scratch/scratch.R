

account_names=op_list$accounts$`Account Name`
text2write=paste("# ",account_names," Notes",sep="")
filepath="data/account_notes/"
files=list.files(filepath)

write_line_to_markdown_file <- function(file_path, text_to_write, append_mode = FALSE) {
  # Determine the mode for opening the file: "w" for write (overwrite), "a" for append
  open_mode <- if (append_mode) "a" else "w"

  # Use tryCatch for robust error handling during file operations
  tryCatch({
    # Open the file connection
    # 'file()' creates a connection object.
    # 'open = "w"' means write mode (overwrites existing content).
    # 'open = "a"' means append mode (adds to the end of existing content).
    con <- file(file_path, open = open_mode, encoding = "UTF-8")

    # Write the line of text to the file
    # 'writeLines()' writes character strings to a connection.
    # It automatically adds a newline character at the end of each string.
    writeLines(text_to_write, con = con)

    # Close the file connection
    # It's crucial to close file connections to ensure data is written and resources are freed.
    close(con)

    message(paste0("Successfully wrote '", text_to_write, "' to '", file_path, "' (mode: ", open_mode, ")."))

  }, error = function(e) {
    # If an error occurs (e.g., file permissions), print a warning
    warning(paste("Error writing to file", file_path, ":", e$message))
  }, finally = {
    # Ensure the connection is closed even if an error occurs
    if (exists("con") && isOpen(con)) {
      close(con)
    }
  })
}

# --- Example Usage ---

# 1. Define the file path and the text you want to write
markdown_file_path <- "my_markdown_docs/ACME_Notes.md"
title_text <- "# ACME Notes"
first_line_content <- "This is the first line of content for the ACME Notes document."

# Ensure the directory exists before attempting to write the file
# This is good practice to prevent errors if the directory doesn't exist.
target_directory <- dirname(markdown_file_path)
if (!dir.exists(target_directory)) {
  dir.create(target_directory, recursive = TRUE)
  message(paste("Created directory:", target_directory))
}

# Write the title to the file (this will create or overwrite the file)
write_line_to_markdown_file(markdown_file_path, title_text, append_mode = FALSE)

# Now, append another line of text to the same file
write_line_to_markdown_file(markdown_file_path, first_line_content, append_mode = TRUE)

# You can add more lines by calling the function again with append_mode = TRUE
write_line_to_markdown_file(markdown_file_path, "## Section 1", append_mode = TRUE)
write_line_to_markdown_file(markdown_file_path, "- Item 1", append_mode = TRUE)
write_line_to_markdown_file(markdown_file_path, "- Item 2", append_mode = TRUE)

# To verify, you can read the file content (optional)
# file_content <- readLines(markdown_file_path)
# print("Content of the file:")
# print(file_content)



temp=read_parquet("data/idex/accounts.parquet")




qbr_execution
lastqbr=op_list$renewals$`Date of last QBR/EBR`> as.Date("2025-02-01")
nextqbr=op_list$renewals$`Date of next QBR/EBR`< as.Date("2025-08-01")
lastqbr[is.na(lastqbr)]=FALSE
nextqbr[is.na(nextqbr)]=FALSE
op_list$renewals$qbr_compliant=lastqbr|nextqbr

op_list$renewals %>%
  group_by(Region) %>%
  summarize(
    true_complaints = sum(qbr_compliant == TRUE, na.rm = TRUE)
  )
