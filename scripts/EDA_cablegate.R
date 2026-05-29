library(jsonlite)
path="/Users/matt.benigni/Documents/projects/cables_parsed.jsonl"
dat <- stream_in(file(path))

msg<-dat$body[1]
clean_msg <- msg |>
  gsub('\\\\n', '\n', x = _) |>
  gsub('\\\\\"', '"', x = _) |>
  gsub('[[:space:]]+\n', '\n', x = _) |>
  gsub('\n{3,}', '\n\n', x = _) |>
  trimws()

cat(clean_msg)
