# Slide generator: paired header + health-summary PNGs per region, written to
# output/slide_content/<region>/{headers,health_summaries}/ with a manifest CSV.
# generate_region_slide_assets() clears each region's old PNGs first (clean = TRUE),
# so no manual file.remove() is needed. Run after main.R (needs op_list).

generate_region_slide_assets(op_list, "APJ")

generate_region_slide_assets(op_list, c("West", "East"))
