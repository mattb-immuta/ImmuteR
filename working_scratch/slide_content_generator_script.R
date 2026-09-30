#slide generator


file.remove(list.files("output/slide_content/headers/", full.names = TRUE))
file.remove(list.files("output/slide_content/health_summaries/", full.names = TRUE))

generate_region_slide_assets(op_list, "EMEA")

generate_region_slide_assets(op_list, c("West", "East"))
