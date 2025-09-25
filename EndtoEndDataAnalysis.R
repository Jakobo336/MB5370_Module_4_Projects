#End to End Data Analysis in R
#I will be using the QFish Commercial Line Fisheries data from 1990 to 2014

setwd("/Users/jacobdouglass/Downloads/GITHUB MB5370 STUFF/MB5370_Module_4_Projects")

pkgs <- c("tidyverse","readr","janitor","scales","glue","fs")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, Ncpus = 2)
invisible(lapply(pkgs, library, character.only = TRUE))

# 1) Paths
data_path <- if (file.exists("export.csv")) "export.csv" else if (file.exists("code/export.csv")) "code/export.csv" else {message("Pick CSV…"); file.choose()}
out_dir  <- if (dir.exists("../figs")) "../figs" else {fs::dir_create("figs"); "figs"}
tidy_wide_path <- if (dir.exists("../data")) "../data/commercial_line_tidy_wide.csv" else "commercial_line_tidy_wide.csv"
tidy_long_path <- if (dir.exists("../data")) "../data/commercial_line_tidy_long.csv" else "commercial_line_tidy_long.csv"

cat(glue("Reading: {fs::path_abs(data_path)}\nSaving figures to: {fs::path_abs(out_dir)}\n"))

# 2) READ (raw)
raw <- readr::read_csv(data_path, show_col_types = FALSE) |>
  janitor::clean_names()
expected <- c("calendar_year","licences","days","tonnes")
missing_cols <- setdiff(expected, names(raw))
if (length(missing_cols)) stop(glue("Missing columns in CSV: {paste(missing_cols, collapse=', ')}"))

# 3) TIDY (no warnings; safely drops "Grand Total" / "2024 incomplete")
tidy_wide <- raw %>%
  mutate(
    year_text = as.character(calendar_year),
    year_num  = as.integer(stringr::str_extract(year_text, "\\d{4}"))
  ) %>%
  filter(!is.na(year_num)) %>%
  transmute(
    calendar_year = year_num,
    licences = readr::parse_number(as.character(licences)),
    days     = readr::parse_number(as.character(days)),
    tonnes   = readr::parse_number(as.character(tonnes))
  ) %>%
  group_by(calendar_year) %>%
  summarise(
    licences = sum(licences, na.rm = TRUE),
    days     = sum(days,     na.rm = TRUE),
    tonnes   = sum(tonnes,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(calendar_year) %>%
  mutate(
    cpue_t_per_day = dplyr::if_else(days > 0, tonnes / days, NA_real_)
  )

# Validate tidy shape
stopifnot(!any(is.na(tidy_wide$calendar_year)))
dups <- tidy_wide |> count(calendar_year) |> filter(n > 1)
if (nrow(dups)) stop("Duplicate years remain after summarise — inspect input.")

# LONG (tidy) version for multi-metric plotting
tidy_long <- tidy_wide |>
  pivot_longer(
    cols = c(tonnes, days, licences),
    names_to = "metric", values_to = "value"
  ) |>
  mutate(
    metric = factor(metric, levels = c("tonnes","days","licences"),
                    labels = c("Catch (tonnes)","Effort (days)","Licences"))
  )

# Write tidy artifacts for the assignment
readr::write_csv(tidy_wide, tidy_wide_path)
readr::write_csv(tidy_long, tidy_long_path)
cat(glue("Wrote tidy CSVs:\n - {fs::path_abs(tidy_wide_path)}\n - {fs::path_abs(tidy_long_path)}\n"))

# 4) Quick checks
print(glimpse(tidy_wide))
print(tidy_wide |>
        summarise(
          year_min = min(calendar_year), year_max = max(calendar_year),
          total_t  = sum(tonnes, na.rm=TRUE), total_days = sum(days, na.rm=TRUE),
          mean_cpue = mean(cpue_t_per_day, na.rm=TRUE)
        ))

# 5) Plot: CPUE over time
p_cpue <- ggplot(tidy_wide, aes(calendar_year, cpue_t_per_day)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.9) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "Commercial Line CPUE over time",
    subtitle = glue("CPUE = Tonnes / Days  |  Years: {min(tidy_wide$calendar_year)}–{max(tidy_wide$calendar_year)}"),
    x = "Year", y = "CPUE (tonnes/day)",
    caption = "Source: QFISH (Queensland Government)."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_cpue)

# 6) Plot: Catch, Effort, Licences (small multiples)
p_multi <- ggplot(tidy_long, aes(calendar_year, value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Commercial Line fishery: catch, effort, and licences",
    x = "Year", y = NULL,
    caption = "Source: QFISH (Queensland Government)."
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
print(p_multi)
