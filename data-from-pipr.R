library(tidyverse)
dt_pipr_fg <- pipr::get_stats(fill_gaps = T, server = "dev")
fst::write.fst(dt_pipr_fg, 
               path = fs::path("dt_pipr_fg.fst"))

pipversion <- pipr::get_versions(server = "dev") |> 
  pull(version) |> 
  _[][[1]] |> 
  substr(start = 1, 
         13)
qs::qsave(x = pipversion, 
          file = fs::path("pipversion.qs"))
