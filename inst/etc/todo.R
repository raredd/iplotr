## incorporate some rCharts
## add option to save plots in different formats
## add option to open plots in browser instead of viewer

use_viewer <- function() {
  ## this is hacky I suppose
  options(viewer = NULL, browser = Sys.getenv("R_BROWSER"))
}

# // this works but doesnt change the output
if (!use_viewer) {
  oo <- options()
  on.exit(options(oo))
  use_viewer()
}

print(getOption('browser'))
print(getOption('viewer'))
options(device = '')
