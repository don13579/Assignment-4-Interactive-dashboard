# Run this from your project root AFTER shinylive::export()
# It patches plotly.js inside the tgz to fix "Plotly is not defined" in shinylive

tgz <- "docs/shinylive/webr/packages/plotly/plotly_4.12.0.tgz"
stopifnot(file.exists(tgz))

tmp <- file.path(tempdir(), "plotly_patch")
dir.create(tmp, showWarnings = FALSE)

# 1. Unpack
untar(tgz, exdir = tmp)
js_path <- file.path(tmp, "plotly", "htmlwidgets", "plotly.js")
stopifnot(file.exists(js_path))

# 2. Read
js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

# Verify we have the unpatched version
if (grepl("waitForPlotly", js)) {
  message("Already patched — nothing to do.")
  quit(save = "no")
}

# 3. Patch A: resize() — guard Plotly.relayout so it doesn't crash
#    when Plotly.js hasn't loaded yet
js <- sub(
  "resize: function\\(el, width, height, instance\\) \\{\n    if \\(instance\\.autosize\\) \\{",
  "resize: function(el, width, height, instance) {\n    if (instance.autosize && window.Plotly) {",
  js
)

# 4. Patch B: renderValue() — defer the whole render until window.Plotly exists
old_render <- "    // if no plot exists yet, create one with a particular configuration\n    if (!instance.plotly) {\n      \n      var plot = Plotly.newPlot(graphDiv, x);"

new_render <- "    // if no plot exists yet, create one with a particular configuration
    // SHINYLIVE FIX: plotly-latest.min.js loads async in WebR; defer until ready
    if (!window.Plotly) {
      var _retries = 0;
      var _waitForPlotly = setInterval(function() {
        _retries++;
        if (window.Plotly) {
          clearInterval(_waitForPlotly);
          var binding = HTMLWidgets.find('#' + el.id);
          if (binding) binding.renderValue(el, x, instance);
        }
        if (_retries > 40) clearInterval(_waitForPlotly);
      }, 500);
      return;
    }
    if (!instance.plotly) {
      
      var plot = Plotly.newPlot(graphDiv, x);"

js <- sub(old_render, new_render, js, fixed = TRUE)

# 5. Verify both patches landed
stopifnot(grepl("window.Plotly", js))
stopifnot(grepl("waitForPlotly", js))
message("Patches applied OK")

# 6. Write patched file back
writeLines(js, js_path)

# 7. Repack — must cd into tmp so paths inside tgz are relative (plotly/...)
owd <- setwd(tmp)
tar(
  tarfile  = "plotly_4.12.0.tgz",
  files    = "plotly",
  compression = "gzip",
  tar      = "internal"   # use R's built-in tar, not system tar
)
setwd(owd)

# 8. Copy back over the original
file.copy(
  file.path(tmp, "plotly_4.12.0.tgz"),
  tgz,
  overwrite = TRUE
)

# 9. Quick sanity check — re-extract and confirm patch is present
tmp2 <- file.path(tempdir(), "plotly_verify")
dir.create(tmp2, showWarnings = FALSE)
untar(tgz, exdir = tmp2)
verify <- paste(readLines(file.path(tmp2, "plotly", "htmlwidgets", "plotly.js"), warn = FALSE), collapse = "\n")

if (grepl("waitForPlotly", verify)) {
  message("SUCCESS: patch confirmed inside tgz — commit and push docs/")
} else {
  stop("FAILED: patch not found after repacking — check tar permissions")
}