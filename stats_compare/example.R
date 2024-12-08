two_conds <- tibble::tibble(
  sample = c("sample1", "sample2", "sample3", "sample4", "sample5", "sample6"),
  condition = c("control", "control", "control", "treatment", "treatment", "treatment"),
  var1 = c(5, 10, 15, 50, 55, 60),
  var2 = c(400, 500, 600, 100, 200, 300)
)
ANOVA <- tibble::tibble(
  sample = c("sample1", "sample2", "sample3", "sample4", "sample5", "sample6", "sample7", "sample7", "sample9"),
  condition = c("control", "control", "control", "treatment1", "treatment1", "treatment1", "treatment2", "treatment2", "treatment2"),
  var1 = c(5, 10, 15, 50, 55, 60, 100, 110, 120),
  var2 = c(400, 500, 600, 100, 200, 300, 10, 15, 20)
)
