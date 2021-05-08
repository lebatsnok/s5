# s5

R package to score s5

Install: `devtools::install_github("lebatsnok/s5")`

Use:  `score2(my_data_file[, S5_columns], key.s5, average=TRUE, hofstee=TRUE)`
* set `average=TRUE` to  computes scores as averages [as opposed to sums]
* set `hofstee=TRUE`  to rescale everything to the range [-1; +1]

Other available keys: `xs5.key` (34-item version) and `xs5.key2` (34-item version, updated A4 item)

Updates will follow as needed :)
