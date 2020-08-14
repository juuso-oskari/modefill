# modefill
R-package for filling missing DDD values from drug purchase data



Fills the missing DDD values by finding comparative purchases (same vnr) from the data and calculating their DDD / cost. 
New DDD is then just this times the purchase cost. Comparative purchases are found following logic: 1. search current interval where prices should have stayed the same (intervals are 1.-14. and 15.-30. of the month), and calculate the mode.
DDD / cost. If no comparative purchases found, see for the latest active mode from previous intervals. How long a mode stays active depends on parameter actv_time (given in days).
If multiple values with themax frequency, calculate the median of these max frequency values.
Libraries required:
  haven,
  data.table,
  tidyr,
  dplyr,
  DescTools
