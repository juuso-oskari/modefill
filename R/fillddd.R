#' Fill missing DDD values
#'
#' Fills the missing DDD values by finding comparative purchases (same vnr) from the data and calculating their DDD / cost. New DDD is then just this times the purchase cost.
#' Comparative purchases are found following logic: 1. search current interval where prices should have stayed the same (intervals are 1.-14. and 15.-30. of the month), and calculate the mode
#' DDD / cost. If no comparative purchases found, see for the latest active mode from previous intervals. How long a mode stays active depends on parameter actv_time (given in days). If
#' multiple values with themax frequency, calculate the median of these max frequency values.
#' @import data.table
#' @importFrom DescTools Mode
#' @importFrom stats median
#' @importFrom tidyr fill
#' @importFrom dplyr group_by mutate ungroup
#' @param purch_data data frame of the drug purchase data
#' @param actv_time how long a value stays active to be able to predict from
#' @param pvmvar purchase date variable name in the data frame
#' @param vnrovar drug code variable name in the data frame
#' @param kustvar cost variable name in the data frame
#' @param dddvar ddd variable name in the data frame
#' @param dispatch_rm remove dispatch cost
#' @param extra_rm remove all the added variables from the returned data
#' @param print_info prints info of all the predictions made
#' @param info_price show package cost in info instead of DDD / cost
#' @return data frame with filled DDD values
#' @export
fillddd <- function(purch_data, actv_time = 60, pvmvar = "otpvm", vnrovar = "vnr", kustvar = "kust", dddvar = "ddd", dispatch_rm = TRUE, extra_rm = TRUE, rstr_ord = FALSE, print_info = FALSE, info_price = TRUE){
  dt <- setDT(purch_data)
  dt[, ddd:=as.numeric(get(dddvar))]
  dt[, vnr:=as.numeric(get(vnrovar))]
  dt[, kust:=as.numeric(get(kustvar))]
  dt[, otpvm:=as.Date(get(pvmvar), format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))]
  if(dispatch_rm){
    # remove dispatching cost from purchase cost
    dt[otpvm<as.Date("2014-01-01") & (kust-43 > 0), kust:=(kust-43)]
    dt[otpvm>=as.Date("2014-01-01") & (kust-239 > 0), kust:=(kust-239)]
  }
  dt$index <- seq.int(nrow(dt)) # to restore the original order at the end of function
  setkeyv(dt, c("vnr", "otpvm", "ddd")) # orders the table and sets the ordering variables as keys (for faster computation in future)
  dt[, predicted := is.na(ddd)]
  dt[, dddperkust := ddd / kust]
  # first we want to find out if there is a match on the interval where the price should remain constant (1.-14. and 15.-31.) and fill with that dddperkust value
  # after that, if there are still missing dddperkust values, fill with latest active value
  dt[, day := format(as.Date(otpvm,format="%Y-%m-%d"), format = "%d")][, month := format(as.Date(otpvm,format="%Y-%m-%d"), format = "%m")][, day_group := ifelse(day < 15, 1, 2)][, year:=format(as.Date(otpvm,format="%Y-%m-%d"), format = "%Y")]
  # count the modes for intervals where the price should have stayed the same
  # and fill with the mode if dddperkust is na
  getmode <- function(v) {
    modes <- DescTools::Mode(v)
    if(length(modes)>1){
      median(modes)
    }else{
      modes[1]
    }
  }
  dt <- dt %>%
    dplyr::group_by(vnr, year, month, day_group) %>%
    mutate(mode = getmode(dddperkust), dddperkust = ifelse(is.na(dddperkust), mode, dddperkust)) %>%
    dplyr::ungroup()
  # then fill with latest active mode (parameter actv_time dictates how long a mode stays active)
  dt <- dt %>%
    dplyr::group_by(vnr) %>%
    mutate(priordate = ifelse(is.na(dddperkust), NA, otpvm)) %>%
    fill(priordate, .direction = "down") %>%
    mutate(diff1 = as.numeric(difftime(otpvm, as.Date(priordate, origin="1970-01-01"), units = "days"))) %>%
    fill(mode, .direction = "down") %>%
    mutate(dddperkust = ifelse(is.na(dddperkust) & diff1 < actv_time & !is.na(diff1), mode, dddperkust)) %>%
    dplyr::ungroup()

  dt <- setDT(dt)
  mi <- which(is.na(dt$ddd))
  # do the actual filling
  dt[mi,]$ddd <-  dt[mi,]$kust * dt[mi,]$dddperkust
  # following is just to print some information about the fills
  if(print_info){
    temp <- copy(dt)
    if(info_price){
      temp[, aika_alku := fifelse(!predicted, otpvm, as.Date(NA))][, hinta_ala := ifelse(!predicted, kust/plkm, NA)]
    }else{
      temp[, aika_alku := fifelse(!predicted, otpvm, as.Date(NA))][, hinta_ala := ifelse(!predicted, dddperkust, NA)]
    }
    temp[, aika_loppu := aika_alku][, hinta_yla := hinta_ala]
    temp <- temp %>%
      dplyr::group_by(vnr) %>%
      fill(aika_alku, hinta_ala, .direction = "down") %>%
      dplyr::ungroup()

    temp <- temp %>%
      dplyr::group_by(vnr) %>%
      fill(aika_loppu, hinta_yla, .direction = "up") %>%
      dplyr::ungroup()

    temp <- setDT(temp)
    predictions <<- temp[mi, .SD, .SDcols = c("vnr", "ddd", "hinta_ala", "hinta_yla", "aika_alku", "aika_loppu", "otpvm")]
  }
  # remove added variables
  if(extra_rm){
    dt[,c("day", "month", "day_group", "year", "predicted", "mode"):=NULL]
  }
  # restore to original order
  if(rstr_ord){
    dt <- dt[order(index)]
  }
  dt[, index:=NULL]
  dt
}
