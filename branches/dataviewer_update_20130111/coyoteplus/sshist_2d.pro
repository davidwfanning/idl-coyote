; Author: Shigenobu Hirose at JAMSTEC
; based on original paper
; Shimazaki and Shinomoto, Neural Computation 19, 1503-1527, 2007
;
function sshist_2d, data1, data2, x1=x1, x2=x2, cost=cost, nbin=nbin

  COMPILE_OPT idl2

  nbin_min = 2
  nbin_max = 200
  
  ntrial = nbin_max - nbin_min + 1
  
  nbin   = INDGEN(ntrial) + nbin_min
  
  delta1 = FLTARR(ntrial)
  delta2 = FLTARR(ntrial)
  cost   = FLTARR(ntrial)
  
  for n = 0, ntrial-1  do begin
     delta1[n] = (MAX(data1) - MIN(data1)) / (nbin[n] - 1) * (1. - (MACHAR()).EPS)
     delta2[n] = (MAX(data2) - MIN(data2)) / (nbin[n] - 1) * (1. - (MACHAR()).EPS)
     
     k = HIST_2D(data1, data2, bin1=delta1[n], bin2=delta2[n], min1=MIN(data1), min2=MIN(data2))
     
     kmean = MEAN(k)
     kvari = MEAN((k - kmean)^2)
     cost[n] = (2. * kmean - kvari) / (delta1[n] * delta2[n])^2
  endfor
  
  n = (WHERE(cost eq MIN(cost)))[0]
  k = HIST_2D(data1, data2, bin1=delta1[n], bin2=delta2[n], min1=MIN(data1), min2=MIN(data2))
  
  x1 = FINDGEN(nbin[n]) * delta1[n] + MIN(data1)
  x2 = FINDGEN(nbin[n]) * delta2[n] + MIN(data2)

  return, k
  
end
