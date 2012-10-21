pro taylorDiagTest

;**********************************
; All cross correlation values are 0.0 to 1.0 [inclusive]
;**********************************

; "p" dataset
                   
  p_rat    = [1.230, 0.988, 1.092, 1.172, 1.064, 0.966, 1.079, 0.781]

  p_cc     = [0.958, 0.973, 0.740, 0.743, 0.922, 0.982, 0.952, 0.433]
  
; "t" dataset

  t_rat    = [1.129, 0.996, 1.016, 1.134, 1.023, 0.962, 1.048, 0.852]
  
  t_cc     = [0.963, 0.975, 0.801, 0.814, 0.946, 0.984, 0.968, 0.647]

  nDataSets  = 2                               ; number of datasets
  
  npts       = size(p_rat, /dim)
  
  ratio      = fltarr(nDataSets, npts)
  
  cc         = fltarr(nDataSets, npts)

  ratio[0,*] = p_rat
  
  cc[0,*]    = p_cc 

  ratio[1,*] = t_rat
  
  cc[1,*]    = t_cc
   
;**********************************
; create plot
;**********************************

  var = ["SLP","Tsfc","Prc","Prc 30S-30N","LW","SW","U300","Guess"]
   
  oWindow = obj_new('IDLgrWindow', dimensions=[900, 900], title='Taylor')

  opts   = {fontSize:18, normalizedSD:0, refRad:1.0, stnRad:[0.25, 0.5, 0.75, 1.25], centerDiffRMS:1, ccRays:[0.6, 0.9], $
            caseLabels:['Case A', 'Case B'], mainTitle:'Example Taylor diagram', varLabels:var}

  plot  = taylor_diagram(ratio, cc, opts)

  oWindow->Erase
  
  oWindow->Draw, plot
    
end
