indikator = "5.3.1.(a)"

library(spatstat)

dt_ssnIND[
   get(jenis_kel) == 2 & 
   get(umur) >= 25 & get(umur) <= 49 &
   get(stat_kawin) == 2,
   
   .(
      MS = matrixStats::weightedMedian(get(kawin_pertama), get(bobot)),
      iso = isotone::weighted.median(get(kawin_pertama), get(bobot)),
      ss = spatstat::weighted.median(get(kawin_pertama), get(bobot))
   ), by = prov
][, weighted.mean(PHB_15, get(bobot), na.rm = TRUE)]

