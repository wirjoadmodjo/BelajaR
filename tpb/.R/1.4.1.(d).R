indikator = "1.4.1.(d)"

## untuk data tahun 2015 dan 2016
m2015 = c("2015", "2016")
if (any(m2015 == vg_tahundata)) {
   #  rincian air minum terlindungi
   ## Leding meteran, Leding eceran, Air Hujan
   ar_minum = c(3,4,11)
   
   #  rincian air terlindung dengan jarak >= 10m
   ## Sumur bor/pompa, Sumur terlindung, Mata air terlindung
   ar_minum_jarak = c(5,6,8)
   
   #  rincian air minum tidak sustain
   ## Air kemasan bermerk, Air isi ulang
   ar_tdk_sustain = c(1,2)
   
   #  rincian air minum tidak tidak terlindung
   ## Sumur tak terlindung, Mata air tak terlindung, Air Sungai, lainnya
   ar_tdk_terlindung = c(7,9,10,12)
   
   #  rincian air mandi/cuci yan terlindung
   ## Leding meteran, Leding eceran, Sumur bor/pompa, Sumur terlindung
   ## Mata air terlindung, Air Hujan
   ar_mandi = c(3,4,5,6,8,11)
}
rm("m2015")


## untuk data tahun 2017
m2017 = c("2017")
if (any(m2017 == vg_tahundata)) {
   #  rincian air minum terlindungi
   ## Leding, Air Hujan
   ar_minum = c(3,10)
   
   #  rincian air terlindung dengan jarak >= 10m
   ## Sumur bor/pompa, Sumur terlindung, Mata air terlindung
   ar_minum_jarak = c(4,5,7)
   
   #  rincian air minum tidak sustain
   ## Air kemasan bermerk, Air isi ulang
   ar_tdk_sustain = c(1,2)
   
   #  rincian air minum tidak tidak terlindung
   ## Sumur tak terlindung, Mata air tak terlindung, Air Sungai, lainnya
   ar_tdk_terlindung = c(6,8,9,11)
   
   #  rincian air mandi/cuci yan terlindung
   ## Leding, Sumur bor/pompa, Sumur terlindung
   ## Mata air terlindung, Air Hujan
   ar_mandi = c(3,4,5,7,10)
}
rm("m2017")


if (vg_tahundata == "2015") {
   #  rincian air minum terlindungi
   ## Leding meteran, Leding eceran, Air Hujan
   ar_minum = c(3,4,11)
   
   
} else if (vg_tahundata == "2016") {
   
}


## define rincian untuk filter air minum layak
switch(
   vg_tahundata,
   
   "2015" = {
      ## rincian2 imunisasi DPT
      ##ar_DPT = c(r902f, r902g, r902h)
   },
   
   "2016" = {
      #  rincian air minum terlindungi
      ## Leding meteran, Leding eceran, Air Hujan
      ar_minum = c(3,4,11)
      
      #  rincian air terlindung dengan jarak >= 10m
      ## Sumur bor/pompa, Sumur terlindung, Mata air terlindung
      ar_minum_jarak = c(5,6,8)
      
      #  rincian air minum tidak sustain
      ## Air kemasan bermerk, Air isi ulang
      ar_tdk_sustain = c(1,2)
      
      #  rincian air minum tidak tidak terlindung
      ## Sumur tak terlindung, Mata air tak terlindung, Air Sungai, lainnya
      ar_tdk_terlindung = c(7,9,10,12)
      
      #  rincian air mandi/cuci yan terlindung
      ## Leding meteran, Leding eceran, Sumur bor/pompa, Sumur terlindung
      ## Mata air terlindung, Air Hujan
      ar_mandi = c(3,4,5,6,8,11)
   },
   
   "2017" = {
      #  rincian air minum terlindungi
      ## Leding, Air Hujan
      ar_minum = c(3,10)
      
      #  rincian air terlindung dengan jarak >= 10m
      ## Sumur bor/pompa, Sumur terlindung, Mata air terlindung
      ar_minum_jarak = c(4,5,7)
      
      #  rincian air minum tidak sustain
      ## Air kemasan bermerk, Air isi ulang
      ar_tdk_sustain = c(1,2)
      
      #  rincian air minum tidak tidak terlindung
      ## Sumur tak terlindung, Mata air tak terlindung, Air Sungai, lainnya
      ar_tdk_terlindung = c(6,8,9,11)
      
      #  rincian air mandi/cuci yan terlindung
      ## Leding, Sumur bor/pompa, Sumur terlindung
      ## Mata air terlindung, Air Hujan
      ar_mandi = c(3,4,5,7,10)
   }
)

## define variable baru untuk air minum layak
dt_ssnRT[,
         air_layak:= as.integer(
            ## penghitungan dengan rumus 1
            (
               ## air minum terlindungi
               get(air_minum) %in% ar_minum |
                  
                  ## air minum terlindungi dengan jarak >= 10m
                  (get(air_minum) %in% ar_minum_jarak & get(jarak_air_minum) == 2)
            ) |
               
               ## penghitungan rumus 2
               (
                  ## air minum terlindungi dengan jarak < 10m atau tidah tahu
                  (
                     get(air_minum) %in% ar_minum_jarak & 
                        get(jarak_air_minum) != 2 &
                        mapply(cek_mandi, get(air_minum), get(air_mandi))
                     ## referensi penggunaan mapply di
                     ## https://stackoverflow.com/questions/25431307/r-data-table-apply-function-to-rows-using-columns-as-arguments
                  ) |
                     
                     ## air minum tidak sustain dan tidak terlindung
                     (
                        (get(air_minum) %in% ar_tdk_sustain | get(air_minum) %in% ar_tdk_terlindung) &
                           get(air_mandi) %in% ar_mandi
                     )
               )
         ) * 100
         ]

## Persentase Nasional
hasil = dt_ssnIND[
   ## filter kuintil nasional
   QNas <= 2,
   
   ## Persentase Nasional
   .(PNas =  weighted.mean(PUS_CPRCS, get(bobot), na.rm = TRUE))
]

## simpan hasil di excel
write2xl(hasil$PNas, "nasional")

## Persentase Kota Desa
hasil = dt_ssnIND[
   ## filter kuintil Kota Desa
   QKD <= 2,
   
   ## Persentase Kota Desa
   .(PKD = weighted.mean(PUS_CPRCS, get(bobot), na.rm = TRUE)),
   
   ## group by kota desa
   by = kota_desa
]

## simpan hasil di excel
write2xl(unlist(c(hasil[1,2], hasil[2,2])), "kotadesa")

## Penghitungan Perentase Provinsi
hasil = dt_ssnIND[
   ## filter kuintil Provinsi, 1 dan 2
   QProv <= 2,
   
   ## Persentase Provinsi
   .(PProv = weighted.mean(PUS_CPRCS, get(bobot), na.rm = TRUE)),
   
   ## agregasi Provinsi
   by = prov
]

## simpan hasil di excel
write2xl(hasil$PProv, "provinsi")
