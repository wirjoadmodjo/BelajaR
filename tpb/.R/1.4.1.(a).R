indikator = "1.4.1.(a)"

## define rincian faskes persalinan berdasarkan tahun data
faskes2015 = c("2015", "2016")
if (any(faskes2015 == vg_tahundata)) {
   faskes = c(1,2,3)
}

faskes2017 = c("2017")
if (any(faskes2017 == vg_tahundata)) {
   faskes = c(1,2,3,4,5,6) 
}

## hapus var
rm("faskes2015", "faskes2017")

## pembentukan variable JPSalifaskes
dt_ssnIND[
   ## filter jenis kelamin
   get(jenis_kel) == 2 &
      
   ## filter status pernah kawin
   get(stat_kawin) >= 2 &
      
   ## filter umur 15-19
   get(umur) >= 15 & get(umur) <= 49 &
      
   ## filter pernah melahirkan lahir hidup
   get(lahir_hidup_2th) == 1,
   
   ## variable baru 
   ## penduduk yang melakukan persalinan di Faskes
   JPSalifaskes:= as.integer(get(tempat_salin) %in% faskes)
]

## Persentase Nasional
hasil = dt_ssnIND[
   ## filter kuintil nasional
   QNas <= 2,
   
   ## Persentase Nasional
   .(PNas =  weighted.mean(JPSalifaskes, get(bobot), na.rm = TRUE) * 100)
]

## simpan hasil di excel
write2xl(hasil$PNas, "nasional")

## Persentase Kota Desa
hasil = dt_ssnIND[
   ## filter kuintil Kota Desa
   QKD <= 2,
   
   ## Persentase Kota Desa
   .(PKD = weighted.mean(JPSalifaskes, get(bobot), na.rm = TRUE) * 100),
   
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
   .(PProv = weighted.mean(JPSalifaskes, get(bobot), na.rm = TRUE) * 100),
   
   ## agregasi Provinsi
   by = prov
]

## simpan hasil di excel
write2xl(hasil$PProv, "provinsi")
