indikator = "1.4.1.(c)"

dt_ssnIND[
   ## filter umur 15-49thn
   get(umur) >= 15 & get(umur) <= 49 &
      
      ## jenis kelamin perempuan
      get(jenis_kel) == 2 &
      
      ## status perkawinan = kawin
      get(stat_kawin) == 2,
   
   ## variale baru
   ## PUS yang menggunakan KB
   PUS_CPRCS:= as.integer(get(kb_pakai) == 2) * 100
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
