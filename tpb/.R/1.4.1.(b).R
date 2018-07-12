indikator = "1.4.1.(b)"

# define minimal jumlah imunisasi
nDPT = 3
nPolio = 3
nCampak = 1
nBCG = 1
nHB = 3

## pengitungan imunisasi bersumber pada 2 hal
## 1. data yang di kartu KMS
## 2. data ingatan responden

## define rincian2 yang merupakan imunisasi
ar_imun_kartu = c(imun_bcg, imun_polio1, imun_polio2, imun_polio3, imun_polio4,
                  imun_dpt1, imun_dpt2, imun_dpt3, imun_hb_lahir, imun_hb1,
                  imun_hb2, imun_hb3, imun_campak)

## penglompokan rincian berdasarkan jenis imunisasi
ar_bcg = c(imun_bcg)
ar_polio = c(imun_polio1, imun_polio2, imun_polio3, imun_polio4)
ar_dpt = c(imun_dpt1, imun_dpt2, imun_dpt3)
ar_hb = c(imun_hb_lahir, imun_hb1, imun_hb2, imun_hb3)
ar_campak = c(imun_campak)

## cleaning data tgl, bln thn imunisasi di kartu KMS
# penanganan untuk data tahun 2017
imun2017 = c("2017")
if (any(imun2017 == vg_tahundata)) {
   
   ## di tahun 2017 pada pencatatan imunisasi terdapat rincian tgl, bln dan thn
   ## bila salah satu rincian (tgl, bln, thn) tsb terdapat isian > 0 maka
   ## dinyatakan balita mendapat imunisasi
   
   ar_kolom = colnames(dt_ssnIND)
   ## iterasi pengecakan imunisasi
   for (imun in ar_imun_kartu) {
      ar = grep(imun, ar_kolom, value=TRUE)
      ## cleaning data, replace NA value to 0
      for (f in names(dt_ssnIND[, ar, with = FALSE])) {
         set(dt_ssnIND,which(is.na(dt_ssnIND[[f]])),f,0)
      }
      
      dt_ssnIND[,
         (imun) := ifelse(rowSums(dt_ssnIND[, ar, with = FALSE], na.rm = FALSE) > 0, 1, 0)
      ]
   }
}  
rm("imun2017")


## cleaning data, replace NA value to 0
for (f in names(dt_ssnIND[, ar_imun_kartu, with = FALSE])) {
   set(dt_ssnIND,which(is.na(dt_ssnIND[[f]])),f,0)
}


## penghitungan jumlah imunisasi berdasarkan
## catatan kartu KMS
dt_ssnIND[,
          
   `:=` (
      ## total imunisasi BCG
      kbcg = rowSums(dt_ssnIND[, ar_bcg, with=FALSE], na.rm = TRUE),
             
      ## total imunisasi Polio
      kpolio = rowSums(dt_ssnIND[, ar_polio, with=FALSE], na.rm = TRUE),
             
      ## total imunisasi DPT
      kdpt = rowSums(dt_ssnIND[, ar_dpt, with=FALSE], na.rm = TRUE),
             
      ## total imunisasi Hepatitis B
      khb = rowSums(dt_ssnIND[, ar_hb, with=FALSE], na.rm = TRUE),
             
      ## total imunisasi Campak
      kcampak = rowSums(dt_ssnIND[, ar_campak, with=FALSE], na.rm = TRUE)
   )
]


## penghitungan jumlah imunisasi berdasarkan
## ingatan responden
dt_ssnIND[,
   `:=` (
      ## imunisasi bcg berdasarkan ingatan responden
      mbcg = ifelse(get(bcg_cek) == 1, 1,0),
             
      ## imunisasi campak berdasarkan ingatan responden
      mcampak = ifelse(get(campak_cek) == 1, 1,0)
   )
]

dt_ssnIND[
   ## responden tidak dapat menunjukkan kartu KMS
   get(kartu_kms) == 2 | get(punya_kms) > 0,
   
   `:=` (
      ## imunisasi polio berdasarkan ingatan responden
      mpolio = get(polio_cek),
      
      ## imunisasi dpt berdasarkan ingatan responden
      mdpt = get(dpt_cek),
      
      ## imunisasi hepatitis B berdasarkan ingatan responden
      mhb = get(hb_cek)
   )
]

ar_na2 = c("kbcg", "kpolio", "kdpt", "khb", "kcampak", 
           "mbcg", "mpolio", "mdpt", "mhb", "mcampak")
## cleaning data, replace NA value to 0
for (j in names(dt_ssnIND[, ar_na2, with = FALSE])) {
   set(dt_ssnIND,which(is.na(dt_ssnIND[[j]])),j,0)
}

## definisi imunisasi dasar lengkap 
## dengan menjumlah hasil 2 metode diatas
dt_ssnIND[,
   ## variable imunisasi dasar lengkap
   IDL:= as.integer(
      ## cek jumlah imunisasi BCG
      (kbcg + mbcg) >= nBCG &
                
      ## cek jumlah imunisasi Polio
      (kpolio + mpolio) >= nPolio &
                
      ## cek jumlah imunisasi DPT
      (kdpt + mdpt) >= nDPT &
                
      ## cek jumlah imunisasi Hepatitis B
      (khb + mhb) >= nHB &
                
      ## cek jumlah imunisasi Campak
      (kcampak + mcampak) >= nCampak
   ) 
]


## persentase nasional
hasil = dt_ssnIND[
   ## filter kuintil nasional
   QNas <= 2 & 
      
   ## filter umur 12-23 bulan
   get(umur_balita) >= 12 & get(umur_balita) <= 23,
   
   ## Persentase Nasional
   .(PNas =  weighted.mean(IDL, get(bobot), na.rm = TRUE) * 100)
]

## simpan hasil di excel
write2xl(hasil$PNas, "nasional")


## persentase agregasi kota-desa
hasil = dt_ssnIND[
   ## filter kuintil 2 kota-desa
   QKD <= 2 & 
   
   ## filter umur 12-23 bulan
   get(umur_balita) >= 12 & get(umur_balita) <= 23,
   
   ## Persentase Kota-Desa
   .(PKD =  weighted.mean(IDL, get(bobot), na.rm = TRUE) * 100),
   by = kota_desa
]

## simpan hasil di excel
write2xl(unlist(c(hasil[1,2], hasil[2,2])), "kotadesa")


## persentase agregasi jenis kelamin
hasil = dt_ssnIND[
   ## filter kuintil nasional
   QLP <= 2 & 
      
   ## filter umur 12-23 bulan
   get(umur_balita) >= 12 & get(umur_balita) <= 23,
   
   ## Persentase Nasional
   .(PJK =  weighted.mean(IDL, get(bobot), na.rm = TRUE) * 100),
   by = jenis_kel
]

## simpan hasil di excel
write2xl(unlist(c(hasil[1,2], hasil[2,2])), "jk")


## persentase agregasi provinsi
hasil = dt_ssnIND[
   ## filter kuintil nasional
   QProv <= 2 & 
      
   ## filter umur 12-23 bulan 
   get(umur_balita) >= 12 & get(umur_balita) <= 23,
   
   ## Persentase Nasional
   .(PProv =  weighted.mean(IDL, get(bobot), na.rm = TRUE) * 100),
   by = prov
]

## simpan hasil di excel
write2xl(hasil$PProv, "provinsi")