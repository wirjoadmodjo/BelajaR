indikator = "1.4.1.(b)"

# define minimal jumlah imunisasi
nDPT = 3
nPolio = 3
nCampak = 1
nBCG = 1
nHB = 3

ar_imun = c(imun_bcg, imun_polio1, imun_polio2, imun_polio3, imun_polio4,
            imun_dpt1, imun_dpt2, imun_dpt3, imun_hb_lahir, imun_hb1,
            imun_hb2, imun_hb3, imun_campak)

ar_bcg = c(imun_bcg)
ar_polio = c(imun_polio1, imun_polio2, imun_polio3, imun_polio4)
ar_dpt = c(imun_dpt1, imun_dpt2, imun_dpt3)
ar_hb = c(imun_hb_lahir, imun_hb1, imun_hb2, imun_hb3)
ar_campak = c(imun_campak)

## define rincian2 imunisasi berdasarkan tahun data
imun2015 = c("2015", "2016")
if (any(imun2015 == vg_tahundata)) {
   ar_imun = c(imun_bcg, imun_polio1, imun_polio2, imun_polio3, imun_polio4,
               imun_dpt1, imun_dpt2, imun_dpt3, imun_hb_lahir, imun_hb1,
               imun_hb2, imun_hb3, imun_campak)
   
   ar_bcg = c(imun_bcg)
   ar_polio = c(imun_polio1, imun_polio2, imun_polio3, imun_polio4)
   ar_dpt = c(imun_dpt1, imun_dpt2, imun_dpt3)
   ar_hb = c(imun_hb_lahir, imun_hb1, imun_hb2, imun_hb3)
   ar_campak = c(imun_campak)
}

imun2017 = c("2017")
if (any(imun2017 == vg_tahundata)) {
   
   ## di tahun 2017 pada pencatatan imunisasi terdapat rincian tgl, bln dan thn
   ## bila salah satu rincian (tgl, bln, thn) tsb terdapat isian > 0 maka
   ## dinyatakan balita mendapat imunisasi
   
   ar_kolom = colnames(dt_ssnIND)
   ## iterasi pengecakan imunisasi
   for (imun in ar_imun) {
      ar = grep(a, ar_kolom, value=TRUE)
      
      dt_ssnIND[,
                (imun) := ifelse(rowSums(dt_ssnIND[, ar, with = FALSE], na.rm = FALSE) > 0, 1, 0)
                ]
   }
   
   # clean data
   ## replace data tanggal imunisasi ke 1 bila tanggal > 0
   for (j in names(dt_ssnIND[, ar_imun, with = FALSE])) {
      set(dt_ssnIND,which(dt_ssnIND[[j]] > 0),j,1)
   }
}

dt_ssnIND[,
          .(r1104a, r1104a_bln, r1104a_tgl, r1104a_thn)
          ]

ar_kolom = colnames(dt_ssnIND)
ar_kolom[ar_kolom %in% "r1104"]
which(ar_kolom %in% "urut")
match("r1104", ar_kolom)
grep(imun_bcg, ar_kolom, value=TRUE)
## remove temp var
rm("imun2015", "imun2017")

## cleaning data, replace NA value to 0
for (j in names(dt_ssnIND[, ar_imun, with = FALSE])) {
   set(dt_ssnIND,which(is.na(dt_ssnIND[[j]])),j,0)
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