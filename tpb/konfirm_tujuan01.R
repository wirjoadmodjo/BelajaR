## TUJUAN 01

# load library
library(foreign)
#library(haven)
#library(readxl)
library(data.table)
library(Hmisc)
#library(openxlsx)

## read data
dt_IND = setDT(read.dbf("E:/PabrikAngka/rawdata/Susenas201603/kor16ind_revisi_14122016.dbf"))

## hitung batas pendapatan 40% NASIONAL
EXPCAP_40_NAS = wtd.quantile(dt_IND$EXP_CAP, 
                     weights = dt_IND$FWT_TAHUN,
                     probs = 0.4, na.rm = TRUE)

## hitung batas pendapatan 40% PERKOTAAN
EXPCAP_40_KOTA = wtd.quantile(dt_IND[R105==1,.(EXP_CAP, FWT_TAHUN)]$EXP_CAP, 
                     weights = dt_IND[R105==1,.(EXP_CAP, FWT_TAHUN)]$FWT_TAHUN,
                     probs = 0.4, na.rm = TRUE)

## hitung batas pendapatan 40% PERDESAAN
EXPCAP_40_DESA = wtd.quantile(dt_IND[R105==2,.(EXP_CAP, FWT_TAHUN)]$EXP_CAP, 
                     weights = dt_IND[R105==2,.(EXP_CAP, FWT_TAHUN)]$FWT_TAHUN,
                     probs = 0.4, na.rm = TRUE)


### INDIKATOR 1.4.1.(c)

## angka nasional
dt_temp = dt_IND[
   ## filter status perkawinan = kawin
   R404 == 2 &
   
   ## filter jenis kelamin perempuan
   R405 == 2 &
      
   ## filter umur 15-49 tahun
   R407 >= 15 & R407 <= 49,
      
   
   .(
      ## jumlah pasangan usia subur
      JPUS15_49  = ifelse(EXP_CAP <= EXPCAP_40_NAS, FWT_TAHUN, 0),
      
      ## jumlah yang sedang memakai CPR
      JPUS_CPRSC = ifelse(R1401 == 2 & EXP_CAP <= EXPCAP_40_NAS, FWT_TAHUN, 0),
      
      ## jumlah PUS KOTA
      JPUS15_49_KOTA = ifelse(R105 == 1 & EXP_CAP <= EXPCAP_40_KOTA, FWT_TAHUN, 0),
      
      ## jumlah yang sedang memakai CPR KOTA
      JPUS_CPRSC_KOTA = 
         ifelse(R105 == 1 & R1401 == 2 & EXP_CAP <= EXPCAP_40_KOTA, FWT_TAHUN, 0),
      
      ## jumlah PUS DESA
      JPUS15_49_DESA = ifelse(R105 == 2 & EXP_CAP <= EXPCAP_40_DESA, FWT_TAHUN, 0),
      
      ## jumlah yang sedang memakai CPR DESA
      JPUS_CPRSC_DESA = 
         ifelse(R105 == 2 & R1401 == 2 & EXP_CAP <= EXPCAP_40_DESA, FWT_TAHUN, 0)
   )
]

dt_temp[,
   .(
      CPR_SC = round((sum(JPUS_CPRSC) * 100 / sum(JPUS15_49)) ,2),
      CPR_SC_KOTA = round((sum(JPUS_CPRSC_KOTA) * 100 / sum(JPUS15_49_KOTA)) ,2),
      CPR_SC_DESA = round((sum(JPUS_CPRSC_DESA) * 100 / sum(JPUS15_49_DESA)) ,2)
   )
]

R101 = unique(dt_IND$R101)
EXPCAP_PROV = c()
for (i in R101) {
   temp_expcap = 
      wtd.quantile(dt_IND[R101 == i,.(EXP_CAP, FWT_TAHUN)]$EXP_CAP, 
                   weights = dt_IND[R105 == i,.(EXP_CAP, FWT_TAHUN)]$FWT_TAHUN,
                   probs = 0.4, na.rm = TRUE)
   
   EXPCAP_PROV = c(EXPCAP_PROV, temp_expcap)
   
}

dt_EXPCAP_PROV = data.table(R101, EXPCAP_PROV)

dt_temp = dt_IND[
   ## filter status perkawinan = kawin
   R404 == 2 &
      
   ## filter jenis kelamin perempuan
   R405 == 2 &
      
   ## filter umur 15-49 tahun
   R407 >= 15 & R407 <= 49,
   
   
   .(
      ## prov
      R101 = R101,
      
      ## R1401
      R1401 = R1401,
      
      ## FWT
      FWT_TAHUN = FWT_TAHUN,
      
      ## EXP
      EXP_CAP = EXP_CAP
   )
]

setkey(dt_temp, R101)
setkey(dt_EXPCAP_PROV, R101)

dt_temp = merge(dt_temp, dt_EXPCAP_PROV, all.x = TRUE)

dt_temp = dt_temp[
   EXP_CAP <= EXPCAP_PROV,
   .(
      R101 = R101,
      
      ## jumlah pasangan usia subur
      JPUS15_49  = FWT_TAHUN,
      
      ## jumlah yang sedang memakai CPR
      JPUS_CPRSC = ifelse(R1401 == 2, FWT_TAHUN, 0)
   )
]

dt_temp[,
   .(
     CPR_SC = round((sum(JPUS_CPRSC) * 100 / sum(JPUS15_49)) ,2)
   ), by = c("R101")
]





### INDIKATOR 1.4.1.(a)
dt_temp = dtIND[
   # filter jenis kelamin
   R405 == 2 &
      
   # filter status perkawinan pernah kawin
   R404 >= 2 &
      
   # filter umur 15 - 49
   R407 >= 15 & R407 <= 49 &
      
   # filter pernah melahirkan lahir hidup 2th terakhir 
   R1301 == 1 &
      
   # filter pendapatan 40%
   EXP_CAP <= exp_cap_40,
   
   .(
      prov = R101,
      kota_desa = R105,
      JPSalifaskes = ifelse(R1302B <= 3, FWT_TAHUN, 0),
      JP15_49 = FWT_TAHUN
   )
]

## angka nasional & provinsi
cube(dt_temp, .(PSalifaskes = round(sum(JPSalifaskes) * 100 /sum(JP15_49), 2)), 
     by=c("prov"))
# yang NA merupakan angka nasional


## agregasi kota desa
cube(dt_temp, .(PSalifaskes = round(sum(JPSalifaskes) * 100 /sum(JP15_49), 2)), 
     by=c("kota_desa"))
###############################################################################


## INDIKATOR 1.4.1.(b)
dt_temp = dtIND[
   # filter balita umur 12-23 bulan
   R902 >= 12 & R902 <= 23 &
      
   # filter pendapatan 40%
   EXP_CAP <= exp_cap_40,
   
   .(
      prov = R101,
      kota_desa = R105,
      jenis_kel = R405,
      JAIDL = ifelse(
         ## imunisai DPT 3x
         R907F > 0 & R907G > 0 & R907H > 0 &
            
            ## versi polio 3x
            (
               ## polio 1, 2, 3
               (R907B > 0 & R907C > 0 & R907D > 0) |
               ## polio 1, 3, 4
               (R907B > 0 & R907D > 0 & R907E > 0) |
               ## polio 2, 3, 4
               (R907C > 0 & R907D > 0 & R907E > 0)
            ) &
            
            ## imunisasi Polio 4x
            #R907B > 0 & R907C > 0 & R907D > 0 & R907E > 0 &
            
            ## imunisasi campak & bcg
            R907M > 0  & R907A > 0 &
            
            ## versi hepatitis 3x
            (
               ## 1, 2, 3
               (R907I > 0 & R907J > 0 & R907K > 0) |
               ## 1, 3, 4
               (R907I > 0 & R907K > 0 & R907L > 0) |
               ## 2, 3, 4
               (R907J > 0 & R907K > 0 & R907L > 0)
            ), 
         
         ## imunisasi hepatitis B 4x
         #R907I > 0 & R907J > 0 & R907K > 0 & R907L > 0,
         
         FWT_TAHUN, 0
      ),
      JA12_23bln = FWT_TAHUN
   )
]

# replace NA value
dt_temp[is.na(JAIDL), JAIDL := 0]

## persentase by prov dan angka nasional
cube(dt_temp, 
     .(PIDL = round(sum(JAIDL) * 100 /sum(JA12_23bln), 2)),
     by=c("prov"))

## persentase by kota - desa
dt_temp[,
        .(PIDL = round(sum(JAIDL) * 100 /sum(JA12_23bln), 2)),
        by = list(kota_desa)
]

## persentase by jenis kelamin
dt_temp[,
        .(PIDL = round(sum(JAIDL) * 100 /sum(JA12_23bln), 2)),
        by = list(jenis_kel)
]


### INDIKATOR 1.4.1.(c)
dt_temp = dtIND[
   # filter umur 15-49
   R407 >= 15 & R407 <= 49 &
   
   # filter status perkawinan
   R404 >= 2 &
      
   # filter pendapatan 40%
   EXP_CAP <= exp_cap_40 &
      
   # filter melahirkan anak lahir hidup 2th terakhir
   R1301 == 1,
   
   .(
      prov = R101,
      JPUS_CPRSC = ifelse(R1401 == 2, FWT_TAHUN, 0),
      JPUS15_49 = FWT_TAHUN
   )
]

## persentase by prov dan angka nasional
cube(dt_temp, 
     .(CPR_SC = round(sum(JPUS_CPRSC) * 100 /sum(JPUS15_49), 2)),
     by=c("prov"))
