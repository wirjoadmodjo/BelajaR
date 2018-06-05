# load library
#library(data.table)
#library(grattan)

## set index variabel akan sering dipakai
setkeyv(dt_ssnIND, c(prov, kota_desa, jenis_kel, expcap))

## Kuantil Nasional
dt_ssnIND[,
   ## variable baru kuantil Nasional
   QNas:= weighted_ntile(get(expcap), get(bobot), n=5)
]
   
## Kuantil Kota - Desa
dt_ssnIND[,
   QKD:= weighted_ntile(get(expcap), get(bobot), n=5),
   by = get(kota_desa)
]

## Kuantil Jenis Kelamin
dt_ssnIND[,
   QLP:= weighted_ntile(get(expcap), get(bobot), n=5),
   by = get(jenis_kel)
]

## Kuantil Provinsi
dt_ssnIND[,
   QProv:= weighted_ntile(get(expcap), get(bobot), n=5),
   by = get(prov)
]