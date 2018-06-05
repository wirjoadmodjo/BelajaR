# load library
#library(data.table)
#library(grattan)

setkeyv(dt_ssnRT, c(prov, kota_desa, expcap))

## Kuantil Nasional
dt_ssnRT[,
   QNas:= weighted_ntile(get(expcap_rt), get(bobot_rt) * get(n_art), n=5)
]

## Kuantil Kota - Desa
dt_ssnRT[,
   QKD:= weighted_ntile(get(expcap_rt), get(bobot_rt) * get(n_art), n=5),
   by = get(kota_desa)
]

## Kuantil Provinsi
dt_ssnRT[,
   QProv:= weighted_ntile(get(expcap_rt), get(bobot_rt) * get(n_art), n=5),
   by = get(prov)
]