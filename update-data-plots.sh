#! /bin/bash
Rscript 01-proc-datos.R
Rscript 02-graficos-fallecimiento-domicilios.R
Rscript 03-graficos-fallecimiento-menores.R
# wait for 60 seconds so we do not hit github's rate limit
sleep 60
Rscript 04-grafico-fallecimiento-sinadef-confirmados.R
