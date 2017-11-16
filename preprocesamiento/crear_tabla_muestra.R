library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(dplyr)
library(purrr)
library(sp)
library(rslurm)

setwd("/LUSTRE/sacmod/validacion_madmex/tabla_muestra/")

# leer un shape por estado con el nombre del estado
read_edo <- function(path_edo){
    paths_files <- list.files(path_edo, recursive = FALSE)
    shp_name <- unique(tools::file_path_sans_ext(paths_files))
    readOGR(path_edo, shp_name)@data
}

# shapes ccon rectángulo
paths_edos <- list.dirs("/LUSTRE/sacmod/validacion_madmex/estados_vecinos", recursive = FALSE)
params_df <- dplyr::data_frame(path_edo = paths_edos)

sjob <- slurm_apply(read_edo, params = params_df, jobname = "tabla_muestra_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "5", ntasks = "5"))
print_job_status(sjob)

res_raw <- get_slurm_out(sjob, outtype = "raw", wait = FALSE)

# muestra con rectángulos agregados
muestra_ag <- map_df(res_raw, ~.x)

# save(muestra_ag, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-09-06_muestra_vecinos.RData")

resumen <- muestra_ag %>%
    group_by(edo) %>%
    summarise(
        n = n(),
        mains = sum(main)
    )


# verificamos que la muestra este en la nueva tabla
load("/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-25_muestra.Rdata") 
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")

pais_df_tidy <- pais_df %>%
    dplyr::mutate(id = 1:n())

muestra_edo_oid <- muestra %>% 
    left_join(pais_df_tidy, by = c("id", "edo"))

muestra_join <- inner_join(muestra_edo_oid, muestra_ag, by = c("oid", "edo", "tile"))

# verificar que oid es único por estado
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")
pais_df_tidy <- pais_df %>%
    dplyr::mutate(id = 1:n())

edo_oid <- muestra %>% 
    left_join(pais_df_tidy, by = "id") %>% 
    group_by(edo.x, oid, predicted) %>% 
    count()

muestra %>% 
    left_join(pais_df_tidy, by = "id") 
