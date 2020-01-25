# anp_reporte

Se crean reportes en formato html y formato pdf con información de las ANPs de 
CONANP, estos incluyen datos de uso de suelo, pérdida de cobertura boscosa, 
integridad ecosistémica, calidad de hábitat. Los reportes se pueden consultar en https://monitoreo.conabio.gob.mx en la sección de indicadores, como ejemplo el 
correspondiente a Calakmul esta [aquí](https://monitoreo.conabio.gob.mx/i-efectividad/reportes_html/9103.html).

### Fuentes de información

* [Ecorregiones terrestres](http://www.conabio.gob.mx/informacion/metadata/gis/ecort08gw.xml?_xsl=/db/metadata/xsl/fgdc_html.xsl&_indent=no)
* [Integridad Ecológica]()
* [Deforestación, Matt Hansen](http://earthenginepartners.appspot.com/science-2013-global-forest)
* [Clases de cobertura Madmex](http://madmex.conabio.gob.mx/)

## Código
El esquema de carpetas es como sigue:

```
anp_reporte
    ├── preprocesamiento
    │   ├── crear_tablas_cobertura.R
    │   ├── crear_tablas_descarga.R 
    │   ├── crear_tablas_perdida_tth.R
    │   ├── crear_tablas_ecorregion_region.R
    │   ├── crear_tablas_ie_adi.R
    │   ├── crear_mapas_png.R    
    │   ├── crear_tablas_calidad_habitat.R
    │   └── crear_tablas_descarga
    ├── reportes
    │   ├── render_reportes.R
    │   ├── funciones_reporte.R
    │   ├── prototipo_pdf.Rmd
    │   └── prototipo.Rmd
    ├── datos_insumo
    └── datos_procesados

```

## Workflow

1. Generar tablas de datos a usar en reportes:

`crear_tablas_ecorregion_region.R`  -> `crear_tabla_categorias_extents.R` -> `crear_mapas_png.R`  
`crear_tablas_calidad_habitat.R`  
`crear_tablas_ie_adi.R`  
`crear_tablas_cobertura.R` -> `crear_tablas_descarga.R` -> `crear_tablas_perdida_tth.R`


2. Correr reportes

`render_reportes.R`



## preprocesamiento
En la sección de preprocesamiento se encuentran los scripts que generan tablas 
de datos que después se utilizan en los reportes.


### `crear_datos_ecorregion_region_anp.R`
Crea tablas de ecorregión para ANPs con variables: anp, eco (ecorregion), hectáreas.
Crea tablas de región CONANP (regiones administrativas) con variables: region, anp. 

#### Entradas
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer/.. shapes de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_rings/.. shapes de anillos de 20km alrededor de ANPs
* datos_insumo/ecorregiones/ecort08gw mapa de ecorregiones 2008 descargado del link de arriba

#### Salidas
* datos_procesados/aaaa-mm-dd_ecorregion.RData
* datos_procesados/aaaa-mm-dd_ecorregion_rings.RData
* datos_procesados/aaaa-mm-dd_anp_region.RData

### `crear_tabla_categorias_extents.R`
Crea tabla indicando a que categoría de manejo pertenece cada ANP y su extent.

### `crear_tablas_ie_adi_anp.R`
Crea una lista con una entrada para cada ANP, la lista incluye la media, mediana y desviación estándar 
de integridad ecológica, el último campo de la lista es una muestra de 1000 valores de integridad.

Reproyecta los rasters de ADI a longlat.

#### Entradas
* datos_insumo/ie_2014_250m.tif
* datos_insumo/adi/ADI_2014-2015_1000m.tif
* datos_insumo/adi/ADI_dia_2014-2015_1000m.tif
* datos_insumo/adi/ADI_noche_2014-2015_1000m.tif

#### Salidas
* datos_procesados/aaaa-mm-dd_ie_list.RData
* datos_procesados/aaaa-mm-dd_adi_longlat.tif
* datos_procesados/aaaa-mm-dd_adi_dia_longlat.tif
* datos_procesados/aaaa-mm-dd_adi_noche_longlat.tif


### `crear_tablas_cobertura.R`
Crea tablas para ANPs y sus anillos con variables: year_loss (0: no pérdida, 10: pérdida 2011, 11: pérdida 2012,...), 
clase_madmex (clase de cobertura), n (número de pixeles 30x30 m), anp (nombre de ANP coinicide con nombre de archivo).

#### Entradas
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer/.. shapes de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_rings/.. shapes de anillos de 20km alrededor de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_zonasnucleo/.. shapes de zonas núcleo (definidos solo en algunas ANPs)
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_zonaspreservacion/.. shapes de zonas preservación (definidos solo en algunas ANPs)
* datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif raster con píxeles de pérdida de cobertura boscosa
* datos_insumo/madmex_nalc_10c_30m_2010.tif raster con 8 clases MAD-MEX postprocesado para coincidir con proyección de raster Hansen (Julián E.)

#### Salidas
1. `datos_procesados/madmex_hansen/aaaa_mm_dd_anp_tipo_loss.RData` donde tipo es sinBuffer, rings, 
zonasnucleo zonaspreservacion: pérdida en pixeles por año.  
2. `datos_procesados/madmex_hansen/aaaa_mm_dd_anp_tipo_madmex.RData` donde tipo es sinBuffer, rings, 
zonasnucleo, zonaspreservacion: número de pixeles en cada clase Madmex.  
3. `datos_procesados/madmex_hansen/aaaa_mm_dd_anp_tipo_madmex_loss.RData` donde tipo es sinBuffer, rings,
zonasnucleo, zonaspreservacion: pérdida en pixeles por año para cada clase Madmex.

### `crear_tablas_descarga.R`
Crea tablas para descarga directa en zip (archivos en csv).

#### Entradas
* datos_procesados/nombres_anps_id.csv
* datos_procesados/area_ecorregion/2018-08-08_ecorregion.RData
* datos_procesados/area_ecorregion/2018-08-08_area_rings.RData, datos_procesados/area_ecorregion/2018-08-08_area_sinBuffer.RData, 
datos_procesados/area_ecorregion/2018-08-08_area_zonasnucleo.RData, 
datos_procesados/area_ecorregion/2018-08-08_area_zonaspreservacion.RData
* datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif
* datos_procesados/integridad/2018-07-27_ie_stats.csv
* datos_procesados/2018-08-24_suitabilility_report.RData
* datos_procesados/2019-02-14_suitabilility_report_ponca.RData
* datos_insumo/anp_ids.csv

#### Salidas
* datos_procesados/descarga/ecoregion_area.cs
* datos_procesados/descarga/area_anp_periferia_zn_zp.csv
* datos_procesados/descarga/area_anp_periferia_zn_zp.csv
* datos_procesados/descarga/perdida_cobertura_2001_2018.csv
* datos_procesados/descarga/clase_cobertura_2015.csv
* datos_procesados/descarga/perdida_cobertura_clase_2001_2018.csv
* datos_procesados/descarga/integridad_estadisticas.csv
* datos_procesados/madmex_hansen/perdida_cobertura_2001_2018.csv
* datos_procesados/madmex_hansen/perdida_cobertura_clase_2001_2018.csv
* datos_procesados/descarga/calidad_habitat_puma.csv
* datos_procesados/descarga/calidad_habitat_ponca.csv

## reportes

### `render_reportes.R`
genera los reportes leyendo archivos con la información de las ANPs y llamando
los Rmds para crear los reportes, ya sea pdf (`prototipo_pdf.Rmd`) o html 
(`prototipo.Rmd`).

### `funciones_reporte.R`
Funciones comunes a los reportes pdf y html, para generar tablas y gráficas, 
separar el reporte en funciones facilita el mantenimiento.

### `prototipo.Rmd` y `prototipo_pdf.Rmd`
Código para crear reportes html o pdf.

#### Salidas
* aaaa-mm-dd_reportes_html/NOMBRE_ANP.html


#### Salidas
* aaaa-mm-dd_reportes_pdf/NOMBRE_ANP.pdf


## datos_insumo
* datos_insumo/anp_nombres.tsv : generado manualmente contiene nombres cortos para gráficas
* datos_insumo/spatial_layers_styles.json : estilos de geoportal para generar etiquetas de mapas
* datos_insumo/anp_ids.csv: obtenido de la base de datos postgres *reportes\_anp*

## Pendiente

* Eliminar mala práctica de guardar RDatas.

* en el caso de que una ANP pertenezca a más de una ecorregión (con porcentajes no despreciables) hacer los análisis de ecorregión considerando las ANPs a las que pertenece, y también las tablas.

* Las ANPs que pertenecen a más de una región CONANP deben separarse.

* Agregar gráficas de ADI, cambiar escala de mapa IE, agregar tabla navegable.


