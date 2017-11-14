# anp_reporte

Se crean reportes en formato html y formato pdf con información de las ANPs de CONANP, estos incluyen datos de uso de suelo, pérdida de cobertura boscosa e integridad ecosistémica.

Fuentes de información

* [Ecorregiones terrestres](http://www.conabio.gob.mx/informacion/metadata/gis/ecort08gw.xml?_xsl=/db/metadata/xsl/fgdc_html.xsl&_indent=no)
* [Integridad Ecológica]()
* [Deforestación, Matt Hansen](http://earthenginepartners.appspot.com/science-2013-global-forest)
* [Clases de cobertura Madmex](http://madmex.conabio.gob.mx/)

## prprocesamiento

### crear_datos_cobertura_slurm.R 
Crea tablas para ANPs y sus anillos con variables: year_loss (0: no pérdida, 10: pérdida 2011, 11: pérdida 2012,...), 
clase_madmex (clase de cobertura), n (número de pixeles 30x30 m), anp (nombre de ANP coinicide con nombre de archivo).

#### Entradas
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer/.. shapes de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_rings/.. shapes de anillos de 20km alrededor de ANPs
* datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif
* datos_insumo/madmex_nalc_10c_30m_2010.tif

#### Salidas
* datos_procesados/aaaa-mm-dd_perdida_cobertura.RData
* datos_procesados/aaaa-mm-dd_perdida_cobertura_rings.RData

### crear_datos_ecorregion_region_anp.R
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

### crear_tablas_ie_adi_anp.R
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

### crear_mapas_png_mdmx_defo
Crea mapas en png de pérdida de cobertura boscosa para cada ANP con la pérdida entre 2011 y 2016, crea mapas png de cobertura de suelo usando la clasificación MAD-MEX 2010.

#### Entradas
* datos_insumo/madmex_nalc_10c_30m_2010.tif
* datos_insumo/shapes_anp/anp_sinBuffer
* datos_insumo/anp_nombres.tsv
* datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif

#### Salidas
* datos_procesados/mapas/deforestacion/NOMBRE_ANP_defo.png
* datos_procesados/mapas/deforestacion/NOMBRE_ANP_cobertura.png

## reportes

### prototipo.Rmd
Código para crear reportes html

#### Salidas
* aaaa-mm-dd_reportes_html/NOMBRE_ANP.html

### prototipo_pdf.Rmd
Código para crear reportes pdf. Éstos reportes tienen un subconjunto de los análisis incluídos en los reportes en formato html.

#### Salidas
* aaaa-mm-dd_reportes_pdf/NOMBRE_ANP.pdf

## Pendiente
* en el caso de que una ANP pertenezca a más de una ecorregión (con porcentajes no despreciables) hacer los análisis de ecorregión considerando las ANPs a las que pertenece, y también las tablas.

* Las ANPs que pertenecen a más de una región CONANP deben separarse.

* Agregar gráficas de ADI, cambiar escala de mapa IE, agregar tabla navegable.

* Revisar NAs en mapa Hansen con !NA en mapa MADMEX.

