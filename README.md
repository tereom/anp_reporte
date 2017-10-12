# anp_reporte


Fuentes de información

* [Ecorregiones terrestres](http://www.conabio.gob.mx/informacion/metadata/gis/ecort08gw.xml?_xsl=/db/metadata/xsl/fgdc_html.xsl&_indent=no)
* [Integridad Ecológica]()
* [Deforestación, Matt Hansen](http://earthenginepartners.appspot.com/science-2013-global-forest)
* [Clases de cobertura Madmex](http://madmex.conabio.gob.mx/)

## Procesamiento

### crear_datos_cobertura_slurm.R 
Crea tablas para ANPs y sus anillos con variables: year_loss (0: no pérdida, 10: pérdida 2011, 11: pérdida 2012,...), 
clase_madmex (clase de cobertura), n (número de pixeles 30x30 m), anp (nombre de ANP coinicide con nombre de archivo).

#### Entradas
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer/.. shapes de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_rings/.. shapes de anillos de 20km alrededor de ANPs
* datos_insumo/hansen_forest_loss_v1_3.tif
* datos_insumo/madmex_nalc_10c_30m_2010.tif

#### Salidas
* datos_procesados/2017-10-02_perdida_cobertura.RData
* datos_procesados/2017-10-02_perdida_cobertura_rings.RData

### crear_datos_ecorregion_anp.R
Crea tablas para ANPs con variables: anp, eco (ecorregion), hectáreas.

#### Entradas
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer/.. shapes de ANPs
* /LUSTRE/MADMEX/eodata/footprints/anp/anp_rings/.. shapes de anillos de 20km alrededor de ANPs
* datos_insumo/ecorregiones/ecort08gw mapa de ecorregiones 2008 descargado del link de arriba

#### Salidas
* datos_procesados/2017-10-07_ecorregion.RData
* datos_procesados/2017-10-07_ecorregion_rings.RData
