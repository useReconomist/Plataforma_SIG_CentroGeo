# Plataforma_SIG_CentroGeo

Proyecto de SIG CentroGeo, consiste en la implementación de triggers para archivos CSV, al subir un CSV, txt, TSV, hace una coerción a objeto espacial
con proyección 4326 (WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS) hace la intersección con los AGEBS, e integra indicadores como:
 
 * Población
 * Población Masculinos}
 * Población Femenino
 * Marginación
 * Alcaldía
 * Ageb
 * % De población sin servicios de salud
 * % De Población sin educación básica (<15 años)
 
El proceso que integra las variables es a través de la ubicación y su posición geográfica, usa como input un archivo csv y 
devuelve otro csv con las variables integradas, 
la información que arroja son bases del Censo de Población 2020, CONAPO Marginación y AGEBS.

**Los Autores del proyecto son:**

* Zumaya Tapia Rodrigo - al.rzumaya@centrogeo.edu.mx
* Noé Osorio García - al.nosorio@centrogeo.edu.mx

**Asesores**

* Mtra. Paulina Paredes: pparedes@centrogeo.edu.mx
* Mtro. Jesús Trujillo Almeida: jtrujillo@centrogeo.edu.mx
