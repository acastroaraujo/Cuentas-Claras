Análisis rápido de financiación de candidatos a la alcaldía de Bogotá. El propósito de este repositorio es mostrar cómo se puede hacer un _análisis rápido_ en R.

Toda la información viende del portal Cuentas Claras.

https://www5.registraduria.gov.co/CuentasClarasPublicoTer2019/Consultas/Candidatos/

__Nota:__ Sólo estoy usando los formularios 5.1 y 5.2. __No soy experto en estos temas!__ Se trata de un análisis rápido, nada más.

__Nota 2:__ Por alguna razón, los documentos en formato pdf tienen información sobre cédula y NIT, mientras que los documentos en excel no. Entonces usamos el paquete [tabulizer](https://CRAN.R-project.org/package=tabulizer)