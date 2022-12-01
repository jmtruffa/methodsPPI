Métodos para conectarse a la API de Portfolio Personal
Asume que las Keys (claves) están en .Renviron bajo el nombre de:

- PPI_API_KEY
- PPI_SECRET_KEY

Seteando esas dos con las que se obtienen de la página (y reiniciando R o forzando a que las levante de nuevo) va a poder utilizarse.

Está configurado como un package de R, por lo que si lo forkean, lo que tienen que hacer es abrir el proyecto e ir a: Build -> Clean and Install

Luego, cuando lo quieren usar:

library(methodsPPI)

Algunas de las funciones tienen el help.

Ej: ?methodsPPI::getPPILogin2 

Y les arrojará el help.
