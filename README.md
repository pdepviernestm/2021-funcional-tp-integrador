# TP Integrador

## El enunciado

https://docs.google.com/document/d/1ylFqJUVLWK8Vu-DkkdYnOi_FuLulKhPnmprVPRsa6us/edit?usp=sharing

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste la kata. Ejemplo: `cd /home/juan/haskell/2021-funcional-tp-integrador`. Ejecutar `stack build --test`.
- [ ] Reemplazar la lista de integrantes con los nombres de los integrantes del equipo en el archivo README.md
- [ ] Resolver el ejercicio siguiendo [un esquema de trabajo](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md), eso incluye
- [ ] Ejecutar los tests con `stack test` y que den verde
- [ ] A medida que vas resolviendo el ejercicio, subir [el progreso a git](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/git/resolverConflictos.md)

## Integrantes

**Equipo:**  Team Rocket

- Ayelen Garcia (@ayelenMacarena)
- Federico Romero (@fecheromero)
- Juan Fernandes (@juanFdS)

## Objetivos

Los objetivos de este tp son practicar los conceptos vistos del paradigma funcional.

## Ayuda

Si tienen dudas con Haskell pueden ayudarte todo el tiempo con esta documentación

- [Guía de lenguajes](https://docs.google.com/document/d/1oJ-tyQJoBtJh0kFcsV9wSUpgpopjGtoyhJdPUdjFIJQ/edit?usp=sharing), un resumen de las principales funciones que vienen con Haskell.
- [Hoogle](https://www.haskell.org/hoogle/), un motor de búsqueda específico para Haskell.

Aparte, siempre pueden preguntar a sus ayudantes en discord!

Y para comenzar a trabajar con Git les recomendamos [este apunte inicial de Git](https://docs.google.com/document/d/1ozqfYCwt-37stynmgAd5wJlNOFKWYQeIZoeqXpAEs0I/edit) o estos videos donde se explica como usar Git:
- [Parte 1: Qué es GIT y cómo clonar el repo basado en GitHub classroom](https://www.youtube.com/watch?v=rRKe7l-ZNvM)
- [Parte 2: Uso básico de GIT con status, add, reset, commit, push](https://www.youtube.com/watch?v=OgasfM5qJJE)
- [Parte 3: Resolución de conflictos](https://www.youtube.com/watch?v=sKcN7cWFniw)

### Probando cosas por consola

La forma que recomendamos de resolver la kata es no programar todo de una y después ver si anda, si no ir probando en cada paso a medida que van programando cada función.

Para esto, les recomiendo que usen mucho `stack ghci` para probar cosas por consola, y vuelvo a linkear [esta página](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md#comandos-%C3%BAtiles) donde se explican un par de cositas de como usar `ghci`.

### Testeo automatizado

Nuestra solución tiene que estar escrita en el archivo `Library.hs` del directorio `src`, entonces podemos correr pruebas **automatizadas** en la terminal:

```bash
stack test
```

También pueden usar ghcid para correr los tests automáticamente como se explica [acá](https://github.com/pdepviernestm/2021-clases/blob/main/clase-02/correrTestsMasRapidoConGhcid.md).

## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
