# Mini Parser de JSON

Este es un mini parser de JSON desarrollado como proyecto de prueba. Permite analizar y manipular estructuras JSON de manera sencilla.

Parte del código fue visto en el curso de [Abordaje Funcional a EDSLs en el ECI 37 (2024)](https://eci.dc.uba.ar/cursos-eci/n1-abordaje-funcional-a-edsls/).

## Cómo utilizarlo

Para utilizar este parser, sigue estos pasos:

1. Importa el módulo `JSONParser` en tu proyecto:

   ```haskell
   import JSONParser (parseJson, parseFile, foldrJson, parseJsonM, parseFileM, JSON(..))
   ```

2. Utiliza las funciones proporcionadas para parsear y manipular JSON:

   - `parseJson`: Parsea una cadena JSON y devuelve un objeto `JSON`.
   - `parseFile`: Lee un archivo JSON y devuelve un objeto `JSON`.
   - `parseJsonM`: Parsea una cadena JSON y devuelve un `Maybe JSON`.
   - `parseFileM`: Lee un archivo JSON y devuelve un `Maybe JSON`.
   - `foldrJson`: Permite recorrer y transformar estructuras JSON.

## Interfaz

El parser define el tipo de datos `JSON` con los siguientes constructores:

- `JSONBool`: Representa un valor booleano.
- `JSONNumber`: Representa un número.
- `JSONString`: Representa una cadena de texto.
- `JSONObject`: Representa un objeto JSON (pares clave-valor).
- `JSONList`: Representa una lista JSON.

## Archivo de Ejemplo `Main.hs`

El archivo `Main.hs` es un ejemplo de cómo utilizar el parser.

La función `todosLosNombresDePokemones` EN `Main.hs` lee el archivo folder/pokemones.json, lo parsea y extrae la lista de nombres de los pokémones.

## Parseo de Archivos JSON desde la Línea de Comandos

Para parsear cualquier archivo JSON desde la línea de comandos, se puede utilizar la función `parseFileCLI` incluida en el archivo `Main.hs`.