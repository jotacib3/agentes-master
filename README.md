# Projecto de la Asignatura Simulación y Declarativa

## A tener en cuenta
- Si deseca cambiar el intervalo de cambio aleatorio del tablero puede ir a la linea 33 de Lib.hs
```hs
| thisIteration `rem` 4 == 0 = Just newEnvironment
```
y cambiar 4 por el valor que desee distinot de 0 claro
- En la linea 91 del archivo Lib.hs defino de forma fija 14 como la cantidad de iteracciones(tiempo)
- EN la linea 272 del archivo Behavior.hs defino la probabilidad del niño de ensuciar la casilla que deja
- Si desea ejecutar paso a paso vaya a la linea 85 del archivo Lib.hs y descoméntela
## Dependencies
EL projecto se desarolló con stack y tiene las dependencias especificadas en el package.yml, se corre con el comando

```
stack run
```

## TODO
Agregar las cosas a tener en cuenta que se pueda aceder y editar desde el runSimulation en el Lib.hs como todas las demás variables