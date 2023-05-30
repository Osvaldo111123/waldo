#1.1.1. Subsección 1.1.1: Distribución Uniforme1.1.1. 
#1. Un juego de dados justo se lanza tres veces. ¿Cuál es la probabilidad de obtener una suma de 10?
#(1,1), (1,2).(1,3),(1,4),(1,5),(1,6)
#(2,1), (2,2).(2,3),(2,4),(2,5),(2,6)
#(3,1), (3,2).(3,3),(3,4),(3,5),(3,6)  
#(4,1), (4,2).(4,3),(4,4),(4,5),(4,6)
#(5,1), (5,2).(5,3),(5,4),(5,5),(5,6)
#(6,1), (6,2).(6,3),(6,4),(6,5),(6,6)
#Que la suma sea 10, hay: (4,6),(5,5),(6,4) 3 posibles resultados de 36
p <- 3/36
print(paste0("La probabilidad de que P(X=10) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#2. Una urna contiene 10 bolas numeradas del 1 al 10. Si se elige una bola al azar, 
#¿cuál es la probabilidad de que sea un número par?
#1,2,3,4,5,6,7,8,9,10
#hay 5 numeros pares de 10 numeros totales
p <- 5/10
print(paste0("La probabilidad de que P(X=par) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#3. Un fabricante de piezas de repuesto sabe que el 5 % de las piezas que produce 
#son defectuosas. Si se eligen aleatoriamente 5 piezas, 
#¿cuál es la probabilidad de que al menos una de ellas sea defectuosa?
#de una pieza su probabilidad de ser defectuosa es del 5% 
#p(X=1) = 0.05
p <- punif(1,0,5)
print(paste0("La probabilidad de que P(X<=1) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#4. Un fabricante de cartas de póker quiere asegurarse de que todas las cartas tengan la 
#misma probabilidad de ser seleccionadas al azar. ¿Cuántas cartas debe incluir en su mazo?

print("las 52 cartas tienen la misma propabilidad de ser selccionadas al azar, 
      asi que debe incluir las 52 sin problemas") 
####---------------------------------------------####-------------------------------------------#####
#5. Un estudiante elige al azar una página de un libro y cuenta el número de palabras 
#que contiene. Si el libro tiene 200 páginas y un promedio de 1000 palabras por página, 
#¿cuál es la probabilidad de que el número de palabras en una página elegida 
#al azar esté entre 900 y 1100?
#probalidad de tomar una hoja cualquiera es de
# p <- (b-a)/n
p <- (1100-900)/1000
print(paste0("La probabilidad de que P(900>=X<=1100) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#6. Un vendedor de seguros tiene 10 pólizas que puede vender. Si se eligen al azar 3 pólizas,
#¿cuál es la probabilidad de que al menos una de ellas sea vendida?
p <- dunif(1,0,3)
print(paste0("La probabilidad de que P(X<=1) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#7. Un fabricante de dulces quiere que cada bolsa contenga una cantidad igual de caramelos 
#de cada sabor. Si hay 4 sabores de caramelos y cada bolsa contiene 20 caramelos, 
#¿cuál es la probabilidad de que haya exactamente 5
#caramelos de cada sabor en una bolsa seleccionada al azar?

print("La probalida de que P(x=5) seria del 100% dado que si se quieren el mismo numero de 
      caramelos este en la bolsa y esta tenga 20, seria dividir los 20 caramelos totales
      entre en numero de sabores y este nos dara 5 y cumple con lo requerido")  
####---------------------------------------------####-------------------------------------------#####
#8. Una persona lanza una moneda 5 veces. ¿
#Cuál es la probabilidad de que obtenga exactamente 2 caras?
p <- punif(2,0,5)
print(paste0("La probabilidad de que P(X=2) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#9. Un jugador de fútbol tiene una tasa de éxito del 80 % en tiros libres. 
#Si tira 5 tiros libres, ¿cuál es la probabilidad
#de que haga exactamente 4 goles?
#la tasa promedio es de 0.8 por cada tiro
p <- 0.8^4
print(paste0("La probabilidad de que P(X=4) es: ",p))
####---------------------------------------------####-------------------------------------------#####
#10. Se lanza un dado justo hasta obtener un número par. 
#¿Cuál es la probabilidad de que se necesiten al menos 3 lanzamientos?
#1,2,3,4,5,6
#probailidad de que NO caiga par en 2 intentos
x <- (3/6)^2
#probabilidad de que caiga par en el 3er intento
p <- (3/6) * x
print(paste0("La probabilidad de que P(X=2) es: ",p))

#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#Subsección 1.1.2: Distribución Bernoulii
#son eventos que se calculan una sola vez

#Un jugador de baloncesto tiene una tasa de acierto del 70 % en tiros libres. 
#¿Cuál es la probabilidad de que haga el primer tiro libre?
n <- 1; p <- 0.7; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####
#2. Un anuncio en línea tiene una tasa de clics del 2 %. 
#Si se muestra el anuncio a 1000 personas, 
#¿cuál es la probabilidad de que exactamente 20 personas hagan clic en él?
n <- 1000; p <- 0.02; x <- 20:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
#graficar la distribucion binomial
print(paste0("La probabilidad de que P(X=20) = ", dbinom(x, size = n, prob = p)))
#barplot(prob, names.arg = x, col = "lightblue",
#        main = "Distribucion Binomial", xlab ="Numero de exitos que tienen defecto",
#        ylab = "Probabilidad")
####---------------------------------------------####-------------------------------------------#####
#3. Un fabricante de bombillas sabe que el 3 % de las bombillas que produce son defectuosas. 
#Si se elige una bombilla al azar, ¿cuál es la probabilidad de que sea defectuosa?
n <- 1; p <- 0.03; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####
#4. Un jugador de fútbol americano tiene una tasa de éxito del 90 % en tiros de campo. 
#Si intenta un tiro de campo, ¿cuál es la probabilidad de que tenga éxito?
n <- 1; p <- 0.9; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####  
#5. La probabilidad de que un jugador de baloncesto haga un tiro libre es de 0.85. 
#Si un jugador tira un solo tiro libre, ¿cuál es la probabilidad de que lo haga?
n <- 1; p <- 0.85; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####  
#6. La probabilidad de que una persona enferma de gripe tenga fiebre es de 0.9. 
#Si se selecciona aleatoriamente a una persona enferma de gripe,
#¿cuál es la probabilidad de que tenga fiebre?
n <- 1; p <- 0.9; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#7. La probabilidad de que un estudiante apruebe un examen es de 0.6. 
#Si se selecciona aleatoriamente a un estudiante, 
#¿cuál es la probabilidad de que apruebe su examen?
n <- 1; p <- 0.6; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#8. La probabilidad de que una persona que juega a la ruleta gane en una ronda es de 0.2. 
#Si una persona juega una sola ronda, ¿cuál es la probabilidad de que gane?
n <- 1; p <- 0.2; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#9. La probabilidad de que una persona contratada por una empresa sea un buen empleado 
#es de 0.7. Si una empresa contrata a una sola persona, 
#¿cuál es la probabilidad de que sea un buen empleado?
n <- 1; p <- 0.7; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#10. La probabilidad de que una persona en una ciudad determinada tenga seguro 
#médico es de 0.4. Si se selecciona aleatoriamente a una persona en la ciudad,
#¿cuál es la probabilidad de que tenga seguro médico?
n <- 1; p <- 0.4; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#11. La probabilidad de que una persona llegue a tiempo a una cita es de 0.8. 
#Si una persona tiene una sola cita,
#¿cuál es la probabilidad de que llegue a tiempo?
n <- 1; p <- 0.8; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#12. La probabilidad de que un estudiante se enferme durante un semestre es de 0.3.
#Si se selecciona aleatoriamente
#a un estudiante, ¿cuál es la probabilidad de que se enferme durante el semestre?
n <- 1; p <- 0.3; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))
####---------------------------------------------####-------------------------------------------#####    
#13. La probabilidad de que un equipo de fútbol gane un partido es de 0.6. 
#Si un equipo juega un solo partido,
#¿cuál es la probabilidad de que gane?
n <- 1; p <- 0.6; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))  
####---------------------------------------------####-------------------------------------------#####    
#14. La probabilidad de que una persona responda correctamente una pregunta en un 
#examen de opción múltiple es de 0.25. Si una persona responde una sola pregunta, 
#¿cuál es la probabilidad de que responda correctamente?
n <- 1; p <- 0.25; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=1) = ", dbinom(x, size = n, prob = p)))  
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#Distribucion Discretas
#1.1.3. Subsección 1.1.3: Distribución Binomial
#Calcular la distribucion binomial
#son eventos que se calculas mas de una vez en el mismo evento
#1. Una fábrica de chocolate sabe que el 10 % de sus barras de chocolate tienen algún defecto. 
#Si se seleccionan al azar 5 barras, 
#¿cuál es la probabilidad de que al menos una tenga defectos?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 5; p <- 0.1; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
#graficar la distribucion binomial
print(paste0("La probabilidad de que al menos una es P(X<=1) = ", sum(dbinom(c(0,1), size = n, prob = p)) ))
#barplot(prob, names.arg = x, col = "lightblue",
#        main = "Distribucion Binomial", xlab ="Numero de exitos que tienen defecto",
#        ylab = "Probabilidad")  
####---------------------------------------------####-------------------------------------------#####    
#2. Un agente de bienes raíces sabe que la probabilidad de que un comprador cierre 
#un trato es del 40 %. Si el agente tiene 8 posibles compradores interesados, 
#¿cuál es la probabilidad de que exactamente 3 cierren un trato?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 8; p <- 0.4; x <- 3:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que P(X=3) = ", dbinom(x, size = n, prob = p))) 
####---------------------------------------------####-------------------------------------------#####    
#3. Un equipo de béisbol tiene una tasa de éxito del 70 % en bateo. 
#Si el equipo batea 25 veces, ¿cuál es la
#probabilidad de que al menos 18 de los bateos sean exitosos?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 25; p <- 0.7; x <- 18:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos 18 es P(X<=18) = ", sum(dbinom(c(0,18), size = n, prob = p)) ))
####---------------------------------------------####-------------------------------------------#####    
#4. Una empresa de marketing sabe que la tasa de respuesta a un correo electrónico 
#es del 20 %. Si envían 100 correos electrónicos, 
#¿cuál es la probabilidad de que menos de 10 personas respondan?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 100; p <- 0.2; x <- 9:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que menos de 10 es P(X<10) = ", sum(dbinom(c(0,9), size = n, prob = p)) ))
####---------------------------------------------####-------------------------------------------#####    
#5. Un investigador sabe que la probabilidad de que un ratón de laboratorio
#tenga una mutación específica es del 5 %. Si el investigador utiliza 30 ratones,
#¿cuál es la probabilidad de que al menos uno tenga la mutación?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 30; p <- 0.05; x <- 1:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos 1 es P(X<=1) = ", sum(dbinom(c(0,1), size = n, prob = p)) ))
####---------------------------------------------####-------------------------------------------#####    
#6. Una compañía de seguros sabe que la probabilidad de que un cliente haga un reclamo 
#es del 10 %. Si la compañía tiene 500 clientes, 
#¿cuál es la probabilidad de que al menos 50 hagan un reclamo?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 500; p <- 0.1; x <- 50:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos 50 es P(X<=50) = ", sum(dbinom(c(0,50), size = n, prob = p)) ))
####---------------------------------------------####-------------------------------------------#####    
#7. Un centro de llamadas sabe que la probabilidad de que un agente 
#cierre una venta es del 25 %. Si el centro de llamadas tiene 20 agentes, 
#¿cuál es la probabilidad de que al menos 5 cierren ventas en un día?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 20; p <- 0.25; x <- 5:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos 5 es P(X<=5) = ", sum(dbinom(c(0,50), size = n, prob = p)) ))  
####---------------------------------------------####-------------------------------------------#####    
#8. Un fabricante de juguetes sabe que la probabilidad de que un juguete sea defectuoso 
#es del 5 %. Si se producen 200 juguetes, 
#¿cuál es la probabilidad de que exactamente 15 sean defectuosos?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 200; p <- 0.05; x <- 5:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que sean 15 es P(X=15) = ", dbinom(x, size = n, prob = p))) 
####---------------------------------------------####-------------------------------------------#####    
#9. Un casino sabe que la probabilidad de que un jugador gane en la ruleta es del 47 %.
#Si un jugador juega 10
#rondas, ¿cuál es la probabilidad de que gane al menos 6 rondas?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 10; p <- 0.47; x <- 6:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos sean 6 es P(X<=6) = ",sum(dbinom(c(0,6), size = n, prob = p)) ))  
####---------------------------------------------####-------------------------------------------#####    
#10. Una tienda de ropa sabe que la probabilidad de que un cliente 
#compre un artículo es del 30 %. Si 50 clientes
#entran a la tienda, ¿cuál es la probabilidad de que al menos 10 compren algún artículo?
#n-numero de eventos
#p-funcion de probabilidad 
#x-numero de existos
n <- 50; p <- 0.30; x <- 10:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
print(paste0("La probabilidad de que al menos sean 10 es P(X<=10) = ",sum(dbinom(c(0,10), size = n, prob = p)) ))  
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.1.4. Subsección 1.1.1: Distribución Poisson
#1. Una tienda de abarrotes recibe un promedio de 5 clientes por hora. 
#¿Cuál es la probabilidad de que lleguen exactamente 10 clientes en las próximas dos horas?
# 1 horas - 5 clientes
# 2 horas - 10 clientes
lambda = 10
x = 10
ax = dpois(x, lambda)
print(paste0("La probabilidad de que P(X=10) = ",dpois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#2. Un centro de llamadas recibe en promedio 15 llamadas por hora. 
#¿Cuál es la probabilidad de que reciban al menos 20 llamadas en una hora determinada?
#15 llamadas - 1 hora
#20 llamadas
lambda = 15
x = 20
ax = ppois(x, lambda)
print(paste0("La probabilidad de que al menos sea 20 P(X<=20) = ",ppois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#3. Un restaurante recibe un promedio de 3 quejas de clientes por día. 
#¿Cuál es la probabilidad de que reciba exactamente 2 quejas en un día?
# 3 quejas - 1 dia
# 2 quejas 
lambda = 3
x = 2
ax = dpois(x, lambda)
print(paste0("La probabilidad de que sea 2 P(X=2) = ",dpois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#4. Una empresa de envío de paquetes recibe un promedio de 4 paquetes por hora.
#¿Cuál es la probabilidad de que reciban al menos 6 paquetes en una hora determinada?
# 4 paquetes - 1 hora
# 6 paquetes 
lambda = 4
x = 6
ax = ppois(x, lambda)
print(paste0("La probabilidad de que al menos sea 6 P(X<=6) = ",ppois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#5. Un médico atiende un promedio de 2 pacientes por hora. 
#¿Cuál es la probabilidad de que atienda exactamente 5 pacientes en un período de 3 horas?
# 2 pacientes - 1 hora
# 6 pacientes - 3 horas
lambda = 6
x = 5
ax = dpois(x, lambda)
print(paste0("La probabilidad de que sea 5 P(X=5) = ",dpois(x, lambda)))   
####---------------------------------------------####-------------------------------------------#####    
#6. Un canal de noticias transmite un promedio de 3 noticias de última hora por día. 
#¿Cuál es la probabilidad de
#que transmitan exactamente 2 noticias de última hora en un día determinado?
# 3 noticias - 1 hora
# 2 dias
lambda = 3
x = 2
ax = dpois(x, lambda)
print(paste0("La probabilidad de que sea 2 P(X=2) = ",dpois(x, lambda)))  
####---------------------------------------------####-------------------------------------------#####    
#7. Una fábrica produce un promedio de 10 piezas defectuosas por día. 
#¿Cuál es la probabilidad de que produzcan
#al menos 15 piezas defectuosas en un día determinado?
#10 piezas - 1 dia
#15 piezas
lambda = 10
x = 15
ax = ppois(x, lambda)
print(paste0("La probabilidad de que al menos sea 15 P(X<=15) = ",ppois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#8. Un equipo de fútbol anota en promedio 2 goles por partido. 
#¿Cuál es la probabilidad de que anoten exactamente
#3 goles en un partido determinado?
#2 goles - 1 partido
#3 goles
lambda = 2
x = 3
ax = dpois(x, lambda)
print(paste0("La probabilidad de que sea 3 P(X=3) = ",dpois(x, lambda)))  
####---------------------------------------------####-------------------------------------------#####    
#9. Un supermercado vende en promedio 20 paquetes de arroz por día. 
#¿Cuál es la probabilidad de que vendan
#menos de 15 paquetes de arroz en un día determinado?
#20 paquetes - 1 dia
#15 paques
lambda = 20
x = 14
ax = ppois(x, lambda)
print(paste0("La probabilidad de que menos de 15 P(X<14) = ",ppois(x, lambda))) 
####---------------------------------------------####-------------------------------------------#####    
#10. Una empresa de taxis recibe en promedio 8 solicitudes de viaje por hora.
#¿Cuál es la probabilidad de que
#reciban al menos 12 solicitudes de viaje en una hora determinada?
#8 solicitudes - 1 hora
#12 solicitudes
lambda = 8
x = 12
ax = ppois(x, lambda)
print(paste0("La probabilidad de que al menos de 12 P(X<=12) = ",ppois(x, lambda))) 
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.1.5. Subsección 1.1.1: Distribución Geométrica
#1. La probabilidad de que un equipo de baloncesto anote un punto en un tiro libre 
#es del 70 %. La distribución geométrica se puede usar para calcular 
#la probabilidad de que un jugador necesite más de un intento para anotar un tiro libre
x = 1
p = 0.7
#p(x>1)= 1-p(x<=1)
ax = 1-dgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 1 P(X>1) = ",ax)) 
####---------------------------------------------####-------------------------------------------#####    
#2. Un sitio web tiene una tasa de conversión del 5 %, lo que significa que el 5 % 
#de los visitantes del sitio web se convierten en clientes. 
#La distribución geométrica se puede usar para calcular la probabilidad de que se
#necesiten más de 10 visitas al sitio web para obtener un cliente.
x = 10
p = 0.05
#p(x>10) = 1-p(x<=10)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 10 P(X>10) = ",ax)) 
####---------------------------------------------####-------------------------------------------#####    
#3. Un restaurante tiene una probabilidad del 20 % de que un cliente ordene un postre. 
#La distribución geométrica se puede usar para calcular la probabilidad de que 
#se necesiten más de 5 clientes antes de que alguien ordene un postre.
x = 5
p = 0.2
#p(x>5) = 1-p(x<=5)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 5 P(X>5) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#4. Un jugador de póker tiene una probabilidad del 2 % de recibir un as en su mano inicial. 
#La distribución geométrica se puede usar para calcular la probabilidad de que el 
#jugador necesite más de 3 manos para recibir su primer as.
x = 3
p = 0.02
#p(x>3) = 1-p(x<=3)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 3 P(X>3) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#5. Una empresa de marketing envía correos electrónicos a una lista de clientes potenciales.
#La probabilidad de que un cliente potencial abra el correo electrónico es del 10 %. 
#La distribución geométrica se puede usar para calcular la probabilidad de que se 
#necesiten más de 5 correos electrónicos para que un cliente potencial abra uno.
x = 5
p = 0.1
#p(x>5) = 1-p(x<=5)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 5 P(X>5) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#6. Un jugador de béisbol tiene una probabilidad del 25 % de conectar una pelota en un 
#turno al bate. La distribución geométrica se puede usar para calcular la probabilidad
#de que el jugador necesite más de 4 turnos al bate para conectar su primer hit.
x = 4
p = 0.25
#p(x>4) = 1-p(x<=4)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 4 P(X>4) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#7. Un vendedor tiene una probabilidad del 30 % de cerrar una venta en una llamada de ventas. 
#La distribución geométrica se puede usar para calcular la probabilidad de que el 
#vendedor necesite más de 6 llamadas de ventas para cerrar su primera venta.
x = 6
p = 0.3
#p(x>6) = 1-p(x<=6)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 6 P(X>6) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#8. La probabilidad de que un estudiante pase un examen es del 60 %. 
#La distribución geométrica se puede usar
#para calcular la probabilidad de que el estudiante necesite más de 4 
#intentos para pasar el examen.
x = 4
p = 0.6
#p(x>4) = 1-p(x<=4)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 4 P(X>4) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#9. Un jugador de fútbol americano tiene una probabilidad del 5 % de interceptar un 
#pase en un juego. La distribución geométrica se puede usar para calcular la 
#probabilidad de que el jugador necesite más de 10 intentos para interceptar su primer pase.
x = 10
p = 0.05
#p(x>10) = 1-p(x<=10)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 10 P(X>10) = ",ax))
####---------------------------------------------####-------------------------------------------#####    
#10. Un pescador tiene una probabilidad del 10 % de capturar un pez en cada 
#lance de su caña de pesca. La distribución geométrica se puede usar para calcular 
#la probabilidad de que el pescador necesite más de 8 lances para capturar su primer pez.
x = 8
p = 0.1
#p(x>8) = 1-p(x<=8)
ax = 1-pgeom(x, p) 
print(paste0("La probabilidad de que sea mas de 8 P(X>8) = ",ax))
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.2. Sección 1.1: Variables Aleatorias Continuas

####---------------------------------------------####-------------------------------------------#####    
#1.2.2. Subsección 1.1.1: Distribución Exponencial
#1. Una empresa de mensajería promete entregar los paquetes dentro de 2 horas desde que se
#realiza el pedido. El tiempo que tarda en entregarse un paquete sigue una distribución 
#exponencial con una tasa de 0.5. ¿Cuál es la
#probabilidad de que un paquete se entregue en menos de 1 hora?
lambda = 0.5
x = 1
#p(x<1) = 1-p(x>=1)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<1) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#2. La vida útil de las bombillas de una fábrica sigue una distribución exponencial
#con una media de 800 horas.
#¿Cuál es la probabilidad de que una bombilla dure más de 1000 horas?
lambda = 1/800
x = 1000
#p(x>1000) = 1-p(x<=1000)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X>1000) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#3. Un sistema informático tiene una tasa de fallos de 0.01 por hora.
#¿Cuál es la probabilidad de que el sistema falle en menos de 50 horas?
lambda = 0.01
x = 50
#p(x<50) = 1-p(x>=50)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<50) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#4. Un vendedor tarda en promedio 15 minutos en atender a un cliente en su tienda. 
#El tiempo que tarda en atender a los clientes sigue una distribución exponencial con una 
#tasa de 0.25. ¿Cuál es la probabilidad de que atienda a un cliente en menos de 10 minutos?
lambda = 0.25
x = 10
#p(x<10) = 1-p(x>=10)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<10) = ",ax))    
####---------------------------------------------####-------------------------------------------#####    
#5. El tiempo entre llegadas de dos clientes a una tienda sigue una distribución exponencial 
#con una media de 5 minutos. ¿Cuál es la probabilidad de que no llegue ningún 
#cliente en 10 minutos?  
lambda = 1/5
x = 10
#p(x<=10) = 1-p(x>10)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<=10) = ",ax))    
####---------------------------------------------####-------------------------------------------#####    
#6. El tiempo que tarda una persona en recorrer una distancia de 5 km en bicicleta sigue 
#una distribución exponencial con una media de 30 minutos. 
#¿Cuál es la probabilidad de que la persona tarde menos de 20 minutos?
lambda = 1/30
x = 20
#p(x<20) = 1-p(x>=20)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<20) = ",ax)) 
####---------------------------------------------####-------------------------------------------#####    
#7. El tiempo que tarda una máquina en producir una pieza sigue una distribución exponencial 
#con una tasa de 0.003 piezas por minuto. ¿Cuál es la probabilidad de que la máquina 
#produzca al menos 10 piezas en 5 minutos?
lambda = 1/5
x = 10
#p(x<=10) = 1-p(x>10)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<=10) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#8. El tiempo que tarda un proceso industrial en completarse sigue una distribución exponencial 
#con una media de 8 horas. ¿Cuál es la probabilidad de que el proceso tarde más de 10 horas 
#en completarse?
lambda = 1/8
x = 10
#p(x>10) = 1-p(x<=10)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X>10) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#9. El tiempo que tarda un cliente en pagar en una caja sigue una distribución exponencial
#con una media de 2 minutos. Si hay 10 clientes esperando en la fila,
#¿cuál es la probabilidad de que un cliente nuevo tenga que esperar más de 5 minutos para pagar?
lambda = 1/10
x = 5
#p(x>5) = 1-p(x<=5)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X>5) = ",ax))    
####---------------------------------------------####-------------------------------------------#####    
#10. La tasa de llegada de un evento de demanda en un centro de atención telefónica sigue 
#una distribución exponencial con una media de 4 eventos por hora. 
#¿Cuál es la probabilidad de que no llegue ningún evento en 10 minutos?
lambda = 1/4
x = 10
#p(x<=10) = 1-p(x>10)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<=10) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#11. El tiempo que tarda una persona en llegar a una estación de tren sigue una distribución 
#exponencial con una media de 20 minutos. Si una persona tiene que tomar un tren que sale 
#en 30 minutos, ¿cuál es la probabilidad de que llegue a tiempo?
lambda = 1/20
x = 30
#p(x<30) = 1-p(x>=10)
ax = 1-dexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<30) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#12. El tiempo que tarda un coche en recorrer una distancia de 100 km en una carretera 
#sigue una distribución exponencial con una media de 2 horas.
#¿Cuál es la probabilidad de que el coche tarde menos de 1 hora y media?
lambda = 1/2
x = 1.5
#p(x<1.5) = 1-p(x>=1.5)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<1.5) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#13. Se sabe que el tiempo que tarda un técnico en reparar un electrodoméstico sigue 
#una distribución exponencial con una tasa de 0.05. ¿Cuál es la probabilidad de que el 
#técnico repare el electrodoméstico en menos de 20 minutos?
lambda = 0.05
x = 20
#p(x<20) = 1-p(x>=20)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<20) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#14. Una empresa de mensajería sabe que el tiempo que tarda en entregar un paquete sigue 
#una distribución exponencial con una tasa de 0.1. Si se tienen que entregar 100 paquetes, 
#¿cuál es la probabilidad de que la empresa tarde menos de 30 minutos en entregar cada paquete?
lambda = 0.1
x = 30
#p(x<30) = 1-p(x>=30)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<30) = ",ax))    
####---------------------------------------------####-------------------------------------------#####    
#15. Se sabe que el tiempo que tarda en atenderse un cliente en una tienda sigue una 
#distribución exponencial con una tasa de 0.2. ¿Cuál es la probabilidad de que un cliente 
#espere más de 10 minutos para ser atendido?
lambda = 0.2
x = 10
#p(x>10) = 1-p(x<=10)
ax = 1-dexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<10) = ",ax))    
####---------------------------------------------####-------------------------------------------#####    
#16. Un fabricante de baterías sabe que la duración de sus baterías sigue una distribución
#exponencial con una tasa de 0.01. Si se venden 500 baterías, 
#¿cuál es la probabilidad de que al menos 10 de ellas duren más de 100 horas?
lambda = 0.01
x = 100
#p(x>100) = 1-p(x<=100)
ax = 1-dexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X>100) = ",ax))   
####---------------------------------------------####-------------------------------------------#####    
#17. Un vendedor de autos usados sabe que la cantidad de tiempo que tarda en vender un auto 
#sigue una distribución exponencial con una tasa de 0.1. Si tiene 20 autos en venta, 
#¿cuál es la probabilidad de que tarde más de 5 horas en venderlos todos?
lambda = 0.1
x = 5
#p(x>5) = 1-p(x<=5)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X>5) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#18. Se sabe que la cantidad de tiempo que tarda una persona en caminar desde su casa hasta 
#la estación de tren sigue una distribución exponencial con una tasa de 0.05. 
#Si la distancia es de 1 kilómetro, ¿cuál es la
#probabilidad de que la persona tarde menos de 20 minutos en llegar a la estación?
lambda = 0.05
x = 20
#p(x<20) = 1-p(x>=20)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<20) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#19. Un banco sabe que la cantidad de tiempo que tarda un cliente en realizar una transacción
#sigue una distribución exponencial con una tasa de 0.15. Si se tienen que atender a 50 
#clientes, ¿cuál es la probabilidad de que el
#banco tarde menos de 2 horas en atenderlos a todos?
lambda = 0.15
x = 2
#p(x<2) = 1-p(x>=2)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<2) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#20. Se sabe que la cantidad de tiempo que tarda en fallar un equipo electrónico sigue una 
#distribución exponencial con una tasa de 0.02. Si se tienen 100 equipos en operación, 
#¿cuál es la probabilidad de que al menos 5 fallen en menos de 50 horas?
lambda = 0.02
x = 50
#p(x<50) = 1-p(x>=50)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<50) = ",ax))  
####---------------------------------------------####-------------------------------------------#####    
#21. Un restaurante sabe que la cantidad de tiempo que tarda un cliente en terminar de 
#comer sigue una distribución exponencial con una tasa de 0.2. Si se tienen 30 mesas, 
#¿cuál es la probabilidad de que al menos 5 de ellas estén desocupadas en menos de 1 hora?
lambda = 0.2
x = 1
#p(x<1) = 1-p(x>=1)
ax = 1-pexp(x, lambda) 
print(paste0("La probabilidad de que sea P(X<1) = ",ax))  
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.2.3. Subsección 1.1.1: Distribución Normal
#1. Una fábrica de papel produce rollos de papel higiénico que tienen una longitud media de 
#100 metros y una desviación estándar de 5 metros. ¿Cuál es la probabilidad de que un rollo 
#de papel higiénico tenga una longitud entre 95 y 105 metros?
#p(105) - p(95)
media = 100
sd = 5
p = pnorm(105,media, sd) - pnorm(95,media,sd)
print(paste0("La probabilidad de que sea P(95>X<105) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#2. La altura de los estudiantes de una escuela secundaria sigue una distribución normal 
#con media de 165 cm y desviación estándar de 10 cm. 
#¿Cuál es la probabilidad de que un estudiante seleccionado al azar tenga una
#altura entre 150 y 180 cm?
media = 165
sd = 10
p = pnorm(180,media, sd) - pnorm(150,media,sd)
print(paste0("La probabilidad de que sea P(150>X<180) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#3. Un fabricante de zapatos produce zapatos que tienen una longitud media de 28 cm y 
#una desviación estándar de 2 cm. ¿Cuál es la probabilidad de que un zapato seleccionado 
#al azar tenga una longitud mayor que 32 cm?
media = 28
sd = 2
p =1 - pnorm(32,media, sd) 
print(paste0("La probabilidad de que sea P(X>32) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#4. La distribución de las calificaciones en un examen sigue una distribución normal con 
#una media de 70 y una desviación estándar de 10. ¿Cuál es la probabilidad de que un estudiante
#seleccionado al azar tenga una calificación menor a 60?
media = 70
sd = 10
p = pnorm(60,media, sd) 
print(paste0("La probabilidad de que sea P(X<60) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#5. La duración de las baterías de un dispositivo electrónico sigue una distribución normal 
#con una media de 10 horas y una desviación estándar de 2 horas. ¿Cuál es la probabilidad de 
#que una batería seleccionada al azar dure más de 12 horas?
media = 10
sd = 2
p = 1 - pnorm(12,media, sd) 
print(paste0("La probabilidad de que sea P(X>12) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#6. La distribución de los tiempos de espera en una fila sigue una distribución normal con una
#media de 5 minutos y una desviación estándar de 1 minuto. ¿Cuál es la probabilidad de que 
#un cliente tenga que esperar más de 8 minutos?
media = 5
sd = 1
p = 1 - pnorm(8,media, sd) 
print(paste0("La probabilidad de que sea P(X>8) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#7. El peso de los recién nacidos sigue una distribución normal con una media de 3.5 kg y una
#desviación estándar de 0.5 kg. ¿Cuál es la probabilidad de que un recién nacido pese menos
#de 2.5 kg?
media = 3.5
sd = 0.5
p = pnorm(2.5,media, sd) 
print(paste0("La probabilidad de que sea P(X<2.5) = ",p))     
####---------------------------------------------####-------------------------------------------#####    
#8. La velocidad de un vehículo sigue una distribución normal con una media de 60 km/h y
#una desviación estándar de 5 km/h. ¿Cuál es la probabilidad de que un vehículo viaje a una 
#velocidad mayor de 70 km/h?
media = 60
sd = 5
p = 1 - pnorm(70,media, sd) 
print(paste0("La probabilidad de que sea P(X>70) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#9. La distribución de los ingresos de los trabajadores en una empresa sigue una distribución 
#normal con una media de $50, 000 y una desviación estándar de $10, 000. ¿Cuál es la 
#probabilidad de que un trabajador gane menos de $30,000?
media = 50000
sd = 10000
p = pnorm(30000,media, sd) 
print(paste0("La probabilidad de que sea P(X<30000) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#10. La temperatura de una ciudad sigue una distribución normal con una media de 25 grados 
#Celsius y una desviación estándar de 2 grados Celsius. ¿Cuál es la probabilidad de que la 
#temperatura sea mayor de 30 grados Celsius?
media = 25
sd = 2
p = 1-pnorm(30,media, sd) 
print(paste0("La probabilidad de que sea P(X>30) = ",p)) 
####---------------------------------------------####-------------------------------------------#####    
#11. La altura de las botellas de refresco sigue una distribución normal con una 
#media de 20 cm y una desviación estándar de 1 cm. ¿Cuál es la probabilidad de que 
#una botella seleccionada al azar tenga una altura menor a 18 cm?
media = 20
sd = 1
p = pnorm(18,media, sd) 
print(paste0("La probabilidad de que sea P(X<18) = ",p)) 
####---------------------------------------------####-------------------------------------------#####    
#12. Si los puntajes de un examen están distribuidos normalmente con una media de 70 y 
#una desviación estándar de 10, ¿cuál es la probabilidad de obtener un puntaje mayor a 80?
media = 70
sd = 10
p = 1 - pnorm(80,media, sd) 
print(paste0("La probabilidad de que sea P(X>80) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#13. Suponga que el tiempo que se tarda en atender a un cliente en un banco sigue una
#distribución normal con media de 5 minutos y una desviación estándar de 1.2 minutos. 
#¿Cuál es la probabilidad de que un cliente sea atendido en menos de 4 minutos?
media = 5
sd = 1.2
p = pnorm(4,media, sd) 
print(paste0("La probabilidad de que sea P(X<4) = ",p))     
####---------------------------------------------####-------------------------------------------#####    
#14. Un estudio indica que el peso promedio de los estudiantes de una universidad es de 
#70 kg con una desviación estándar de 8 kg. Si se selecciona una muestra aleatoria de 25 
#estudiantes, ¿cuál es la probabilidad de que el peso promedio de la muestra sea mayor a 75 kg?
media = 70
sd = 8
p = 1 - pnorm(75,media, sd) 
print(paste0("La probabilidad de que sea P(X>75) = ",p)) 
####---------------------------------------------####-------------------------------------------#####    
#15. Suponga que la altura de los jugadores de un equipo de baloncesto se distribuye
#normalmente con una media de 200 cm y una desviación estándar de 10 cm. Si se selecciona
#un jugador al azar, ¿cuál es la probabilidad de que su altura sea mayor a 215 cm?
media = 200
sd = 10
p = 1 - pnorm(215,media, sd) 
print(paste0("La probabilidad de que sea P(X>215) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#16. Si los ingresos mensuales de los empleados de una empresa están distribuidos normalmente
#con una media de $2500 y una desviación estándar de $500, ¿cuál es la probabilidad de
#que un empleado tenga un ingreso mensual mayor a $3000?
media = 2500
sd = 500
p = 1 - pnorm(3000,media, sd) 
print(paste0("La probabilidad de que sea P(X>3000) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#17. La cantidad de días que tarda una empresa en pagar a sus proveedores sigue una 
#distribución normal con una media de 30 días y una desviación estándar de 5 días. Si se 
#selecciona un proveedor al azar, ¿cuál es la probabilidad de que la empresa le pague 
#en menos de 25 días?
media = 30
sd = 5
p = pnorm(25,media, sd) 
print(paste0("La probabilidad de que sea P(X<25) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#18. Suponga que el diámetro de los tornillos producidos por una fábrica sigue una 
#distribución normal con media de 4 cm y una desviación estándar de 0.2 cm. Si se selecciona 
#un tornillo al azar, ¿cuál es la probabilidad de que su diámetro esté entre 3.8 cm y 4.2 cm?
media = 4
sd = 0.2
p = pnorm(4.2,media, sd) - pnorm(3.8,media,sd) 
print(paste0("La probabilidad de que sea P(3.8>X<4.2) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#19. El tiempo que tarda un trabajador en completar una tarea sigue una distribución normal
#con media de 6 horas y una desviación estándar de 1 hora. Si se selecciona un trabajador al 
#azar, ¿cuál es la probabilidad de que complete la tarea en menos de 4 horas?
media = 6
sd = 1
p = pnorm(4,media, sd) 
print(paste0("La probabilidad de que sea P(X>4) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#20. Si la cantidad de gasolina que se llena en un tanque de un automóvil sigue una 
#distribución normal con una media de 50 litros y una desviación estándar de 5 litros, 
#¿cuál es la probabilidad de que se llenen entre 45 y 55 litros?
media = 50
sd = 5
p = pnorm(55,media, sd) - pnorm(45,media,sd) 
print(paste0("La probabilidad de que sea P(45>X<55) = ",p))  
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.2.4. Subsección 1.1.1: Distribución Gamma
#1. Un fabricante de bombillas sabe que la vida útil de las bombillas sigue una distribución 
#gamma con una media de 1500 horas y una desviación estándar de 500 horas. ¿Cuál es la 
#probabilidad de que una bombilla dure más de 2000 horas?
a = 500
media = 1/1500
p <- 1- pgamma(2000, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X>2000) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#2. Una planta de producción de papel produce rollos de papel de 100 metros de longitud.
#La distribución de la longitud de los rollos sigue una distribución gamma con una media de 
#105 metros y una desviación estándar de 10 metros. ¿Cuál es la probabilidad de que un
#rollo tenga una longitud de entre 90 y 110 metros?
a = 10
media = 1/105
p <- pgamma(110, shape = a, rate = media) - pgamma(90, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(90>X<110) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#3. La cantidad de tiempo que un grupo de estudiantes dedica a estudiar para un examen
#sigue una distribución gamma con una media de 20 horas y una desviación estándar de 5 horas.
#Si el tiempo mínimo requerido para pasar el examen es de 15 horas, ¿cuál es la probabilidad 
#de que un estudiante al azar pase el examen?
a = 5
media = 1/20
p <- pgamma(15, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<15) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#4. Un fabricante de baterías de automóvil sabe que el tiempo de vida de las baterías sigue
#una distribución gamma con una media de 4 años y una desviación estándar de 1 año. ¿Cuál es la
#probabilidad de que una batería dure menos de 2 años?
a = 1
media = 1/4
p <- pgamma(2, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<2) = ",p))     
####---------------------------------------------####-------------------------------------------#####    
#5. La cantidad de tiempo que una persona tarda en completar una tarea sigue una distribución 
#gamma con una media de 10 minutos y una desviación estándar de 2 minutos. Si se requiere que
#la tarea se complete en un tiempo máximo de 12 minutos, ¿cuál es la probabilidad de que una 
#persona al azar pueda completar la tarea a tiempo?
a = 2
media = 1/10
p <- pgamma(12, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<12) = ",p)) 
####---------------------------------------------####-------------------------------------------#####    
#6. Una fábrica de muebles produce sillas de madera. La cantidad de tiempo que tarda en 
#producir una silla sigue una distribución gamma con una media de 2 horas y una desviación 
#estándar de 0.5 horas. Si se requiere que se produzcan 10 sillas en un día de trabajo, 
#¿cuál es la probabilidad de que se logre completar la producción en un día?
a = 0.5
media = 1/2
p <- pgamma(10, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<10) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#7. Un fabricante de relojes sabe que la vida útil de sus relojes sigue una distribución gamma 
#con una media de 5 años y una desviación estándar de 1 año. Si un reloj es considerado 
#defectuoso si dura menos de 3 años, ¿cuál es la probabilidad de que un reloj al azar sea 
#considerado defectuoso?
a = 1
media = 1/5
p <- pgamma(3, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<3) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#8. Un fabricante de focos sabe que la vida útil de sus focos sigue una distribución gamma con 
#una media de 800 horas y una desviación estándar de 200 horas. Si se requiere que los focos 
#duren al menos 1000 horas, ¿cuál es la probabilidad de que un foco al azar dure el tiempo
#necesario?
a = 200
media = 1/800
p <- pgamma(1000, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<1000) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#9. La cantidad de tiempo que tarda un técnico en reparar un dispositivo electrónico sigue una
#distribución gamma con una media de 3 horas y una desviación estándar de 0.5 horas. Si se 
#requiere que el dispositivo sea reparado en un tiempo máximo de 4 horas, ¿cuál es la 
#probabilidad de que el técnico al azar pueda reparar el dispositivo a tiempo?
a = 0.5
media = 1/3
p <- pgamma(4, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<4) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#10. Se sabe que el tiempo de vida (en años) de una lámpara sigue una distribución gamma 
#con parámetros α = 3 y β = 2. Encuentra la probabilidad de que la lámpara dure más de 4 años.
a = 3
media = 1/2
p <- 1 - pgamma(4, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X>4) = ",p))     
####---------------------------------------------####-------------------------------------------#####    
#11. El tiempo que tarda en llegar un cliente al restaurante sigue una distribución gamma con
#parámetros α = 2 y β = 3. ¿Cuál es la probabilidad de que un cliente tarde más de 5 minutos
#en llegar?
a = 2
media = 1/3
p <- 1 - pgamma(5, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X>5) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#12. Se sabe que la cantidad de kilómetros que recorre un taxi en un día sigue una distribución
#gamma con parámetros α = 4 y β = 6. ¿Cuál es la probabilidad de que el taxi recorra menos 
#de 20 kilómetros en un día?
a = 4
media = 1/6
p <- pgamma(20, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<20) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#13. Un grupo de personas está siendo tratado con un medicamento cuyos efectos secundarios
#siguen una distribución gamma con parámetros α = 2 y β = 4. ¿Cuál es la probabilidad de que 
#el efecto secundario más duradero dure más de 6 horas?
a = 2
media = 1/4
p <- 1- pgamma(6, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X>6) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#14. La cantidad de tiempo que tarda una máquina en fabricar una pieza sigue una distribución 
#gamma con parámetros α = 4 y β = 5. ¿Cuál es la probabilidad de que la máquina tarde menos 
#de 10 minutos en fabricar una pieza?
a = 3
media = 1/5
p <- pgamma(10, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<10) = ",p))     
####---------------------------------------------####-------------------------------------------#####    
#15. Se sabe que la cantidad de kilómetros que puede recorrer un coche con un tanque de 
#gasolina sigue una distribución gamma con parámetros α = 3 y β = 8. Si el coche tiene un
#tanque de gasolina de 40 litros, ¿cuál es la probabilidad de que pueda recorrer más de
#600 kilómetros con un tanque lleno?
a = 3
media = 1/8
p <- pgamma(40, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<40) = ",p))   
####---------------------------------------------####-------------------------------------------#####    
#16. Se sabe que el tiempo que tarda en salir un autobús desde la terminal sigue una 
#distribución gamma con parámetros α = 3 y β = 2. ¿Cuál es la probabilidad de que el autobús
#tarde más de 5 minutos en salir?
a = 3
media = 1/2
p <- 1- pgamma(5, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X>5) = ",p))  
####---------------------------------------------####-------------------------------------------#####    
#17. La cantidad de tiempo que tarda un paquete en ser entregado sigue una distribución gamma 
#con parámetros α = 5 y β = 4. ¿Cuál es la probabilidad de que el paquete sea entregado antes 
#de 25 minutos?
a = 5
media = 1/4
p <- pgamma(25, shape = a, rate = media)
print(paste0("La probabilidad de que sea P(X<25) = ",p))  
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.2.5. Subsección 1.1.1: Distribución Beta
#1. Un sitio web de videos desea medir la tasa de clics en su botón de “reproducir”. Si se sabe 
#que el 10 % de los usuarios hacen clic en el botón, ¿cuál es la probabilidad de que de 20 
#usuarios, exactamente 3 hagan clic?
a = 20
beta = 3
p <- dbeta(0.1, shape1 = a, shape2 = beta) * 100
print(paste0("La probabilidad de que sea P(X=3) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#2. Una empresa quiere saber la probabilidad de que un producto defectuoso se encuentre en el 
#proceso de producción. Si se sabe que la tasa de producción de productos defectuosos es del 5%,
#¿cuál es la probabilidad de que de 200 productos producidos, exactamente 12 sean defectuosos?
a = 12
beta = 200
p <- dbeta(0.05, shape1 = a, shape2 = beta) / 100
print(paste0("La probabilidad de que sea P(X=12) = ",p))    
####---------------------------------------------####-------------------------------------------#####    
#3. Un sitio web de citas desea medir la tasa de conversión de las personas que visitan su 
#sitio y se suscriben a un plan premium. Si se sabe que el 2 % de las personas que visitan el 
#sitio se suscriben a un plan premium, ¿cuál es la probabilidad de que de 500 visitas, 
#exactamente 10 personas se suscriban?
a = 10
beta = 500
p <- dbeta(0.02, shape1 = a, shape2 = beta) / 100
print(paste0("La probabilidad de que sea P(X=10) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#4. Una compañía desea medir la tasa de conversión de los anuncios en línea. Si se sabe que el
#20 % de las personas que ven el anuncio hacen clic en él, ¿cuál es la probabilidad de que de 
#50 vistas, exactamente 12 personas hagan clic?
a = 12
beta = 50
p <- dbeta(0.2, shape1 = a, shape2 = beta) / 100
print(paste0("La probabilidad de que sea P(X=12) = ",p))  
####---------------------------------------------####-------------------------------------------#####      
#5. Un restaurante desea medir la satisfacción de sus clientes en relación con un nuevo plato
#que acaba de agregar al menú. Si se sabe que el 70 % de los clientes están satisfechos con el
#nuevo plato, ¿cuál es la probabilidad de que de 100 clientes, exactamente 50 estén satisfechos?
a = 50
beta = 100
#dbeta(x,alfa,beta)
p <- dbeta(0.7, shape1 = a, shape2 = beta)
print(paste0("La probabilidad de que sea P(X=12) = ",p))/ 100
####---------------------------------------------####-------------------------------------------#####      
#6. Una empresa de marketing desea conocer el porcentaje de conversiones de su última campaña
#publicitaria. Se seleccionan aleatoriamente 200 personas que han visto la publicidad y se 
#observa que 50 de ellas han realizado una compra. Suponiendo una distribución beta con
#parámetros a=2 y b=4, ¿cuál es la probabilidad de que la tasa de conversión esté entre 
#el 20 % y el 30 %?
a = 2
beta = 4
#dbeta(x,alfa,beta)
p <- pbeta(0.3, shape1 = a, shape2 = beta) -  pbeta(0.2, shape1 = a, shape2 = beta)
print(paste0("La probabilidad de que sea P(0.2>X<0.3) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#7. Un fabricante de pinturas quiere determinar la proporción de pigmento que debe agregar a su 
#fórmula para obtener el color deseado. Se realiza una prueba en la que se mezcla la fórmula con
#diferentes proporciones de pigmento y se mide la intensidad del color en una escala de 0 a 100. 
#Suponiendo una distribución beta con parámetros a=5 y b=2, ¿cuál es la probabilidad de que la
#proporción de pigmento necesaria para obtener una intensidad de color de al menos 80 sea menor 
#al 30 %?
a = 5
beta = 2
#dbeta(x,alfa,beta)
p <- 1 - pbeta(0.3, shape1 = a, shape2 = beta)
print(paste0("La probabilidad de que sea P(X>0.3) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#8. Un estudio de opinión pública desea conocer el porcentaje de personas que apoyan una 
#propuesta de ley. Se seleccionan aleatoriamente 1000 personas y se les pregunta si están a favor 
#o en contra. Se observa que 650 están a favor. Suponiendo una distribución beta con parámetros
#a=7 y b=5, ¿cuál es la probabilidad de que la tasa de aprobación esté entre el 60 % y el 70 %?
a = 7
beta = 5
#dbeta(x,alfa,beta)
p <- pbeta(0.7, shape1 = a, shape2 = beta) -  pbeta(0.6, shape1 = a, shape2 = beta)
print(paste0("La probabilidad de que sea P(0.6>X<0.7) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#9. Una empresa de seguros desea conocer la proporción de personas que renuevan su póliza de 
#seguro cada año. Se seleccionan aleatoriamente 5000 clientes y se observa que 4200 renuevan su 
#póliza. Suponiendo una distribución beta con parámetros a=15 y b=5, ¿cuál es la probabilidad de 
#que la tasa de renovación esté por encima del 85 %?
a = 15
beta = 5
#dbeta(x,alfa,beta)
p <-1 - pbeta(0.85, shape1 = a, shape2 = beta) 
print(paste0("La probabilidad de que sea P(X>0.85) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#10. Una compañía de transporte de paquetería desea conocer la proporción de envíos que llegan a su 
#destino en el plazo establecido. Se revisan aleatoriamente 100 envíos y se observa que 80 llegan a 
#tiempo. Suponiendo una distribución beta con parámetros a=8 y b=2, ¿cuál es la probabilidad de que la 
#tasa de envíos que llegan a tiempo esté por encima del 85 %?
a = 8
beta = 2
#dbeta(x,alfa,beta)
p <-1 - pbeta(0.85, shape1 = a, shape2 = beta) 
print(paste0("La probabilidad de que sea P(X>0.85) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#11. Una empresa de telecomunicaciones desea conocer la proporción de clientes que utilizan su 
#servicio de streaming de video. Se seleccionan aleatoriamente 300 clientes y se observa que 
#150 utilizan el servicio. Suponiendo una distribución beta con parámetros a=10 y b=10, 
#¿cuál es la probabilidad de que la tasa de uso del servicio esté entre el 40 % y el 60 %?
a = 10
beta = 10
#dbeta(x,alfa,beta)
p <-pbeta(0.6, shape1 = a, shape2 = beta) - pbeta(0.4, shape1 = a, shape2 = beta) 
print(paste0("La probabilidad de que sea P(0.4>X<0.6) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#12. Una empresa de producción de alimentos desea conocer la proporción de productos que cumplen 
#con las especificaciones de calidad establecidas. Se seleccionan aleatoriamente 500 unidades de 
#producto y se observa que 460 cumplen con las especificaciones. Suponiendo una distribución beta
#con parámetros a=6 y b=4, ¿cuál es la probabilidad de que la tasa de unidades que cumplen 
#con las especificaciones esté por encima del 90 %?
a = 6
beta = 4
#dbeta(x,alfa,beta)
p <-1 - pbeta(0.9, shape1 = a, shape2 = beta)
print(paste0("La probabilidad de que sea P(X>0.9) = ",p)) 
#<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>----------<<<>>>  
#1.2.6. Subsección 1.1.1: Distribución T-Student
#1. Un fabricante de guantes médicos afirma que el tiempo medio de duración de sus guantes es 
#de 3 horas, con una desviación estándar de 0.5 horas. Si tomamos una muestra aleatoria de 20 
#guantes, ¿cuál es la probabilidad de que la duración media de los guantes sea mayor de 3.2 horas?
ds = 0.5
rango = 3.2
p <- 1 - pt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X>1) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#2. Se sabe que el peso de los paquetes de cierta marca sigue una distribución normal con media
#de 1.5 kg y desviación estándar de 0.3 kg. Si tomamos una muestra aleatoria de 10 paquetes, 
#¿cuál es la probabilidad de que el peso promedio de la muestra sea menor de 1.3 kg?
ds = 0.5
rango = 1.3
p <- pt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X<1.3) = ",p)) 
####---------------------------------------------####-------------------------------------------#####      
#3. Se desea estudiar la altura promedio de los estudiantes de una universidad. Se toma una 
#muestra aleatoria de 15 estudiantes y se obtiene una altura promedio de 1.75 metros, con una 
#desviación estándar de 0.15 metros. Si la altura promedio de la población es de 1.8 metros,
#¿es significativo el resultado obtenido?
ds = 0.15
rango = 1.8
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=1.8) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#4. Se sabe que el tiempo de vida de las baterías de cierto modelo sigue una distribución normal con media 
#de 1000 horas y desviación estándar de 100 horas. Si tomamos una muestra aleatoria de 25 baterías,
#¿cuál es la probabilidad de que la duración promedio de la muestra sea mayor de 1050 horas?
ds = 100
rango = 1050
p <-1 -  dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X>1050) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#5. Se desea estudiar la duración promedio de las películas de acción en cierto cine. Se toma 
#una muestra aleatoria de 12 películas y se obtiene una duración promedio de 2.1 horas, con una
#desviación estándar de 0.25 horas. Si la duración promedio de las películas de acción es de 2 
#horas, ¿es significativo el resultado obtenido?
ds = 0.25
rango = 2
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=2) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#6. Se sabe que el diámetro de ciertos tornillos sigue una distribución normal con media de 10 mm
#y desviación estándar de 0.5 mm. Si tomamos una muestra aleatoria de 15 tornillos, ¿cuál es la 
#probabilidad de que el diámetro promedio de la muestra esté entre 9.7 mm y 10.3 mm?
ds = 0.5
p <- pt(10.3, df = ds) - pt(9.7, df = ds)
print(paste0("La probabilidad de que sea P(9.7>X<10.3) = ",p))     
####---------------------------------------------####-------------------------------------------#####      
#7. Se desea estudiar la cantidad promedio de libros que los estudiantes universitarios compran
#por semestre. Se toma una muestra aleatoria de 20 estudiantes y se obtiene una cantidad promedio 
#de 4 libros, con una desviación estándar de 1.5 libros. Si la cantidad promedio de libros que los
#estudiantes compran por semestre es de 3.5, ¿es significativo el resultado obtenido?
ds = 1.5
rango = 3.5
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=3.5) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#8. Se sabe que la altura de ciertas plantas sigue una distribución normal con media de 1.2 
#metros y desviación estándar de 0.2 metros. Si tomamos una muestra aleatoria de 30 plantas, 
#¿cuál es la probabilidad de que la altura promedio de la muestra sea mayor de 1.3 metros?
ds = 0.2
rango = 1.3
p <- 1 - pt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X>1.3) = ",p))     
####---------------------------------------------####-------------------------------------------#####      
#9. En una muestra de 25 estudiantes, se obtuvo una media de 7.5 en un examen de matemáticas con
#una desviación estándar de 1.2. ¿Cuál es el intervalo de confianza del 95 % para la media 
#poblacional de calificaciones?
ds = 1.2
rango = 0.96
p <- pt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X>1.3) = ",p))    
####---------------------------------------------####-------------------------------------------#####      
#10. Se sabe que el 90 % de las mujeres adultas tienen una altura entre 1.5 y 1.8 metros. Si se 
#selecciona una muestra aleatoria de 50 mujeres, ¿cuál es la probabilidad de que su altura
#promedio esté dentro de ese rango?
ds = 0.9

p <- pt(1.8, df = ds) -  pt(1.5, df = ds)
print(paste0("La probabilidad de que sea P(1.5>X<1.8) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#11. Se selecciona una muestra aleatoria de 30 observaciones de una población normalmente 
#distribuida. La media muestral es de 50 y la desviación estándar es de 8. ¿Cuál es la 
#probabilidad de que la media poblacional esté entre 46 y 54?
ds = 8
p <- pt(54, df = ds) - pt(46, df = ds)
print(paste0("La probabilidad de que sea P(46>X<54) = ",p))   
####---------------------------------------------####-------------------------------------------#####      
#12. En un estudio de mercado se entrevistó a una muestra aleatoria de 100 consumidores.
#Se encontró que la media de gasto semanal en alimentos es de $80 con una desviación estándar de
#$15. ¿Cuál es el intervalo de confianza del 99 % para la media poblacional de gasto semanal en
#alimentos?
ds = 15
rango = 0.99
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=0.99) = ",p))     
####---------------------------------------------####-------------------------------------------#####      
#13. Se desea estimar la media poblacional de la duración de una batería en horas. Se selecciona 
#una muestra aleatoria de 20 baterías y se obtiene una media de 30 horas con una desviación
#estándar de 5 horas. Si se sabe que la distribución de la duración de las baterías es normal, 
#¿cuál es el intervalo de confianza del 90 % para la media poblacional de duración?
ds = 5
rango = 0.90
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=0.90) = ",p))     
####---------------------------------------------####-------------------------------------------#####      
#14. Un fabricante de piezas de automóviles afirma que la vida media de sus piezas es de 50,000 km.
#Para comprobarlo, se selecciona una muestra aleatoria de 25 piezas y se encuentra que la vida 
#media es de 48,000 km con una desviación estándar de 6,000 km. Si se asume que la distribución de
#la vida de las piezas sigue una distribución normal, ¿hay suficiente evidencia para rechazar la
#afirmación del fabricante al nivel de significancia del 5 %?
ds = 6000
rango = 0.05
p <- dt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=0.05) = ",p))     
####---------------------------------------------####-------------------------------------------#####      
#15.Se desea comprobar si la media poblacional de la edad de los clientes de una tienda de ropa es
#mayor a 30 años. Se selecciona una muestra aleatoria de 40 clientes y se obtiene una media de 32 
#años y una desviación estándar de 4 años. Si se utiliza un nivel de significancia del 5 %, 
#¿se puede afirmar que la media poblacional es mayor a 30 años?
ds = 4
rango = 0.05
p <- pt(rango, df = ds)
print(paste0("La probabilidad de que sea P(X=0.05) = ",p))     



  