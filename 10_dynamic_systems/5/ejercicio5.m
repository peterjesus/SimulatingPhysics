% Ejercicio 5

% Estudiar la sensibilidad a las condiciones iniciales para la curva logistica con mu=3.839. ¿Hay caos en este caso?

% Aplicamos las funciones logistica y sensibilidad, creadas anteriormente; e introducimos los valores necesarios

%0.3000000 
x01=input('Introducir un valor inicial de x para el caso A=');

%0.3000001
x02=input('Introducir un valor inicial de x para el caso B=');

mu=input('Introducir el parametro de control=');
%3.839; %con 4 se aprecia la sensib

n= input('Introducir el numero de iteraciones deseado=');
%50

sensibilidad(x01,x02,mu,n);
