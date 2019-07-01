% Realizar un programa numerico que dibuje las orbitas de la ecuacion de Henon.

%   i) Como caso particular estudiar el caso a=1.4, b=0.3 para puntos de la region -1.5<x<1.5 y -0.4<y<0.4 
%  ii) Computar varias iteraciones de la orbita con valor inicial (0,0) para ir siguiendo su evolucion en el plano 
% iii) Estudiar la sensibilidad a las condiciones iniciales
%  iv) Hacer diversos zooms en distintas zonas del atractor y discutir el resultado
%   v) Representar el diagrama de Feigenbaum de x vs a para b=0.3 y discutir el resultado





% i) Caso particular

a=1.4;
b=0.3;
x0=rand(1)*1.5;
y0=rand(1)*0.4;
%n=input('Introducir el numero de iteraciones deseado=');
n=1000; 

% Aplicamos la funcion definida anteriormente con el sistema dinamico conocido

henon1(a,b,x0,y0,n);



%axis [(-1.5 1.5 -0.4 0.4)];

% ii) Evolucion iteraciones en el plano con (x0,y0)=(0,0)

a=1.4;
b=0.3;
x0=0;
y0=0;
%n=input('Introducir el numero de iteraciones deseado=');
n=1000; 

% Aplicamos la funcion definida anteriormente con el sistema dinamico conocido

henon2(a,b,x0,y0,n);



% iii) Sensibilidad a condiciones iniciales

%a=input('Introducir el valor de la constante a='); 
a=1.4 
%b=input('Introducir el valor de la constante b=');
b=0.3
%n=input('Introducir el numero de iteraciones deseado='); 
n=50; 
%x01=input('Introducir un valor inicial de x para el caso A=') 
x01=1.00000
%x02=input('Introducir un valor inicial de x para el caso B=') 
x02=1.00001
%y01=input('Introducir un valor inicial de y para el caso A=')
y01=1.60000
%y02=input('Introducir un valor inicial de y para el caso B=') 
y02=1.60001

% Aplicamos la funcion definida anteriormente

senshenon(a,b,x01,x02,y01,y02,n)

%  iv) Zooms

%Ejecutar 
'Al hacer zooms vemos que las lineas se van dividiendo en 2 siguiendo el principìo de cantor y por tanto el atractor de Henon es en esencia un fractal. Se produce el fenómeno de duplicación del periodo antes de llegar al caos, con lo que se deja ver la naturaleza cantoriana de este atractor, a su vez típica del caos (Ejecutar henonaprox;)'

%  v) Feigenbaum

n = 1000;
m = 100;
x0 = rand(1)*1.5;
y0 = rand(1)*0.4;

a1 = 1;
a2 = 1.5;
a = linspace (a1,a2,n);
z1 = zeros(m,n);
b = 0.3;

for k = 1:n
% Creamos una ristra de 1000 valores correspondientes a cada iteracion
	x = henonf(a(k),b, x0,y0, n); 

% cogemos la columna k de ceros y los igualamos a los valores desde 901 a 1000 de la ec logistica
	z1(:,k) = x(n-m+1:n); 
end
% Realizamos este proceso para todo mu

subplot(325)
plot(a,z1,'b.');
title('DIAGRAMA DE FEIGENBAUM');
xlabel('a');
ylabel('x,puntos ciclicos');
axis([1 1.5 -1.5 1.5])


'Podemos destacar que el fenomeno de duplicacion del periodo aparece antes de llegar al caos'

