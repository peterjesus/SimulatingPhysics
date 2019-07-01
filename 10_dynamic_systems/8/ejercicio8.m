%Ejercicio 8
%Tienda de campaña
%f = 2rx 	0<x<0.5
%f = 2r(1-x)	0.5<=x<1

%f' = |2r|

%%% a) Exponente de Lyapunov

%disp('Para un r positivo')

n = 500;
x0 = 0.1;

ri=0; 
ro=1;

L=[];
j=1;
lambda = 0;

for  r = ri:0.001:ro

	for i = 1:n-1
	y1 = 2*r; %-2*r;

	y = 2*r*x0;  %2*r*(1-x0);
	lambda = lambda + log(abs(y1));
	
	x0 = y;

	endfor

lambda = lambda/n;
L(j) = lambda;
j=j+1;
lambda = 0;
endfor


subplot(211)
r = ri:0.001:ro;
%valor del parametro r frente exponente
plot(r,L);
hold on
plot([0.5;0.5],[-8,2],'r');
title('EXPONENTE DE LYAPUNOV');
xlabel('r');
ylabel('lambda');
grid;
hold off


'Tenemos que la derivada de la funcion f(x) respecto a x es 2r para 0<x<0.5 y -2r para 0.5<x<1. Luego para analizar lambda (de cuyo valor depende si el sistema es sensible o no a condiciones iniciales) metemos el sumatorio del valor absoluto de ambas derivadas que es 2r n veces y por tanto al multiplicarlo por 1/n queda que lambda es log|2r|. Lambda será positiva para valores mayores de 0.5 de r y negativa para valores de r menores de 0.5. Por tanto para valores mayores de 0.5 el sistema sera sensible a condiciones iniciales y por tanto caótico.'


%%% b) Diagrama de Feigenbaum

n = 1000;
m = 100;
x0 = rand(1);
% Caso 1

r1 = 0.001;
r2 = 0.999;

r = linspace (r1,r2,n);
z1 = zeros(m,n);

for k = 1:n
% Creamos una ristra de 1000 valores correspondientes a cada iteracion
	x = tienda(r(k), x0, n); 

% cogemos la columna k de ceros y los igualamos a los valores desde 901 a 1000 de la ec logistica
	z1(:,k) = x(n-m+1:n); 

% representamos las ultimas 100 iteraciones
end
% Realizamos este proceso para todo r
subplot(212)
plot(r,z1,'b.');
title('DIAGRAMA DE FEIGENBAUM');
xlabel('r');
ylabel('x,puntos ciclicos');


