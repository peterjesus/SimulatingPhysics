%Ejercicio6

%iv)
%Sea F_a(x)= exp(-alpha*x^2)+a
% a) Dibujar e diagrama de Feingenbaum
% b) Localizar (si los hay) algun valor de a para los que hay ciclos atractivos de orden 8 y dibujar las orbitas para algun valor inicial concreto
% c) Calcular el numero de Feingenbaum
% d) Estudiar la sensibilidad a condiciones iniciales




n = 1000;
m = 100;
x0 = 0.1;


%a) Diagrama de Feigenbaum

a_min = -1;
a_max = 1;
a1 = linspace (a_min,a_max,n);
z = zeros(m,n);

for k = 1:n
	x = exponencial(a1(k), x0, n);
	z(:,k) = x(n-m+1:n);
end

plot(a1,z,'b.');title('DIAGRAMA DE FEIGENBAUM');xlabel('a');ylabel('x,puntos ciclicos');

%b) Ciclos Atractivos de Orden 8

'Haciendo zooms, obtenemos 2 ciclos atractivos de orden 8 para los valores a=-0.6 y a=-0.36 '

%c) Numero de Feigenbaum

'Haciendo zooms sobre el diagrama de Feigenbaum, entre el orden 4 y 8 obtenemos un numero de feigenbaum de delta=4.4655'

%d) Sensibilidad condiciones iniciales
%entre -1 y 1; sino converge muy rapido a a
a=input('Introducir un valor para la constante a=');
%0.5
m=input('Introducir el numero de iteraciones deseado=');
%50
xa=input('Introducir un valor inicial de x para el caso A=');
%0.31000
xb=input('Introducir un valor inicial de x para el caso B=');
%0.310001

sensibilidadexp(xa,xb,a,m)



