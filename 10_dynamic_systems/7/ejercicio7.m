%Ejercicio7
%f = mu.*x.*(1-x);
%f'= mu*(1-2*x);
%Realizar un programa numérico que dibuje el Exponente de Lyapunov para la ecuación logística en función de mu.

n = 500;
x0 = rand(1);


mui=input('Introducir mu inicial='); %3.4
muo=input('Introducir mu final='); %4

L=[];
j=1;
lambda = 0;

for mu = mui:0.001:muo
	for i = 1:n-1
	y1 = mu*(1-2*x0); 
%Derivamos
	y = mu*x0*(1-x0);
	lambda = lambda + log(abs(y1)); 
%función del exponente de Lyapunov
	x0 = y;
	endfor

%Dividimos entre un n suficientemente grande
lambda = lambda/n;
%Creamos una matriz L con lambda para cada mu
L(j) = lambda;
j = j+1;
lambda = 0;

endfor

mu = mui:0.001:muo;
plot(mu,L);
title('EXPONENTE DE LYAPUNOV');
xlabel('mu');
ylabel('lambda');
grid;

