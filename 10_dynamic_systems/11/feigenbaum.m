%function x=henonf(a,b,x0,y0,n)

%x=zeros(n,1);
%y=zeros(n,1);

%x(1)=x0;
%y(1)=y0;

%	for k=1:n-1

%	x(k+1) = 1 + y(k) - a*x(k)*x(k);
%	y(k+1) = b*x(k);

%	end

%plot(x,y,'*');
%axis [-1.5 1.5 -0.4 0.4];

%end
%  v) Diagrama Feingenbaum

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

% representamos las ultimas 100 iteraciones
end
% Realizamos este proceso para todo mu

plot(a,z1,'b.');
title('Diagrama de Feigenbaum');
xlabel('a');
ylabel('x,puntos ciclicos');
