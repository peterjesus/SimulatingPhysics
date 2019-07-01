% Ejercicio 4


% Representar el diagrama de Feigenbaum para los intervalos intervalos mu=[1,4], mu=[3,3.678], mu=[3.45122,3.59383], y mu=[3.54416,3.57490], escogiendo el rango de ordenadas de forma adecuada para que las figuras sean semejantes. ¿A qie valor tiende el cociente de la separacion entre sucesivas bifurcaciones?

n = 1000;
m = 100;
x0 = rand(1);

%%% Caso 1

mu_min1 = 1;
mu_max1 = 4;

mu1 = linspace (mu_min1,mu_max1,n);
z1 = zeros(m,n);

for k = 1:n
% Creamos una ristra de 1000 valores correspondientes a cada iteracion
	x = logistica(mu1(k), x0, n); 

% cogemos la columna numero k de ceros y los igualamos a los valores desde 901 a 1000 de la ec logistica
	z1(:,k) = x(n-m+1:n); 

% representamos las ultimas 100 iteraciones
end

% Realizamos este proceso para todo mu

%%% Caso 2

mu_min2 = 3;
mu_max2 = 3.678;
mu2 = linspace (mu_min2,mu_max2,n);
z2 = zeros(m,n);
for k = 1:n
	x = logistica(mu2(k), x0, n);
	z2(:,k) = x(n-m+1:n);
end


%%% Caso 3

mu_min3 = 3.45122;
mu_max3 = 3.59383;
mu3 = linspace (mu_min3,mu_max3,n);
z3 = zeros(m,n);
for k = 1:n
	x = logistica(mu3(k), x0, n);
	z3(:,k) = x(n-m+1:n);
end

%%% Caso 4

mu_min4 = 3.54416;
mu_max4 = 3.57490;
mu4 = linspace (mu_min4,mu_max4, n);
z4 = zeros(m,n);
for k = 1:n
	x = logistica(mu4(k), x0, n);
	z4(:,k) = x(n-m+1:n);
end

% Diagramas de Feigenbaum

subplot(221);plot(mu1,z1,'b.');title('mu=1-4');xlabel('mu');ylabel('x,puntos ciclicos');

subplot(222);plot(mu2,z2,'b.');title('mu=3-3.678');xlabel('mu');ylabel('x,puntos ciclicos');

subplot(223);plot(mu3,z3,'b.');title('mu=3.45122-3.59383');axis([3.45122 3.59383 0.3 0.6 ]);xlabel('mu');ylabel('x,puntos ciclicos');

subplot(224);plot(mu4,z4,'b.');title('mu=3.54416-3.57490');axis([3.54416 3.57490 0.45 0.58 ]);xlabel('mu');ylabel('x,puntos ciclicos');


'En la cuarta grafica el valor del cociente entre sucesivas bifurcaciones es (3,5688−3,5645)/(3,5697−3,5688)=4.778 , es decir, muy proximo al valor de la constante del caos c=4.6692016...'





