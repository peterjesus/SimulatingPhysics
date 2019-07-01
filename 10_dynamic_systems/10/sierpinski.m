% Calcular la dimension fractal de la alfombra de Sierpinski.


n=input('introducir número de iteraciones=');
	
m=0;
	for k=1:n 
		m=[m,m,m;m,ones(3^(k-1)),m;m,m,m];
	
		subplot(1,n,k);
		imagesc(m);
		axis("square");
		colormap(gray);	
		
	end

m;  %podemos sacar la matriz m a pantalla

%Definimos el numero de cuadraditos de lado A
%Contamos los cuadraditos negros de nuestra alfombra, los cuales se corresponden con los 0 de la matriz m
	
	a=length(m);
	b=a.^2;
	q=0;

	for i=1:b
		if (m(i)==0)
		   q=[q 1];
		
		endif
	endfor

		N=sum(q);

%Definimos el valor de a0/a=P
%Hemos dividido nuestro cuadrado original de longitud a0 en un en cierto numero cuadraditos de longitud a, que se corresponden con el numero de elementos de una fila de nuestra matriz m

	P=length(m); %longitud a del elemento de la matriz = elementos cuadrado


n=log(N);
p=log(P);

%Vemos el valor de la dimension fractal:

dim=n./p;

disp('La dimension fractal de nuestra alfombra es:');
disp(dim)


