function x=henonaprox %(a,b,x0,y0,n)
a=1.4;
b=0.3;
x0=0;
y0=0;
n=500000;



x=zeros(n,1);
y=zeros(n,1);

x(1)=x0;
y(1)=y0;

	for k=1:n-1

	x(k+1) = 1 + y(k) - a*x(k)*x(k);
	y(k+1) = b*x(k);

	end
%subplot(321)
plot(x,y,'.');
axis([0.95 1.25 0.05 0.13]);

end
