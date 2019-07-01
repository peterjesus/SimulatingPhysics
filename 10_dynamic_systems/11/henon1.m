function x=henon1(a,b,x0,y0,n)

x=zeros(n,1);
y=zeros(n,1);

x(1)=x0;
y(1)=y0;

	for k=1:n-1

	x(k+1) = 1 + y(k) - a*x(k)*x(k);
	y(k+1) = b*x(k);

	end
subplot(321)
plot(x,y,'.');
%axis [-1.5 1.5 -0.4 0.4];
title('CASO PARTICULAR')
xlabel('-1.5<x<1.5')
ylabel('-0.4<y<0.4')
end
