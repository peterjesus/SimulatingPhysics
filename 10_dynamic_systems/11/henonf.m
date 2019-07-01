function x=henonf(a,b,x0,y0,n)

x=zeros(n,1);
y=zeros(n,1);

x(1)=x0;
y(1)=y0;

	for k=1:n-1

	x(k+1) = 1 + y(k) - a*x(k)*x(k);
	y(k+1) = b*x(k);

	end

%plot(x,y,'*');
%axis [-1.5 1.5 -0.4 0.4];

end
