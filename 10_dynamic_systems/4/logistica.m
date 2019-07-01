function x=logistica(mu,x0,n)

x=zeros(n,1);
x(1)=x0;

  for k=1:n-1
    x(k+1)=mu*x(k)*(1-x(k));
  end

end


