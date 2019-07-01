function x = exponencial(a,x0,n)
	
	x = zeros(n,1);	
	x(1) = x0;
	alfa = 4.9;

	for k = 1:n-1
		x(k+1) = exp(- alfa * x(k).^2) + a;

	end

endfunction
