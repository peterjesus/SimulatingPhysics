
function x = tienda(r,x0,n)

x = zeros(n,1);
x(1) = x0;


	for k = 1:n-1

		if ( x(k) > 0 && x(k) < 1/2 )		

	 		x(k+1) = 2*r*x(k);
		
		elseif ( x(k) >= 1/2 && x(k) < 1 )

			x(k+1) = 2*r*(1-x(k));
		
		endif

	endfor
end
