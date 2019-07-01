function y=senshenon(a,b,x01,x02,y01,y02,n)

p=henonf(a,b,x01,y01,n);    %matriz con resultados de la ecuacion del sist dinamico discreto para x01
q=henonf(a,b,x02,y02,n);    %matriz con resultados de la ecuacion del sist dinamico discreto para x02

c=n-1;
d=0:1:c;
k=[d]';
m = [k p q];


subplot(326);plot(k,p,'r');hold on;plot(k,q,'b');title('SENSIBILIDAD A CONDICIONES INICIALES');
xlabel('k/iteraciones');ylabel('x/logistica');hold off

subplot(322);plot(k,p,'r');title('SENSIBILIDAD A CONDICIONES INICIALES CASO A');xlabel('k/iteraciones');ylabel('x/logistica');
subplot(324);plot(k,q,'b');title('SENSIBILIDAD A CONDICIONES INICIALES CASO B');xlabel('k/iteraciones');ylabel('x/logistica');

	for i=1:n
% sacamos el valor en el que se ve claramente la sensibilidad a condiciones iniciales
             if abs(p(i)-q(i))>0.01;
		s=i-1;
		disp('La sensibilidad a condiciones iniciales aparece en la iteración'), disp(s)
	     break
% cortamos el bucle
	end

end
