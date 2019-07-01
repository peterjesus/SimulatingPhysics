function y=sensibilidadexp(x01,x02,a,n)

p=exponencial(a,x01,n);    %matriz con resultados de la ecuacion del sist dinamico discreto para x01
q=exponencial(a,x02,n);    %matriz con resultados de la ecuacion del sist dinamico discreto para x01
c=n-1;
d=0:1:c;
k=[d]';                    %matriz columna de valores del 1 a n (indica  en queiteracion estamos)
format long
%matriz valores del sistema dinamico discreto para x01 y x02 en cada iteracion
m = [k p q]


subplot(311);plot(k,p,'r');hold on;plot(k,q,'b');title('SENSIBILIDAD A CONDICIONES INICIALES');
xlabel('k/iteraciones');ylabel('x/logistica');hold off

subplot(312);plot(k,p,'r');title('SENSIBILIDAD A CONDICIONES INICIALES CASO A');xlabel('k/iteraciones');ylabel('x/logistica');

subplot(313);plot(k,q,'b');title('SENSIBILIDAD A CONDICIONES INICIALES CASO B');xlabel('k/iteraciones');ylabel('x/logistica');

for i=1:n
% sacamos el valor en el que se ve claramente la sensibilidad a condiciones iniciales
	if abs(p(i)-q(i))>0.01;
	s=i-1;
	disp('La sensibilidad a condiciones iniciales aparece en la iteracion'), disp(s)
% cortamos el bucle
	break

	end

end
