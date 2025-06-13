%% Affichage de la Gaussienne
% Parametre 
Nx = 500;
Nx = Nx + 1;
Nt = 1500;
snapshot = 5;
n_block = Nt / snapshot;       
disp(n_block);

M = load('E_t.txt');
disp(size(M));


% On charge les données :
n1 = 2;                 % Temps
n2 = 5;
n3 = 10;
x = M(:,1);             % Intervalle spatial
y = M(:,n1);             % Observateur 1 | 2 | 3
y1 = M(:,n2);           
y2 = M(:,n3);

figure(1);
fig = gcf;
fig.Position = [300,80,1200,800];
subplot(3,1,1)
plot(x,y);
title(['Temps : ', num2str(0)]);
subplot(3,1,2)
plot(x,y1);
title(['Temps :', num2str(n2)])
subplot(3,1,3)
plot(x,y2)
title(['Temps : ', num2str(n3)])
xlabel('x');
ylabel('f(x)');
grid on;
