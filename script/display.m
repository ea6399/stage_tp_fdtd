%% Affichage de la Gaussienne
% Parametre 
Nx = 499;
Nx = Nx + 1;
Nt = 1500;
snapshot = 5;
n_block = Nt / snapshot;       
disp(n_block);

M = load('/home/emin/Documents/TP_FDTD/1D/stage_tp_fdtd/E.txt');
disp(size(M));

%% RESHAPE 
M_resh = reshape(M,[Nx,n_block + 1]);
disp("Reshape M");
disp(size(M_resh));


% On charge les données :
n1 = 5;
n2 = 50;
n3 = 150;
x = M_resh(:,1);             % Intervalle spatial
y = M_resh(:,n1);             % Observateur 1 | 2 | 3
y1 = M_resh(:,n2);           
y2 = M_resh(:,n3);


% On charge les données :
n1 = 10;                 % Temps n - 1 
n2 = 50;
n3 = 150;
x = M_resh(:,1);             % Intervalle spatial
y = M_resh(:,n1);             % Observateur 1 | 2 | 3
y1 = M_resh(:,n2);           
y2 = M_resh(:,n3);

figure(1);
fig = gcf;
fig.Position = [300,80,1200,800];

subplot(3,1,1)
plot(x,y);
title(['Temps : ', num2str(0)]);
xlabel('x');
ylabel('f(x)');
grid on;

subplot(3,1,2)
plot(x,y1);
title(['Temps :', num2str(n2)])
xlabel('x');
ylabel('f(x)');
grid on;

subplot(3,1,3)
plot(x,y2)
title(['Temps : ', num2str(n3)])
xlabel('x');
ylabel('f(x)');
grid on;

sgtitle('FDTD');