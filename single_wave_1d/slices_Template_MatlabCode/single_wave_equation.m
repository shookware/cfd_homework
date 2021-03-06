function main
clc;
clear;

N=100; %total cells, points N+1
L=2.0; %length
dx=L/N; % compute the spatial step
x=(0:N)'/N*L; % x positions

%initialize
u=zeros(N+1,1);
xl=0.4;  %L_1
for i=1:N+1
    if x(i)<=xl 
        u(i)=1.0;  %if L(20)=L_1
    end
end

% set the phase velocity
c=0.5;
% set the courant number
cfl=0.7;

un=u;
t=0.0; %initial time
time=2; %total time

while (t<time)
    rhs=spatial_difference(N, un, dx, c);
    dt=cfl*dx/c; %compute dt time step
    un=temporal_advance(un, dt, rhs);
    t=t+dt
end

%exact solution
xl=0.4+c*time;  %L_1
for i=1:N+1
    if x(i)<=xl 
        u(i)=1.0;  %if L(20)=L_1
    end
end

plot(x, un, x, u); %plot the result
end

function rhs=spatial_difference(N, un, dx, c)
    rhs=zeros(N+1,1);
    
    for i=2:N+1
        rhs(i)=-c*(un(i)-un(i-1))/dx;
    end
end

function unp=temporal_advance(un, dt, rhs)
    unp=un+dt*rhs;
end

