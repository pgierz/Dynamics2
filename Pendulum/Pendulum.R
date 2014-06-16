# User defined Variables
lat = 49
tday = 86400 # Length of one Day in seconds
tmax=200 # Time of simulation in seconds

# Initial Conditions
g = 9.81 # m/s**2, acceleration due to gravity
L = 67 # length of pendulum string
initial_x = L/100 # initial x coordinate
initial_y = 0 # initial y
initial_u = 0 # initial u
initial_v = 0 # initial v

# Definitions based on User Variables
Omega = 2*pi/tday
phi = lat/180 * pi




# set up vectors for x, x_d, x_dd, and y, y_d, y_dd
x = vector() # x+x_d*t
x_d = vector() # x_d + x_dd*t
x_dd = vector() # 2*Omega*phi*y_d-(g/L)*x
y = vector() # y+y_d*t
y_d = vector() # y_d + y_dd*t
y_dd = vector() # -2*Omega*phi*x_d-(g/L)*y

a_x <- function(yd, r){
ax = 2*Omega*phi*yd-(g/L)*r
return(ax)
}

a_y <- function(xd, r){
ay = -2*Omega*phi*xd-(g/L)*r
return(ay)
}

# Initialize vectors
x[1] = initial_x
y[1] = initial_y
x_d[1] = initial_u
y_d[1] = initial_v
x_dd[1] = a_x(y_d[1], x[1])
y_dd[1] = a_y(x_d[1], y[1])
#plot(x[1], y[1], xlim=c(-1,1), ylim=c(-1,1))

# loop over everything and plot
for (i in 2:tmax) {
    x_dd[i] =  a_x(y_d[i-1], x[i-1])
    y_dd[i] = a_y(x_d[i-1], y[i-1])
    x_d[i] = x_d[i-1] + x_dd[i]
    y_d[i] = y_d[i-1] + y_dd[i]
    x[i] = x[i-1] + x_d[i]
    y[i] = y[i-1] + y_d[i]
#    points(x[i], y[i])
}

plot(x, y)