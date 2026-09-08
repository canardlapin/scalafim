x=as.matrix(read.csv('x.csv',header=FALSE));y=as.matrix(read.csv('y.csv',header=FALSE))
z=x;z[,3]=z[,3]*1e-6
a=lm.fit(x,y);b=lm.fit(z,y);b$coefficients[3,]=b$coefficients[3,]*1e-6
cat('rank',b$rank,'coefficient max difference',max(abs(a$coefficients-b$coefficients)),'\n')
