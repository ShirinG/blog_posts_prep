#https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf


library(neuralnet)
AND <- c(rep(0,7),1)
OR <- c(0,rep(1,7))
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
print(net <- neuralnet(AND+OR~Var1+Var2+Var3, binary.data, hidden=0,rep=10, err.fct="ce", linear.output=FALSE))
plot(net)

data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert,err.fct="ce", linear.output=FALSE, likelihood=TRUE))
plot(net.infert)

gwplot(net.infert, selected.covariate="parity")
confidence.interval(net.infert)
prediction(net.infert)


Var1 <- runif(50, 0, 100)
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
print(net.sqrt <- neuralnet(Sqrt~Var1,  sqrt.data, hidden=10,
                            threshold=0.01))
compute(net.sqrt, (1:10)^2)$net.result
plot(net.sqrt)
