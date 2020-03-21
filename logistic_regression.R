mat.new[1:10,1:5]
head(class)
fulldx[which(fulldx$class=="unaffected"),3] = 0 
fulldx[which(fulldx$class=="ASD"),3] = 1 
head(fulldx)
#mat.h1.class = as.data.frame(cbind(mat.new$Histamine.H1.receptor,mat.new$Histamine.H2.receptor, mat.new$Histamine.H3.receptor, mat.new$Histamine.H4.receptor, mat.new$Glutamate.receptor.ionotropic..NMDA.3A,  fulldx$V3))
#colnames(mat.h1.class) =c("H1_Receptor", "H2_Receptor", "H3_Receptor", "H4_Receptor","Glutamate_Receptor","Class")

mat.h1.class = as.data.frame(cbind(mat.new$Histamine.H1.receptor,fulldx$V3))
colnames(mat.h1.class) =c("H1_Receptor","Class")
mat.h1.class$Class = as.numeric(mat.h1.class$Class)
model = glm(Class ~. , data = mat.h1.class)
summary(model)
new.dat = data.frame(H1_Receptor=seq(0, 1,len = 1000))
new.dat$asd = predict(model, newdata = new.dat, type = "response")
plot(asd ~ H1_Receptor, new.dat, lwd = 2 )
