#Test DiDiSTATIS. 

#Run up through parts of EigenDiDiSTATIS, and start testing things...

#Are the grand barycentric factor scores mutually orthogonal?
t(res_Grand$Disc$Fdisc) %*% res_Grand$Disc$Fdisc
#no?

#oh, right. The eigen-vector are orthogonal, not the factor scores...
t(res_Grand$eigPbCPPb$Ub) %*% res_Grand$eigPbCPPb$Ub


round(t(res_Grand$eig$U) %*% res_Grand$eig$U, 3)







t(res_Grand$Disc$Fb.d[,,1]) %*% res_Grand$Disc$Fb.d[,,1]

res_Grand$Disc$Fb.d
