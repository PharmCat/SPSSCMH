# Copyright © 2018 PharmCat

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#	http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.icense for the specific language governing permissions and 
# limitations under the License.

#Version = 0.1.0 Beta

Run <- function(args) {
    #Execute the POWERTOST command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
		spsspkg.Template("GF", subc="", var="gf", ktype="varname"),
		spsspkg.Template("GROUP", subc="", var="group", ktype="varname"),
        spsspkg.Template("EFFECT", subc="", var="effect", ktype="varname"),
		spsspkg.Template("ALPHA", subc="", var="alpha", ktype="float", vallist=list(0.0, 1.0))
        ))

    res <- spsspkg.processcmd(oobj, args, "exec")
    
}


exec<-function(gf, group, effect, alpha=0.05){

	library(metafor)
	spsspkg.StartProcedure("CMH")
	
	dataset <- data.frame(spssdata.GetDataFromSPSS(variables=c(gf, group, effect)))
	colnames(dataset)[1] <- "gf"
	colnames(dataset)[2] <- "group"
	colnames(dataset)[3] <- "effect"

	labelsgf <- data.frame(spssdictionary.GetValueLabels(gf))
	labelsgr <- data.frame(spssdictionary.GetValueLabels(group))
	labelsef <- data.frame(spssdictionary.GetValueLabels(effect))
	
	dataset <- dataset[with(dataset, order(gf, group, effect)),]
	
#	print (dataset)
	gfl <- dataset[, "gf"]
 	ugfl <- (unique(gfl))

	
	
#	print (dataset)

#	print(dataset)
	
	
	
	rdata <- data.frame()
	for (f in ugfl) {
		newdata <- subset(dataset, gf == f, select=c(group, effect)) 
		tbl <- table(newdata)
		mtt   <- as.data.frame.matrix(tbl)
		mtt <- mtt[,c(2,1)]
		if (dim(mtt)[1] == 2 && dim(mtt)[2] == 2){
			lab1 <- subset(labelsgf, values==f)[1, "labels"]
			raw   <- data.frame (gf=lab1, tpos=mtt[1,1], tneg=mtt[1,2], cpos= mtt[2,1], cneg = mtt[2,2])
			rdata <- rbind(rdata, raw)
#			print (tbl)
		} else {
			print ("Error: this is not 2X2 table")
			return ()
		}
	}
	colnames(rdata)[1] <- "gf"
	colnames(rdata)[2] <- "tpos"
	colnames(rdata)[3] <- "tneg"
	colnames(rdata)[4] <- "cpos"
	colnames(rdata)[5] <- "cneg"
#	print (rdata)
	
	
	

	mh <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=rdata, measure="RR", digits=4, level=alpha, slab=paste(gf))
	
	spssRGraphics.SetOutput("ON")
	spssRGraphics.SetGraphicsLabel("Forest Plot")
	
#par(mar=c(4,4,1,2))	
	po <- forest(mh, atransf=exp, digits=c(4,2), cex=1)
	text (x= -0.5, y = 3.5, spssdictionary.GetVariableLabel(gf))
    text (x=  0.5, y = 3.5, "Observed RR [% CI]")
	
	print (mh)


}