mydata = read.table("/mnt/data/user_data/00/000/001/ideas/2017/01/27/c89a432816385e51499f777ee57fc353/process/sphinx/sphinx-summary.txt",header=T,quote="",sep = "^");

mydata$norm = scale(mydata$score);

myunique = unique(mydata$docid);
mylen = length(myunique);

multiplier = 1;
	
newdata = data.frame();
for(i in 1:mylen)
	{
	doc = myunique[i];
	mysub = subset(mydata,docid==doc);
		mydim = dim(mysub);
		mycount = mydim[1];
	myscore = mynorm = 0;
	for(j in 1:mycount)
		{
		myS = mysub[j,];
		myMult = 1;
		if(myS$method=="strict") {myMult = multiplier;}
		myMult2 = 1;
		if(as.character(myS$and)=="and") {myMult2 = 2;}
			myscore = myscore + myS$score * myMult * myMult2 * myS$n;  # multiple by 5 for 5-gram
			#mynorm = mynorm + myS$norm * myMult;
		}
		#myscore = sum(mysub$score);
		#mynorm = sum(mysub$norm);
	#newdata = rbind(newdata,  c(doc,mycount,myscore,mynorm) );
	newdata = rbind(newdata,  c(doc,mycount,myscore) );
	}

#colnames(newdata) = c("doc","count","score","norm");
colnames(newdata) = c("doc","count","score");
newdata$norm = scale(newdata$score);
#rownames(newdata) = myunique;

newdata = newdata[order(-newdata$count,-newdata$score, -newdata$norm),];

newdata$norm2 = scale(newdata$score);
newdata$weight = 2 * newdata$count * newdata$norm2; # negative numbers are getting killed here!

newdata$count = 2 * newdata$count; 

mymin = abs( min(newdata$weight) ) + 1;

newdata$final = mymin + newdata$weight;

mymax = max(newdata$final);

newdata$relative = newdata$final / mymax;

newdata = newdata[order(-newdata$relative,-newdata$count),];

write.table(newdata,file="/mnt/data/user_data/00/000/001/ideas/2017/01/27/c89a432816385e51499f777ee57fc353/process/sphinx/R-sphinx-summary.txt",col.names = T,row.names=FALSE,sep="^",quote=F);


