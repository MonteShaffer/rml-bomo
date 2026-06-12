mydata = read.table("/mnt/data/user_data/00/000/001/ideas/2017/01/27/c89a432816385e51499f777ee57fc353/process/sphinx/sphinx-timings.txt",header=T,quote="",sep = "^");

mydata$status.queries

# sum(mydata$status.queries);
# sum(mydata$status.query_cpu);  # CPU 1
# sum(mydata$meta.cpu_time);	 # CPU 2

# sum(mydata$status.query_reads);  # READ 1
# sum(mydata$status.query_readtime );	 # READ 2
# sum(mydata$meta.io_read_time);  # READ 3
# sum(mydata$meta.io_read_ops);	 # READ 4

newdata = data.frame();
	newdata = rbind(newdata,  c(sum(mydata$status.queries),  sum(mydata$status.query_cpu),  sum(mydata$meta.cpu_time),  sum(mydata$status.query_reads),  sum(mydata$status.query_readtime),  sum(mydata$meta.io_read_time),  sum(mydata$meta.io_read_ops)      ) );

colnames(newdata) = c("queries","CPU1","CPU2","READ1","READ2","READ3","READ4");

write.table(newdata,file="/mnt/data/user_data/00/000/001/ideas/2017/01/27/c89a432816385e51499f777ee57fc353/process/sphinx/R-sphinx-timings.txt",col.names = T,row.names=FALSE,sep="^",quote=F);


