

new.will = will$movies.50
new.will$nmid = will$nmid;
new.will$name = will$name;
new.will$countfilms = will$countfilms$totalcount;

new.will = new.will[, c(12,13,14, 1:11)];

new.denzel = denzel$movies.50
new.denzel$nmid = denzel$nmid;
new.denzel$name = denzel$name;
new.denzel$countfilms = denzel$countfilms$totalcount;

new.denzel= new.denzel[, c(12,13,14, 1:11)];

df.will.denzel = rbind[c(new.will, new.denzel)];

write.table(df.will.denzel, file = "WEEK-03/output/df-will-vs-denzel.txt", sep="|", col.names=T, row.names=F);

df.will.denzel2 = read.csv("WEEK-03/output/df-will-vs-denzel.txt", sep="|", header=T);

# if (cache) ... df ...
# else ... above 
# ... will = grabFilmsForPerson(nmid);
# if (personcache) ...
# my.source = 'local'; ... grab and store that file...
