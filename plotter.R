#!/usr/bin/Rscript 
#require(scales)
require(ggplot2)
require(jsonlite)
require(hash)
require(extrafont)
require(fontcm)
require(memisc)
require(foreach)
require(data.table)
#Load Required Libraries
#library(gridExtra)
library(reshape)
library(scales)

fc_names <- c(
	      `F1` = "F1",
	      `precision` = "Precision", 
	      `recall` = "Recall", 
	      `meansim` = "Match Similarity", 
	      `othersim` = "Score Ratio", 
	      `alignsim` = "Alignment Similarity", 
	      `F1sim` = "F1 * sim", 
	      `indelscore` ="InDel Score", 
	      `similar` = "Similar",
	      `version` = "Version",
	      `suite` = "SW Prod. Line",
	      `compiler` = "Compiler",
	      `platform` = "Platform",
	      `architecture` = "Architecture",
	      `optimization` = "Opt. Level",
	      `elem` = "Element",
	      `arch` = "Architecture",
	      `diff` = "diff.",
	      `id` = "ident."

	      )

#Build Function to Return Element Text Object
rotatedAxisElementText = function(angle,position='x'){
	angle     = angle[1]; 
	position  = position[1]
	positions = list(x=0,y=90,top=180,right=270)
	if(!position %in% names(positions))
		stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
	if(!is.numeric(angle))
		stop("'angle' must be numeric",call.=FALSE)
	rads  = (angle - positions[[ position ]])*pi/180
	hjust = 0.5*(1 - sin(rads))
	vjust = 0.5*(1 + cos(rads))
	element_text(angle=angle,vjust=vjust,hjust=hjust)
}

gettouches <- function( ent,names,fnum ) {
	graph <- ent$chart$graph[[1]]
	#jumps = data.frame( src = unlist(apply(cbind(sapply(graph$nxt,length),data=graph$start),1,function(x) replicate(x[1], x[2]))) , tgt = unlist(graph$nxt))
	#print(ent$chart$edges[[1]])
	#print(str(ent$chart$edges[[1]]))
	jumps = data.frame()
	if(length(ent$chart$edges[[1]])>0) {
	jumps = data.frame( src = ent$chart$edges[[1]][,1], tgt = ent$chart$edges[[1]][,2] )
	jumps$type <- "jump"
	}

	#print(jumps)

	#print(ent$fkt$callees[[1]])
	inter <- data.frame() 
	if(length(ent$fkt$callees[[1]]) > 0) {
		if(is.matrix(ent$fkt$callees[[1]])) {
			#print(ent$fkt$callees[[1]][,1])
			#print(ent$fkt$callees[[1]][,2])
			inter <- data.frame(src = ent$fkt$callees[[1]][,1], tgt = ent$fkt$callees[[1]][,2])
	# inter$src <- ent$fkt$start
			inter$type <- "call"
		} else {
			inter <- data.frame(tgt = ent$fkt$callees[[1]])
			inter$src <- ent$fkt$start
			inter$type <- "call"
		}
	}

	entry <- data.frame(src = ent$fkt$start, tgt = ent$fkt$start, type = "entry")

	C <- rbind(entry,jumps, inter)
	C$fstart <- ent$fkt$start
	C$fend<- ent$fkt$end
	C$name <- ent$name
	C$fnum <- ent$fnum

	#print("filename")
	if(!is.null(names) && ! is.null(ent$name)) {
		#print("get filename")
		name = names[[ent$name]]
			#print("set filename")
			#print(name)
		C$filename <- name
		if(is.null(name) || name == "") {
		C$filename <- NA
		}

	}
	
	return( C)
}
	

parsefile <- function(metricsfile, namesfile) {
	metrics <- fromJSON(file(metricsfile))
	message("read json")

	filenames = NULL
	if(!is.null(namesfile)) {
		filenames <- fromJSON(file(namesfile))
		message("read filenames")
		#print(nrow(filenames))
		if(nrow(filenames) <= 1) { 
			warning("did not find names i was supposed to show") 
			filenames = hash()
		} else {
		# eat empty record
		filenames <- filenames[1:(nrow(filenames)-1),]
		filenames <- hash(filenames$"function", filenames$file)
		}
	}

	metrics$fnum <- seq.int(nrow(metrics))

	message("calculating data")
	agg <- list()
	pb <- txtProgressBar(1, nrow(metrics), style = 3)
	for(j in 1:nrow(metrics)) {
		data <- gettouches(metrics[j,],filenames )
		agg[[length(agg)+1]] <- data
		setTxtProgressBar(pb, j)
	}
	agg <- rbindlist(agg)
	close(pb)

	#agg <- foreach(d = iter(metrics, by = 'row'), .combine=rbind) %dopar% gettouches(d, filenames)

	# calculate target touches and enumerate sparse
	
	tgtidxfactor = factor(agg$tgt) 
#print(tgtidxfactor)
#print(unclass(tgtidxfactor))
	agg$tgtidx = as.numeric(unclass(tgtidxfactor))
#print(agg$tgtidx)
	agg$stgt = scale(agg$tgtidx)
#print(agg$fstart)



	agg$idxfstart = as.numeric(unclass(match(agg$fstart, levels(tgtidxfactor))))
	agg$idxfend = as.numeric(unclass(match(agg$fend, levels(tgtidxfactor))))
agg$idxfend[is.na(agg$idxfend)] <- 1+as.numeric(unclass(max(unclass(tgtidxfactor))))





	return(agg)
}


# 1x1 + 1     = 2 => refs
# 1x2 + 1     = 3 => refs + mods
# 1 + 2x1 + 1 = 4 => align refs
# 1 + 2x2 + 1 = 6 => align refs + mods
args <- commandArgs(TRUE)
metricsname = list()
outputname = args[length(args)]
args = args[1:(length(args) -1)]
namesname = list()
alignname = list()
if(length(i <- grep("json$",args ))) {
	matches = args[i]
	files = basename(matches)
	names = sub("(.*)[.]json","\\1",files)
	metricsname[names] = matches
} 
if(length(i <- grep("names$",args ))) {
	matches = args[i]
	files = basename(matches)
	names = sub("(.*)[.]names","\\1",files)
	namesname[names] = matches
} 
if(length(i <- grep("align$",args ))) {
	matches = args[i]
	files = basename(matches)
	names = sub("(.*)[.]align","\\1",files)
	alignname[names] = matches
}
#print(metricsname)
#print(namesname)
#print(alignname)


message("reading almanach")
almanach <- fromJSON(file("~/kuestennebel/almanach.json"))
almanach <- as.data.frame(unclass(almanach))

message("reading plot data")
metricsdata = list()
for(i in names(metricsname)) {
	message(i)
	data <- parsefile(metricsname[[i]], namesname[[i]])
	data$exe <- i
	metricsdata[[i]] = data
}
#print(names(metricsdata))

# load files
message("reading alignments")

alignagg <- data.frame()
agg <- data.frame()
if(length(alignname)) {
	for(i in names(alignname)) {
		message(alignname[[i]])
		alignment <- fromJSON(file(alignname[[i]]))
		align = alignment$alignment

		lfile = sub("(.*)_[a-z]*.json","\\1",alignment$fileA[1])
		rfile = sub("(.*)_[a-z]*.json","\\1",alignment$fileB[1])


lf = sub("seq/(.*)","\\1",lfile)
rf = sub("seq/(.*)","\\1",rfile)

		linfo = almanach[match(lf, almanach$file),]
		linfo$file = NULL
		rinfo = almanach[match(rf, almanach$file),]
		rinfo$file = NULL


		aligninfo = list()
		if(all(names(linfo) == names(rinfo))) {
			for( n in names(linfo)) {
				###aligninfo[paste0(n)] = paste0( ifelse( anyNA(linfo[[n]],rinfo[[n]]) , "unknown",
				###			       ifelse(identical (linfo[[n]] ,rinfo[[n]] ), "ident.", 
				###				      "diff.")), " ", fc_names[[n]])
				if(n!="id") {
					#print(n)
					aligninfo[paste0(n)] = paste0( ifelse( anyNA(linfo[[n]],rinfo[[n]]) , "unknown",
								       ifelse(identical (linfo[[n]] ,rinfo[[n]] ), "ident.", 
									      "diff.")), " ", fc_names[[n]])
				}
			}
		}
		aligninfo = as.data.frame(aligninfo)


		rid = NA
		lid = NA
		if(! is.na(linfo$id)) {
			lid = as.character(linfo[1,toString(linfo$id)])
			#message("lid: ", linfo$id, lid)
			linfo[,toString(linfo$id)] = NULL
			linfo$id = NULL
		}
		if(! is.na(rinfo$id)) {
			rid = as.character(rinfo[1,toString(rinfo$id)])
			#message("rid: ", rinfo$id, rid)
			rinfo[,toString(rinfo$id)] = NULL
			rinfo$id = NULL
		}

		lfile = basename(lfile)
		rfile = basename(rfile)

		lmode = sub(".*_([a-z]*)[.]json","\\1",basename(alignment$fileA[1]))
		rmode = sub(".*_([a-z]*)[.]json","\\1",basename(alignment$fileB[1]))
		mode = lmode
		if(lmode != rmode) {
			warning ("different modes in alignment")
			mode = paste0(lmode,rmode)
		} 
		zerosim = align$o
		zerosim[is.na(zerosim)] = 0
		alignlen = length(align$o)
		opt = sum(zerosim)
		minlen = min(as.numeric(alignment$fileA[2]), as.numeric(alignment$fileB[2]))
		maxlen = max(as.numeric(alignment$fileA[2]), as.numeric(alignment$fileB[2]))
		align$aligned = !is.na(align$ldata) & !is.na(align$rdata)


		matchnums = align[(!is.na(align$lnum) & !is.na(align$rnum)),"num"]
		#print(summary(matchnums))

		len19 = c()
		for(i in -19:19) {
			len19 = append(len19, matchnums+i)
		}
		len19 = unique(len19)
		len9 = c()
		for(i in -9:9) {
			len9 = append(len9, matchnums+i)
		}
		len9 = unique(len9)
		
		message(alignment$matches / alignment$length)
		entry =  data.frame(rid = rid, lid = lid, linfo = linfo, rinfo = rinfo, lfile = lfile, rfile = rfile, mode = mode, unique = alignment$unique, identical = alignment$identical, len = alignment$length, match = alignment$matches, match19 = alignment$match[20], match9 = alignment$match[10], opt, minlen, maxlen, alignlen, aligninfo = aligninfo, len19 = length(len19), len9 = length(len9))

		alignagg <- rbindlist(list(alignagg, entry),use.names=TRUE,fill=T)
#print(grepl("sub_", align$lname))
		align$match = ifelse(!grepl("sub_",align$lname), align$lname == align$rname, NA)
		#print(align)

		#lastr =NULL
		#data = NULL

		if(length(metricsname)) {
			message("reading individual plots")
			ldat = as.vector(na.omit(align[,c("lnum","num")])$num)
			rdat = as.vector(na.omit(align[,c("rnum","num")])$num)

			match = as.vector(align[,c("num","match")]$match)
			o= as.vector(align[,c("num","o")]$o)
			aligned= as.vector(align[,c("num","aligned")]$aligned)

			ldata = metricsdata[[lfile]]
			ldata$seq = ldat[ldata$fnum]
			#ldata$match = ifelse(ldata$type =="jump" || ldata$type == "call",  match[ldata$seq],NA)
			ldata$match = match[ldata$seq]
			ldata$o=o [ldata$seq]
			ldata$aligned=aligned [ldata$seq]
			rdata = metricsdata[[rfile]]
			rdata$seq = rdat[rdata$fnum]
			#rdata$match = ifelse(rdata$type == "jump" || ldata$type == "call",match[rdata$seq],NA)
			rdata$match = match[rdata$seq]
			rdata$o= o[rdata$seq]
			rdata$aligned=aligned [rdata$seq]

			ldata$pos = "L"
			rdata$pos = "R"

			message("changing identifications")
			print(is.na(lid))
			print(is.na(rid))

			if(!is.na(lid) && !is.na(rid)) {
				ldata$exe = lid
				rdata$exe = rid
			}

			#lastr = rdata
			data = rbind(ldata,rdata)

			if(!is.na(lid) && !is.na(rid)) {
				data$lfile = lid
				data$rfile = rid
			} else {
				data$lfile = lfile
				data$rfile = rfile
			}

			data$corr = i

			## implement global alignment
			#print("calculating global alignment")
			#print(summary(agg))
			#if(is.null(lastr)) {
			#	message("first iteration")
			##	agg$gseq = agg$seq
			#} else {
			#	# calculate new gseq numbers for agg using last and data
			#print(unique(	lastr$exe))
			#print(unique(ldata$exe))

			#	print(lastr$fnum)
			#	print(ldata$fnum)


			#}
			agg <- rbind(data,agg)
		}
	}
	agg$pos = factor(agg$pos)
	agg$corr = factor(agg$corr)
	agg$lfile = factor(agg$lfile)
	agg$rfile = factor(agg$rfile)
} else if(length(metricsname)) {
	for(i in metricsdata) {
		data = i
		data$seq = data$fnum
		agg <- rbind(data,agg)
	}
}


agg$type = factor(agg$type)
agg$name= factor(agg$name)
agg$exe = factor(agg$exe)
print(alignagg$lid)
alignagg$lid = factor(alignagg$lid)
alignagg$rid = factor(alignagg$rid)
print(levels(alignagg$lid))
print(levels(alignagg$rid))


agg <- as.data.frame(unclass(agg))

message("reading image size for: ", outputname)
sizes <- fromJSON(file("~/kuestennebel/imgsizes.json"))
sizes <- as.data.frame(unclass(sizes))
result <- sizes[sizes$name == outputname,]
print(result)
if(nrow(result)==1) {
	message("overiding width and height")
	width = result$width
	height = result$height
} else {
	message("using default width and height")
	width = 5
	height = 7
}



message("constructing plot")
p = ggplot()
message("setting base theme")
p = p + theme_bw()
p = p+ theme( text=element_text(size=9,family="serif"))
p = p+theme(plot.margin=unit(c(0,0,0,0),"mm"))
#p = p+theme(panel.margin=unit(c(0,0,0,0),"mm"))
#p = p+theme(legend.margin=unit(c(0,0,0,0),"mm"))

p = p+theme(panel.background = element_blank())
p = p+theme(panel.border = element_blank())
#p = p+theme(strip.background = element_rect(fill=NA))
p = p+theme(strip.background = element_blank())

message("reading annotations")
sizes <- fromJSON(file("~/kuestennebel/annotations.json"))
sizes <- as.data.frame(unclass(sizes))
result <- sizes[sizes$name == outputname,]
if(nrow(result)>0) {
	message("got annotations")
	#print(result)
	message("processing annotations")
	for(i in 1:nrow(result)) {
		geom = as.character(result[i,"geom"])
		row = result[i,]
		#print(row)
		#print(!apply(is.na(row), 2, all))
		current <- row[,!apply(is.na(row), 2, all)]
		current$name = NULL
		current$geom = NULL
		i <- sapply(current, is.factor)
		current[i] <- lapply(current[i], as.character)
		print(current)
		args = unclass(current)
		print(str(args))
		p = p + do.call(annotate,c(geom, args))
		#p = p + do.call(eval(parse(text=paste0("geom_",geom))), do.call(aes_q,args))
	}
}

message("determining plot type: ", length(metricsname))
if (length(metricsname)) {
	message("calculating mode: ", outputname)
	mode = sub(".*_([a-z]*)[.]png","\\1",outputname)
	message("mode: ", mode)

	hexlabels=function(x) {base::sprintf("0x%x",as.integer(x))}


	if(mode == "g") {
		agg$a = agg$src
		p = p + scale_y_reverse("Source Address", labels = hexlabels)
		agg$b = agg$tgt
		p = p + scale_x_continuous("Target Address", labels = hexlabels) 
		p = p + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
		if(nlevels(factor(agg$name)) < 10) {
			p=p + geom_rect(data = agg, aes(ymin = fstart-1, ymax= fend-1, xmin=fstart-1, xmax = fend-1, fill = name))
			p = p + scale_fill_brewer(name="Function Name", na.value=NA)
		}
	} else if(mode == "h") {

		agg$a = agg$src
		p = p + scale_y_reverse("Source Address", labels = hexlabels)
		agg$b = agg$tgtidx
		p = p + scale_x_continuous("Target Index") 
		p = p + theme(axis.text.x = element_text(hjust=0.8))
		if(nlevels(factor(agg$name)) < 10) {
			p=p + geom_rect(data = agg, aes(ymin = fstart, ymax= fend, xmin=-Inf, xmax = Inf, fill = name))
			p = p + scale_fill_brewer(name="Function Name", na.value=NA)
		}
	} else if(mode == "i") {

		agg$a = agg$src
		p = p + scale_y_reverse("Source Address", labels = hexlabels)
		agg$b = agg$stgt
		p = p + scale_x_continuous("Scaled Target") 
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = fstart, ymax= fend, xmin=-Inf, xmax = Inf, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "d") {

		agg$a = agg$seq
		p = p + scale_y_reverse("Source Function Index")
		agg$b = agg$tgt
		p = p + scale_x_continuous("Target Address", labels = hexlabels) 
		p = p + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = seq-0.5, ymax= seq+1-0.5, xmin=fstart-1, xmax = fend-1, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "e") {

		agg$a = agg$seq
		p = p + scale_y_reverse("Source Function Index")
		agg$b = agg$tgtidx
		p = p + scale_x_continuous("Target Index") 
		p = p + theme(axis.text.x = element_text(hjust=0.8))
	if(nlevels(factor(agg$name)) < 10) {
# XXX TODO
	p=p + geom_rect(data = agg, aes(ymin = seq-0.5, ymax= seq+1-0.5, xmin=idxfstart-0.5, xmax = idxfend-0.5, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "f") {

		agg$a = agg$seq
		p = p + scale_y_reverse("Source Function Index")
		agg$b = agg$stgt
		p = p + scale_x_continuous("Scaled Target") 
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = seq-0.5, ymax= seq+1-0.5, xmin=-Inf, xmax = Inf, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "a") {
		agg$a = agg$fstart
		p = p + scale_y_reverse("Source Function Address", labels=hexlabels)
		agg$b = agg$tgt
		p = p + scale_x_continuous("Target Address", labels = hexlabels) 
		p = p + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = fstart, ymax= fend, xmin=-Inf, xmax = Inf, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "b") {

		agg$a = agg$fstart
		p = p + scale_y_reverse("Source Function Address", labels=hexlabels)
		agg$b = agg$tgtidx
		p = p + scale_x_continuous("Target Index") 
		p = p + theme(axis.text.x = element_text(hjust=0.8))
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = fstart, ymax= fend, xmin=-Inf, xmax = Inf, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else if(mode == "c") {

		agg$a = agg$fstart
		p = p + scale_y_reverse("Source Function Address", labels=hexlabels)
		agg$b = agg$stgt
		p = p + scale_x_continuous("Scaled Target") 
	if(nlevels(factor(agg$name)) < 10) {
	p=p + geom_rect(data = agg, aes(ymin = fstart, ymax= fend, xmin=-Inf, xmax = Inf, fill = name))
	p = p + scale_fill_brewer(name="Function Name", na.value=NA)
	}
	} else {
		message("automatically chosing axes")
		agg$a = agg$seq
		if(length(alignname)) {
			p = p + scale_y_reverse("Aligned Function Index")
		} else {
			p = p + scale_y_reverse("Source Function Index")
		}
		if(length(alignname) && length(metricsname)>=2) {
			message("alignment of more than two files, use scaled addr idx")
			agg$b = agg$stgt
			p = p + scale_x_continuous("Scaled Target") 
		} else {
			message("normal, use normal target index")
			agg$b = agg$tgtidx
			p = p + scale_x_continuous("Target Index") 
		p = p + theme(axis.text.x = element_text(hjust=0.8))
		}

		if(length(metricsname) < 3 && length(namesname) > 0 && nlevels(factor(agg$filename))<10 && nlevels(factor(agg$filename)) > 1) {
			message("less than 3 files and filenames available, showing legend")
			p = p+ theme( legend.position= "bottom")
		} else if(mode == "sim") {
			p = p+ theme( legend.position= "bottom")
		} else if(mode == "mis") {
			p = p+ theme( legend.position="none") 
		} else if(mode == "norm") {
			p = p+ theme( legend.position="none") 
		} else if(mode == "verb") {
			p = p+ theme( legend.position="bottom") 
		} else {
			p = p+ theme( legend.position="none") 
		}
	}
	#scale_x_continuous("target address",


	if(length(namesname)) {
		agg$filename = factor(agg$filename)
		if(length(levels(agg$filename))>0 && nlevels(factor(agg$name)) >= 10) {
			message("filename information avalable")
			#if(mode == "inspect") {
			#	p=p + geom_rect(data = agg, aes(ymin = a, ymax= a+1, xmin=-Inf, xmax = Inf, fill = filename))#,alpha=0.1)) 
			#	#p = p + scale_fill_brewer()
			#} else {
				p=p + geom_rect(data = agg, aes(ymin = a, ymax= a+1, xmin=-Inf, xmax = Inf, fill = filename)) 
			#}
			p = p + scale_fill_brewer(name="Filename", na.value=NA)
			#p = p+scale_fill_hue(na.value= NA)
		}
	}

pointsize = 0.01
	if(length(alignname) && length(metricsname) > 2 ) {
		message("alignment available and more than two files: providing alignment pos as color")
		p = p+ geom_point(data = agg, aes(y=a, x=b, colour = pos), size=pointsize)
		if(mode == "mis") {
			message("highlighting indels")
			p=p + geom_rect(data = subset(agg,agg$aligned), aes(ymin = a, ymax= a+1, xmin=-Inf, xmax = -1.5, fill = aligned))
			p = p + scale_fill_manual(values = c("FALSE"="blue", "TRUE"= "white"))
		}
	} else {
		if(mode == "inspect") {
			p = p+ geom_point(data = agg, aes(y=a, x=b, colour = match), size=pointsize)
			p = p + scale_colour_manual(values=c("FALSE"="grey","TRUE"="black"), na.value="grey50")
		} else if(mode == "mis") {
			#p=p + geom_rect(data = agg, aes(ymin = a, ymax= a+1, xmin=-Inf, xmax = Inf, color= sim))

			#scale = scale_fill_manual(values=alpha(c("blue","white"),c(1,0)),e = "blue")

			p=p + geom_rect(data = subset(agg,agg$pos=="R"), aes(ymin = a, ymax= a+1, xmin=-Inf, xmax = -1.5, fill = aligned))
			p=p + geom_rect(data = subset(agg,agg$pos=="L"), aes(ymin = a, ymax= a+1, xmin=1.5, xmax = Inf, fill = aligned))

			p = p+ geom_point(data = agg, aes(y=a, x=b,colour=o), size=pointsize) 

			p = p + scale_fill_manual(values = c("FALSE"="blue", "TRUE"= "white"))
			#p = p + scale_fill_gradient(low="blue", high = "white")
						  #alpha(c("blue","grey50"),c(1,0)),limits=c(FALSE,TRUE))
			p = p + scale_color_gradient(low="grey50",high="#56B1F7",na.value="black")
		} else if(mode == "sim") {
			p = p+ geom_point(data = agg, aes(y=a, x=b, colour = o), size=pointsize)
		} else {
			p = p+ geom_point(data = agg, aes(y=a, x=b), size=pointsize)
		}
	}
	#theme_bw() + #,axis.text.x = element_text(angle = 0, hjust = 1/8,vjust = 1/2)
	if(length(alignname)) {
		message("alignment available")
		# TODO : detect line of alignments and show as wrappanel
		message(nlevels(agg$lfile))
		message(nlevels(agg$rfile))
		message(nlevels(agg$corr))
		if(nlevels(agg$lfile)*nlevels(agg$rfile)/2 > nlevels(agg$corr)) {
			message("line alignment")
			if(width>height) {
				message("width > height, single row (A)")
				p = p+facet_wrap(~lfile+rfile, nrow = 1, scales = "free", labeller = label_wrap_gen())
			} else {
				p = p+facet_wrap(~lfile+rfile, scales = "free", labeller = label_wrap_gen())
			}
			#p = p+facet_wrap(~lfile+rfile, scales = "free")
		} else if(length(metricsname)< 3) {
			message("pair alignment")
			p=p+facet_grid( ~exe, scales = "free",space="free")
		} else {
			message("matrix alignment")
			p=p+facet_grid( rfile~lfile, scales = "free",space="free")
		}
		#lfile~exe+corr+rfile
		#p=p+facet_wrap(~lfile+rfile)
	} else {
		message("alignment available")
		if(length(metricsname)> 3) {
		message("wrapping many executables")
			if(width>height) {
				message("width > height, single row (B)")
				p = p+facet_wrap(~exe, nrow = 1, scales = "free") 
			} else {
				p = p+facet_wrap(~exe, scales = "free") 
			}
		} else {
		message("wrapping few executables")
			if(width>height) {
				message("width > height, single row (C)")
				p = p+facet_wrap(~exe, nrow = 1, scales = "free_x") 
			} else {
				p = p+facet_wrap(~exe, scales = "free_x") 
			}
		}
	}
	#ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
#print(summary(agg))
	write.table(agg, file=paste0(outputname,".csv"))
} else { # only alignments
	message("only alignments")
	alignagg$precision= alignagg$match / alignagg$len
	alignagg$precision9= alignagg$match9 / alignagg$len9
	alignagg$precision19= alignagg$match19 / alignagg$len19
	alignagg$recall   = ifelse(alignagg$identical!=0, alignagg$match / alignagg$identical   , 0 ) 
	alignagg$recall9  = ifelse(alignagg$identical!=0, alignagg$match9 / alignagg$identical  , 0 ) 
	alignagg$recall19 = ifelse(alignagg$identical!=0, alignagg$match19 / alignagg$identical , 0 ) 
	alignagg$similar = alignagg$identical / alignagg$unique

	F1num = 2*alignagg$precision*alignagg$recall
	F1denum = alignagg$precision + alignagg$recall
	F2num = 5*alignagg$precision*alignagg$recall
	F2denum = 4*alignagg$precision + alignagg$recall
	alignagg$F1 = ifelse(F1num!=0, F1num/F1denum, 0)
	alignagg$F2 = ifelse(F2num!=0, F2num/F2denum, 0)
	alignagg$meansim =  alignagg$opt/alignagg$len
	alignagg$alignsim = alignagg$opt/alignagg$alignlen
	alignagg$othersim = alignagg$opt/alignagg$minlen
	alignagg$indels = alignagg$alignlen - alignagg$len
	alignagg$minindels = alignagg$maxlen - alignagg$minlen
	alignagg$indelscore = (alignagg$indels - alignagg$minindels) / alignagg$indels
	alignagg$F1sim = alignagg$F1*alignagg$similar
	#print(alignagg)
	print(summary(alignagg))

	message(nlevels(factor(alignagg$mode)))
	levels(alignagg$mode) <- list( "Raw InvSim" = "flen",
"Raw ExpSim" = "bflen",
"Raw MSim" = "cflen",
"Raw RSim" = "dflen",
"Scaled InvSim"="nflen",
"Scaled ExpSim"="bnflen", 
"Scaled MSim"="cnflen", 
"Scaled RSim"="dnflen", 
"Raw Diff" = "aflen",
"Scaled Diff"="anflen", 
"Name Identity"="truth")

	message("collecting facets")
	lfacets = c()
	for (i in grep("linfo",names(alignagg),value = TRUE) ) {
		if(nlevels(factor(alignagg[[i]])) > 1) {
			lfacets = c(lfacets, i)
		}
	}
	rfacets = c()
	for (i in grep("rinfo",names(alignagg),value = TRUE) ) {
		if(nlevels(factor(alignagg[[i]])) > 1) {
			rfacets = c(rfacets, i)
		}
	}
	aligninfo = c()
	for (i in grep("aligninfo",names(alignagg),value = TRUE) ) {
		if(nlevels(factor(alignagg[[i]])) > 1) {
			aligninfo = c(aligninfo, i)
		}
	}
	message("lfacets: ", lfacets)
	message("rfacets: ", rfacets)
	message("aligninfo: ", aligninfo)

	message("output name: ", outputname)
	mode = sub(".*_([a-zA-Z0-9]*)[.]png","\\1",outputname)
	message("mode: ", mode)
	label=mode
	if(mode == "F1" || mode == "F1a") {
		alignagg$data <- alignagg$F1
		label = parse(text="F[1]")
	} else if (mode == "F2") {
		alignagg$data <- alignagg$F2
	} else if (mode == "precision") {
		alignagg$data <- alignagg$precision
	} else if (mode == "recall") {
		alignagg$data <- alignagg$recall
		label = "Stability"
	} else if (mode == "similarity") {
		alignagg$data <- alignagg$similar
		label = "Similarity"
	} else if (mode == "potential") {
		alignagg$data <- alignagg$similar * alignagg$recall
		label = "potential"
	} else if (mode == "simA") {
		alignagg$data = alignagg$meansim
		label = "mean match similarity"
	} else if (mode == "simB") {
		alignagg$data <- alignagg$othersim
		label = "score ratio"
	} else if (mode == "simC") {
		alignagg$data <- alignagg$alignsim
		label = "mean alignment similarity"
	} else if (mode == "corA") {
		alignagg$datay = alignagg$meansim
		labely = "mean match similarity"
		alignagg$datax = alignagg$F1
		labelx = parse(text="F[1]")
	} else if (mode == "corB") {
		alignagg$datay = alignagg$othersim
		labely = "score ratio"
		alignagg$datax = alignagg$F1
		labelx = "F1"
		labelx = parse(text="F[1]")
	} else if (mode == "corC") {
		alignagg$datay = alignagg$alignsim
		labely = "mean alignment similarity"
		alignagg$datax = alignagg$F1
		labelx = "F1"
		labelx = parse(text="F[1]")
	} else if (mode == "corD") {
		alignagg$datay = alignagg$similar
		labely = "Similarity"
		alignagg$datax = alignagg$F1
		labelx = "F1"
		labelx = parse(text="F[1]")
	} else if (mode == "cor") {
		alignaggorig = alignagg
		alignagg = melt(alignagg, id=c(aligninfo,"precision","recall", "F1","rfile","lfile","lid","rid",lfacets,rfacets,"mode"), measure=c("alignsim","similar"))
		alignagg$datay = alignagg$value
		labely = ""
		alignagg$datax = alignagg$F1
		labelx = "F1"
		labelx = parse(text="F[1]")
	} else if (mode == "pr") {
		alignaggorig = alignagg
		alignagg$datay = alignagg$precision
		labely = "Precision"
		alignagg$datax = alignagg$recall
		labelx = "Recall"
	} else if (mode == "vsA") {
		alignaggorig = alignagg
		alignagg = melt(alignagg, id=c(aligninfo,"rfile","lfile","lid","rid",lfacets,rfacets,"mode"), measure=c("precision","recall", "F1","meansim","othersim", "alignsim"))
		alignagg$datay = alignagg$value
		p = p+ theme( legend.position= "bottom")
		#alignagg$datax = alignagg$F1

		labely = ""
		labelx = ""
		label = ""
	} else if (mode == "vsB") {
		alignaggorig = alignagg
		alignagg = melt(alignagg, id=c(aligninfo,"rfile","lfile","lid","rid",lfacets,rfacets,"mode"), measure=c("precision","recall", "F1", "alignsim"))
		alignagg$datay = alignagg$value
		p = p+ theme( legend.position= "bottom")
		#alignagg$datax = alignagg$F1
		labely = ""
		labelx = ""
		label = ""
	} else if (mode == "vsC") {
		alignaggorig = alignagg
		alignagg = melt(alignagg, id=c(aligninfo,"rfile","lfile","lid","rid",lfacets,rfacets,"mode"), measure=c("precision","recall", "F1","F1sim", "similar" ,"meansim","othersim", "alignsim", "indelscore"))
		alignagg$datay = alignagg$value
		#alignagg$datax = alignagg$F1
		labely = ""
		labelx = ""
		label = ""
	} else if (mode == "bio") {
		## meansim vs len with similar ?F1
		#alignagg$good = !(alignagg$similar > 2*alignagg$F1 | alignagg$similar*2 < alignagg$F1)
		alignagg$good = !(alignagg$similar > 4/3*alignagg$F1)
	} else {
		warning("unknown mode: ", mode)
	}
	print(summary(alignagg))
	write.table(alignagg, file=paste0(outputname,".csv"))

	if (mode == "corA" || mode == "corB" || mode == "corC" || mode == "corD" || mode == "cor" || mode == "pr" ) {
		message("correlations")
		#if(nlevels(factor(alignagg$mode)) == 1) {
		#	message("single alignment mode")
		#	p = p+geom_point(data = alignagg, aes_q(y=~datay, x = ~datax)) #,colour = as.name(lfacets[1]), shape=as.name(rfacets[1])))
		#	p=p+labs(y=labely, x = labelx)
		#	p = p + scale_x_continuous(labels=scales::percent) + scale_y_continuous(labels=scales::percent)
		#	#if(nlevels(factor(alignagg$variable))>1) {
		#	#	fit <- lm(F1~data:variable-1, alignagg)
		#	#	fitB <- lm(F1>4/5~(data>4/5):variable-1, alignagg)
		#	#} else {
		#	#	fit <- lm(F1~data-1, alignagg)
		#	#	fitB <- lm(F1>4/5~(data>4/5)-1, alignagg)
		#	#}
		#	#print(summary(fit))
		#	#print(summary(fitB))
		#	#stargazer(fit,fitB)
		#} else {
		differences = c()

		for (i in grep("aligninfo",names(alignagg),value = TRUE) ) {
			if(nlevels(factor(alignagg[[i]])) > 1) {
				differences = c(differences, i)
			}
		}
		message("differences: ", differences)
		
		if(nlevels(factor(alignagg$linfo.suite)) > 1) {
			message("more than one suite")
			rndid <- with(alignagg, ave(datay, linfo.suite, FUN=function(x) {sample.int(length(x))}))
			alignsample = alignagg[rndid<=100,]
			p = p+geom_point(data = alignsample, aes_q(y=~datay, x = ~datax, shape=~linfo.suite))
			p = p + scale_colour_brewer(guide=guide_legend(title="Testsuite",nrow=2))
			#p = p + scale_shape_discrete(guide=guide_legend(title="Data Preparation"))
		} else if(length(differences) > 0 && length(differences) < 6 ){
			formula = paste0("~",paste(differences, collapse=":"))
			message("annotating differences: ", formula)
			if(nlevels(factor(alignagg$mode)) == 1) {
				message("single alignment mode")
			p = p+geom_point(data = alignagg, aes_q(y=~datay, x = ~datax, colour=as.formula(formula), shape = as.formula(formula)))
			p = p + scale_shape_discrete(guide=guide_legend(title="Variation",ncol=2))
			p = p + scale_colour_brewer(guide=guide_legend(title="Variation",ncol=2))
			}
			else {
				message("multiple alignment modes")
			p = p+geom_point(data = alignagg, aes_q(y=~datay, x = ~datax, shape = ~mode, colour = as.formula(formula)))
			p = p + scale_colour_brewer(guide=guide_legend(title="Variation",ncol=2))
			p = p + scale_shape_discrete(guide=guide_legend(title="Data Preparation"))
	}
		} else {
			message("plain correlation")
			p = p+geom_point(data = alignagg, aes_q(y=~datay, x = ~datax, shape = ~mode)) #,colour = as.name(lfacets[1]), shape=as.name(rfacets[1])))
			p = p + scale_shape_discrete(guide=guide_legend(title="Data Preparation"))
		}
		p = p+ theme( legend.position= "bottom")
		p=p+labs(y=labely, x = labelx)
		p = p + scale_x_continuous(labels=scales::percent) + scale_y_continuous(labels=scales::percent)
		#if(nlevels(factor(alignagg$variable))>1) {
		#	fit <- lm(F1~data:variable:mode-1, alignagg)
		#	fitB <- lm(F1>4/5~(data>4/5):mode:variable-1, alignagg)
		#} else {
		#	fit <- lm(F1~data:mode-1, alignagg)
		#	fitB <- lm(F1>4/5~(data>4/5):mode-1, alignagg)
		#}
		#print(summary(fit))
		#print(summary(fitB))
		#if(mode == "cor") {
		#	fitA <- lm(F1~meansim, alignaggorig)
		#	fitflenA <- lm(F1~meansim, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenA <- lm(F1~meansim, alignaggorig,alignaggorig$mode=="nflen")
		#	fitB <- lm(F1~othersim, alignaggorig)
		#	fitflenB <- lm(F1~othersim, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenB <- lm(F1~othersim, alignaggorig,alignaggorig$mode=="nflen")
		#	fitC <- lm(F1~alignsim, alignaggorig)
		#	fitflenC <- lm(F1~alignsim, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenC <- lm(F1~alignsim, alignaggorig,alignaggorig$mode=="nflen")
		#	fitA1 <- lm(F1~meansim-1, alignaggorig)
		#	fitflenA1 <- lm(F1~meansim-1, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenA1 <- lm(F1~meansim-1, alignaggorig,alignaggorig$mode=="nflen")
		#	fitB1 <- lm(F1~othersim-1, alignaggorig)
		#	fitflenB1 <- lm(F1~othersim-1, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenB1 <- lm(F1~othersim-1, alignaggorig,alignaggorig$mode=="nflen")
		#	fitC1 <- lm(F1~alignsim-1, alignaggorig)
		#	fitflenC1 <- lm(F1~alignsim-1, alignaggorig,alignaggorig$mode=="flen")
		#	fitnflenC1 <- lm(F1~alignsim-1, alignaggorig,alignaggorig$mode=="nflen")
		#	stargazer( fitA, fitflenA, fitnflenA )
		#	stargazer( fitA1, fitflenA1, fitnflenA1 )
		#	stargazer( fitB, fitflenB, fitnflenB )
		#	stargazer( fitB1, fitflenB1, fitnflenB1 )
		#	stargazer( fitC, fitflenC, fitnflenC)
		#	stargazer( fitC1, fitflenC1, fitnflenC1)

		#}
		#}
		if(nlevels(factor(alignagg$variable))>1) {
			message("facets for variable")
			p = p + facet_wrap(~variable, labeller = as_labeller(fc_names))
		}
	} else if ( mode == "bio") {
		p = p+geom_point(data = alignagg, aes(y=alignsim, x = log(len), colour = linfo.suite, shape = good))
	} else if ( mode == "truth") {
		message("truth suite info")
		p = p+geom_boxplot(data = alignagg, aes(linfo.suite, recall)) + coord_flip()
	} else {

		if(nlevels(factor(alignagg$mode)) == 1 && nlevels(alignagg$lfile) < 80) {
			message("tileplot")
			mode2="tileplot"
			if(nlevels(alignagg$lfile) > 20) {
				message("using small fonts")
				#p = p+ theme( text=element_text(size=7,family="serif"))
				p = p+ theme( axis.text=element_text(size=6,family="serif"))
			}

			message("facet lengths: ", length(rfacets), " ", length(lfacets))
			if(mode == "vsA" || mode == "vsB" || mode == "vsC") {
				if(length(rfacets)== 1 && (is.null(alignagg$rid) || is.na(alignagg$rid))) {
					alignagg$rid <- alignagg[[rfacets[length(rfacets)]]]
				#} else if(length(rfacets)> 1) {
					#alignagg$rid = as.factor(do.call(paste, c(alignagg[rfacets],sep= ":")))
				}
				if(length(lfacets)==1 && (is.null(alignagg$lid) || is.na(alignagg$lid))) {
					alignagg$lid <- alignagg[[lfacets[length(lfacets)]]]
				#} else if(length(lfacets)> 1) {
					#alignagg$lid = as.factor(do.call(paste, c(alignagg[lfacets],sep= ":")))
				}

			} else {
				if(length(rfacets)>= 1 && (is.null(alignagg$rid) || is.na(alignagg$rid))) {
					message("using facet as rid")
					alignagg$rid <- alignagg[[rfacets[length(rfacets)]]]
					rfacets = rfacets[-length(rfacets)]
				}
				if(length(lfacets)>= 1 && (is.null(alignagg$lid) || is.na(alignagg$lid))) {
					message("using facet as lid")
					alignagg$lid <- alignagg[[lfacets[length(lfacets)]]]
					lfacets = lfacets[-length(lfacets)]
				}
			}
			if((is.null(alignagg$rid) || is.na(alignagg$rid))) {
				alignagg$rid <- alignagg$rfile
			}
			if((is.null(alignagg$lid) || is.na(alignagg$lid))) {
				alignagg$lid <- alignagg$lfile
			}

# XXX
print(alignagg$lid)
			order =  unique(c(levels(alignagg$lid),levels(alignagg$rid)))
			alignagg$rid = factor(alignagg$rid, levels=order)
			alignagg$lid = factor(alignagg$lid, levels=rev(order))

print(summary(alignagg$data))
print(nlevels(factor(signif(alignagg$data*100,digits=2))))

				num_cuts = min(5, nlevels(factor(alignagg$data)))
				message("cutting into: ", num_cuts)
				if(num_cuts>1) {
					#alignagg$data = tryCatch( {return(cut_number(signif(100*alignagg$data),num_cuts))}, error=function(err) { return(cut(alignagg$data, breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c("[0,20]%","(20,40]%","(40,60]%","(60,80]%","(80,100]%"), include.lowest = TRUE))} )
					alignagg$data = cut(alignagg$data, breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c("[0,20]%","(20,40]%","(40,60]%","(60,80]%","(80,100]%"), include.lowest = TRUE)
					#alignagg$data = cut_number(100*alignagg$data,num_cuts)
				}
				if(mode == "recall") {
message("plotting in greens")
				p = p + scale_fill_brewer(name = label, palette = "Greens", drop = F)
				#p = p + scale_fill_grey(name = label, drop = F)
} else if(mode == "similarity" || mode == "potential") {
				p = p + scale_fill_brewer(name = label, palette = "Reds", drop = F)
				#p = p + scale_fill_grey(name = label, drop = F)
} else {
message("plotting in blues (default)")
				p = p + scale_fill_brewer(name = label, palette = "Blues", drop = F)
				#p = p + scale_fill_brewer(name=label, drop = F)
				#p = p + scale_fill_grey(name = label, drop = F)
}
			message("tile plot")
			p = p+ geom_tile(data = alignagg, aes(y = lid, x = rid, fill = data)) #, width = (1+similar)/2))
			p = p+theme( axis.text.x = element_text(angle = 90, hjust = 1,vjust = 1/2))
			#p = p + theme(axis.text.x = rotatedAxisElementText(45, 'x'), axis.text.y = rotatedAxisElementText(45, 'y'))
			p=p+labs(fill=label, x="",y="")

		} else {
			message("boxplot")
			mode2 = "boxplot"
			# function for number of observations 
			give.n <- function(x){
				  return(data.frame(y = median(x)*1.05, label = length(x))) 
			  # experiment with the multiplier to find the perfect position
			}

			# function for mean labels
			mean.n <- function(x){
				  return(data.frame(y = median(x)*0.97, label = round(mean(x),2))) 
			  # experiment with the multiplier to find the perfect position
			}
			p = p+ geom_boxplot(data = alignagg, aes(reorder(mode,data, FUN=mean), data)) 
			#p =p + stat_summary(fun.data = give.n, geom = "text", fun.y = median) # +
			#	   stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red")
			p=p+labs(y=label, x="Data Preparation")
			p = p + scale_y_continuous(labels=scales::percent)
			p = p + coord_flip()

		}
		message("number of facets: ", length(rfacets) + length(lfacets))
		message("lfacets: ", lfacets)
		message("rfacets: ", rfacets)
		if(mode != "F1a") {
			message(mode)

		if(mode == "vsA" || mode == "vsB" || mode == "vsC" || mode == "cor") {
		#	if(width>height) {
		#		message("width > height, single row (D)")
		#		p = p + facet_wrap(~variable,nrow=1, labeller = as_labeller(fc_names))
		#	} else {
				p = p + facet_wrap(~variable, labeller = as_labeller(fc_names))
			#}
		} else 
			if(length(rfacets) + length(lfacets) > 0 && length(rfacets) + length(lfacets) <= 4 && mode2 != "boxplot") {
			differences = c()
			for (i in grep("aligninfo",names(alignagg),value = TRUE) ) {
				if(nlevels(factor(alignagg[[i]])) > 1) {
					differences = c(differences, i)
				}
			}
			message("number of differences: ", length(differences))
			if(length(differences) > 0) {
				#if(length(rfacets)>= 1) {
				#	rfacets = rfacets[-length(rfacets)]
				#}
				#if(length(lfacets)>= 1) {
				#	lfacets = lfacets[-length(lfacets)]
				#}
				print(lfacets <- paste(lfacets, collapse = "+"))
				print(rfacets <- paste(rfacets, collapse = "+"))
				formula = paste(c(lfacets,rfacets),collapse="~")
				message("facetting data A: ", formula)
				p = p + facet_grid(as.formula(formula),scales="free", space="free")
			} else {
				if(all(alignagg[,lfacets] == alignagg[,rfacets])) {
					message("all values in facets identical")
					print(facets <- paste(rfacets, collapse = "+"))
				} else {
					message("values in facets differ")
					print(facets <- paste(unique(c(lfacets,rfacets)), collapse = "+"))
				}
				formula = paste0("~",facets)
				message("facetting data B: ", formula)
				p = p + facet_wrap(as.formula(formula))
				p = p+ theme( legend.position= "bottom")
			}
		} else if(length(rfacets) + length(lfacets) > 0) {
			differences = c()
			for (i in grep("aligninfo",names(alignagg),value = TRUE) ) {
				message(i, " contains: ", levels(factor(alignagg[[i]])))
				if(nlevels(factor(alignagg[[i]])) > 1) {
					differences = c(differences, i)
				}
			}
			len = length(differences)
			message("len: ", len, " width: ", width, " height: ", height)
			message("differences: ", differences)
			split = ceiling(len*width/(width+height))
			message("split: ", split)
			right = paste(head(differences,split), collapse="+")
			left = paste(tail(differences,-split), collapse="+")
			formula = paste0(left,"~",right)
			message("facetting data C: ", formula)
			if(formula != "~") {
			p = p + facet_grid(as.formula(formula), margins=
c(
head(head(differences,split),1)
,
head(tail(differences,-split),1)
)
)
			}
			###formula = "F1~0+"
			###if(nlevels(factor(alignagg$mode)) > 1) {
			###	formula = paste0(formula, "mode+")
			###}
			######formula = paste0(formula,paste0(differences,collapse="+"),"")
			####message(str(alignagg))
			###message("formula: ", formula)
			###fit <- lm(as.formula(formula), alignagg)
			###print(summary(fit))
		}
		}
	}
}

outfile <- function(file, width, height,plot,...) {
	tmp = sub("(.*)[.]pdf","\\1.tmp.pdf",file)
	png = sub("(.*)[.]pdf","\\1.png",file)
	message("width: ", width)
	message("height: ", height)
	if(length(grep("pdf",file))>0) {
		message("writing: ", png)
		ggsave(png,width=width,height=height,dpi=600,plot=plot, ...)
		message("writing: ", file)
		ggsave(file,width=width,height=height,dpi=600,plot=plot,...)
		#message("embedding fonts: ", file)
		#embed_fonts(tmp,outfile=file,options="-dEmbedAllFonts=true -dSubsetFonts=true -dMaxSubsetPct=100 -dPDFSETTINGS=/prepress")
	} else {
		message("writing: ", file)
		ggsave(file,width=width,height=height,dpi=600,plot=plot, ...)
	}
}



#ggsave(outputname,plot = p, width = width, height = height)
message("plotting and saving")
outfile(outputname, width = width, height = height, plot=p)

warnings()

#geom_bin2d(bins = nrow(metrics), drop=TRUE) +
#geom_raster() + 
#scale_y_discrete(breaks=ybreaks) + 
#plot.margin=unit(c(0,0,0,0),"mm"),
#+ facet_grid(filename~.)

#scale_x_continuous(breaks=xbreaks)
#scale_y_discrete(breaks = seq(minfkt,maxfkt,100))
#+stat_bin2d(aes(fill=..count..),bins=nrow(metrics))
# geom_raster()
# + geom_count() + geom_density_2d()
#+ geom_hex()
#data <- sapply(metrics, gettouches)

#head(data)

#minfkt = min(agg$fkt)
#maxfkt = max(agg$fkt)
#minfkt
#maxfkt
#agg$fkt = reorder(agg$fkt,-agg$fkt)
#agg$fkt = factor(agg$fkt)
##agg$fkt=as.numeric(levels(agg$fkt))[agg$fkt]
##agg$touch= factor(agg$touch)

#ybreaks = agg$fkt[seq(1,length(agg$fkt),length(agg$fkt)/height)]
#xbreaks = agg$fkt[seq(1,length(agg$fkt),length(agg$fkt)/width)]
#xbreaks = as.numeric(levels(xbreaks))[xbreaks]
