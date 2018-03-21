#!/usr/bin/Rscript
filnavn <- 'Mandelplot_x'
bilder <- T

tid <- proc.time()
iter <- c(20,30) * 1				# Iterasjoner: Fra, Til
dim <- c(1600,1200) * 1/2 			# Oppløsning
t <- digamma(1)
r = (1 - cos(t))/2
p <- c(r*cos(t)+1/4,r*sin(t),5e-2)		# Posisjon: x, y, avvik
#p <- c(-0.7, 0, 1.2)
print(paste('Oppløsning:',dim[1],'x',dim[2]))

felt <- function(a,b,c) {
    re <- rep(seq(a-c, a+c,			# Horisontale komponenter
	length.out=dim[1]), dim[2])		# Gjentar verdiene i hver søyle
    im <- rep(seq(b-c, b+c,			# Vertikale komponenter
	length.out=dim[2]), each=dim[1])	# Gjentar verdiene for hver rad
    f <- complex(real=re, imag=im)		# Todimensjonale tall
    return(matrix(f,dim[1],dim[2]))		# Todimensjonal struktur
}

Z <- matrix(0,dim[1],dim[2])			# Startverdi
C <- felt(p[1], p[2], p[3])	 		# Konstanten er en virvel

bilde <- function(z) {
    hvit <- function(n) {
	paste(replicate(n, '#FFFFFF'),sep=',')	# Skviser regnbuen
    }
    pal <- c(hvit(1), rev(rainbow(211,		# Klønete fargepalett
	start=0,end=1)), hvit(1))
    image(1:nrow(z), 1:ncol(z), z,		# Lager bilde av z
	axes=FALSE, col=pal)
}

normaliser <- function(z,k) {
    l <- 0
    m <- 1
    X <- matrix(0, dim[1], dim[2])
    for (a in c(z)) {
        #print(paste(l%%u+1,l,m,abs(a)))
        if (is.finite(abs(a)) && abs(a) >= 1) {
#            X[l%%dim[1]+1,m] <- k+1-log(log(abs(a)))/log(2)
	    y <- 2 - log(log(Mod(a))/log(10^5))
	    X[l%%dim[1]+1,m] <- log(abs(a))/2^y
        } else X[l%%dim[1]+1,m] <- 0
        l <- l + 1
        if (l%%dim[1] == 0) m <- m + 1
    }
    return(X)
}

for (i in 1:iter[2]) {
    Z <- Z * Z + C				# Differensligningen
    if (i >= iter[1]) {
        if (bilder || i == iter[2]) {		
	    #M <- iter[2]-exp(-Mod(Z))		# Dempet lengde av pikslene
	    #M <- Arg(Z)			# Vinkelen til pikslene
	    M <- normaliser(Z,i)		# Farger utsiden
        }
        if (bilder) {
	    if (iter[1] == iter[2]) o <- '' else o <- i
	    f <- paste(filnavn,o,'.png',sep='')
            png(filename=f,
		width=dim[1], height=dim[2])	# Starter grafikkdriveren
            par(mar=c(0,0,0,0))			# Fjerner marginer
            bilde(M)				# Lagrer bildet
            dev.off()				# Lukker grafikkdriveren
            print(f)
        }
    }
}

if (bilder && iter[1] < iter[2]) {
    system(intern=TRUE, paste(			# Kjører kommando
	'/bin/bash -c "convert ', filnavn,	# Lager animasjon
	'{{',iter[1],'..',(iter[2]-1),		# Bashish	
	'},{',iter[2],'..',(iter[1]+1),'}}.png',# Utvides til filnavnene
	' -delay ',250,' -loop 0 ',		# Krever ImageMagick
	filnavn,'.gif" 2>&1',sep=''))		# Viser eventuelle feil
    print(paste(filnavn,'.gif',sep=''))
} #else {
#    par(mar=c(5,4,4,2)+0.1)			# Viser marginer
#    bilde(M)					# Produserer bildet
#}
#print(t(M[,ncol(M):1]))
print(proc.time()-tid)
