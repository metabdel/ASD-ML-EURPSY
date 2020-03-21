##sampling rate: 306ASD to 68000 UNAFFECTER ~ .45%

sample_cases_ctrlgene = function(df, num , seenmid = c() ) {
    tot = nrow(df)
    seennum = c()
    for (  i in 1:num ) {
        while(length(seennum) < i ) {
            rw = sample(1:tot, 1  )
            if ( rw %in% seennum ) {
                next 
            }
            else {
                mid = df[rw,]$MID
                if ( mid %in% seenmid ) {
                    next 
                }
                else {
                    seennum = c(seennum, rw)
                    seenmid = c(seenmid, mid)
                }
            }
        }
    }
    f = list()
    f$seennum = seennum
    f$mid = seenmid
    return(f)
}

##check if seenmid in sample_Cases_ctrlgene bottlenedcks the diversity of the cases:

subset_df_ctrlgene = function(asd.cases, unaffected.cases, asdnum = 150, asdrate = .0045) {
    un.tot = asdnum / asdrate 
    sample.asd = sample_cases_ctrlgene(asd.cases, asdnum )
    seenmid = sample.asd$mid
    un.seenum = 1:nrow(unaffected.cases)
    un.mid = unique(unaffected.cases$MID)
    sample.un = list()
    sample.un$seennum = un.seenum
    sample.un$mid = un.mid
    #sample.un = sample_cases_ctrlgene(unaffected.cases, un.tot  )
    samples = list()
    samples$asd = sample.asd$seennum 
    samples$un = sample.un$seennum
    return(samples)
}

generate_fc = function(asd.cases, unaffected.cases, asdnum = 150 , asdrate = .0045 ){
    samp = subset_df_ctrlgene(asd.cases, unaffected.cases, asdnum , asdrate )
    asd.cid = asd.cases[samp$asd,]
    un.cid = unaffected.cases[samp$un,]
    asd.sample = asd[which(asd$CID %in% asd.cid$CID), ]
    un.sample = unaffected[unaffected$CID %in% un.cid$CID, ]
    un.sample = un.sample[,c(2,41)]
    un.sample = un.sample[! duplicated(un.sample),] 
    asd.sample = asd.sample[,c(2,41)]
    asd.sample = asd.sample[! duplicated(asd.sample),] 
    tot.drug = unique(c(asd.sample$atc_manual, un.sample$atc_manual))
    ii = 1 
    dg = c() ; acnt = c() ; uncnt = c() ; nacnt = c() ; nuncnt = c() ;  fccnt = c() ; pvcnt = c()
    for ( i in tot.drug )
    {
        #print(paste("working on ", ii, "of " , length(tot.drug) ))
        asd.cnt = nrow(asd.sample[asd.sample$atc_manual == i ,])
        un.cnt = nrow(un.sample[un.sample$atc_manual == i ,])
        asd.tot = length(unique(asd.sample$CID))
        un.tot = length(unique(un.sample$CID))
        not.asd.cnt = asd.tot - asd.cnt
        not.un.cnt = un.tot  - un.cnt
        mat = as.matrix(cbind(c(asd.cnt, un.cnt), c(not.asd.cnt, not.un.cnt)))
        ft = fisher.test(mat, alternative = "greater")
        pval = ft$p.value
        fc = log((asd.cnt / asd.tot )/ (un.cnt /un.tot) )
        dg = c(dg, i )
        acnt = c(acnt, asd.cnt)
        uncnt = c(uncnt, un.cnt)
        nacnt = c(nacnt, not.asd.cnt)
        nuncnt = c( nuncnt, not.un.cnt )
        fccnt = c(fccnt, fc)
        pvcnt = c(pvcnt, pval)
        ii = ii + 1
    }
    df.f = as.data.frame(data.matrix(cbind(dg,acnt,uncnt, nacnt, nuncnt, fccnt, pvcnt)))
    df.f$acnt = as.numeric(as.character(df.f$acnt))
    df.f$uncnt = as.numeric(as.character(df.f$uncnt))
    df.f$nacnt = as.numeric(as.character(df.f$nacnt))
    df.f$nuncnt = as.numeric(as.character(df.f$nuncnt))
    df.f$fccnt = as.numeric(as.character(df.f$fccnt))
    df.f$pvcnt = as.numeric(as.character(df.f$pvcnt))
    return(df.f)
}




