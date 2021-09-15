# NORMAL ####
normcall = quote(longdata_randquad(dataseed=seed, numgroups=3,
                                           numtraj=200, numobs=25, noise=.01,
                                           int.sd=.1, slo.sd=.1, q.sd=.1))

# Trends
tmpcall = do.call(substitute, list(normcall))
pdf(file.path(SIM_PDF_DIR, 'normal' , sprintf('trend %s.pdf', deparse(tmpcall) %>% paste(collapse='')))); for(seed in 1:20) {
    cat(paste0('seed=', seed, '\n'))
    tsdata = eval(normcall, list(seed=seed))
    silhouette = compute_silhouette(tsdata, tsdata_groups(tsdata))
    subcall = do.call(substitute, list(normcall, env=list(seed=seed)))
    p = plot_trends(tsdata) +
        coord_cartesian(ylim=c(-2, 2)) +
        labs(title=sprintf('Trends (Silh=%g)', round(silhouette, 1)), subtitle=deparse(subcall, width.cutoff=65) %>% paste(collapse='\n')); print(p)
}; dev.off()

# Trajectories
tmpcall = do.call(substitute, list(normcall))
pdf(file.path(SIM_PDF_DIR, 'normal' , sprintf('%s.pdf', deparse(tmpcall) %>% paste(collapse='')))); for(seed in 1:20) {
    cat(paste0('seed=', seed, '\n'))
    tsdata = eval(normcall, list(seed=seed))
    subcall = do.call(substitute, list(normcall, env=list(seed=seed)))
    p = plot_trajectories(tsdata, grouped=TRUE) +
        coord_cartesian(ylim=c(-2, 2)) +
        labs(title='Trajectories & trends', subtitle=deparse(subcall, width.cutoff=65) %>% paste(collapse='\n')); print(p)
}; dev.off()



# LOGNORM ####
lognormcall = quote(longdata_randquad_lognorm3(dataseed=seed, numtraj=100, numobs=10, noise=.01,
                                             re=RE_LNORM_LOW,
                                             int.sd=sqrt(.5), slo.sd=sqrt(.5), q.sd=sqrt(.5)))
# Trends
pdf(file.path(SIM_PDF_DIR, 'lognorm', 'trends.pdf')); for(seed in 1:20) {
    cat(paste0('seed=', seed, '\n'))
    tsdata = eval(lognormcall, list(seed=seed))
    subcall = do.call(substitute, list(lognormcall, env=list(seed=seed)))
    p = plot_trends(tsdata) +
        coord_cartesian(ylim=c(-2, 2)) +
        labs(title='Trends', subtitle=deparse(subcall, width.cutoff=65) %>% paste(collapse='\n')); print(p)
}; dev.off()

# Trajectories
tmpcall = do.call(substitute, list(lognormcall))
pdf(file.path(SIM_PDF_DIR, 'lognorm' , sprintf('%s.pdf', deparse(tmpcall) %>% paste(collapse='')))); for(seed in 1:20) {
    cat(paste0('seed=', seed, '\n'))
    tsdata = eval(lognormcall, list(seed=seed))
    subcall = do.call(substitute, list(lognormcall, env=list(seed=seed)))
    p = plot_trajectories(tsdata, grouped=TRUE) +
        coord_cartesian(ylim=c(-2, 2)) +
        labs(title='Trajectories & trends', subtitle=deparse(subcall, width.cutoff=65) %>% paste(collapse='\n')); print(p)
}; dev.off()

# PROPNOISE ####
propcall = quote(longdata_randquadG_propnoise(dataseed=seed, numgroups=3, numtraj=100, numobs=10, propnoise=.01,
                                               re=RE_NORM_LOW))
# Trajectories
tmpcall = do.call(substitute, list(propcall))
pdf(file.path(SIM_PDF_DIR, 'propnoise' , sprintf('%s.pdf', deparse(tmpcall) %>% paste(collapse='')))); for(seed in 1:20) {
    cat(paste0('seed=', seed, '\n'))
    tsdata = eval(propcall, list(seed=seed))
    subcall = do.call(substitute, list(propcall, env=list(seed=seed)))
    p = plot_trajectories(tsdata, grouped=TRUE) +
        coord_cartesian(ylim=c(-2, 4)) +
        labs(title='Trajectories & trends', subtitle=deparse(subcall, width.cutoff=65) %>% paste(collapse='\n')); print(p)
}; dev.off()
