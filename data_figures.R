initsim()
library(latex2exp)

plot_example_ribbon = function(tsdata) {
    dt_trends = tsdata_trends(tsdata) %>% copy %>%
        .[, Value.min := Value - RE_NORM_LOW * 2] %>%
        .[, Value.max := Value + RE_NORM_LOW * 2]

    ggplot(dt_trends, aes(x=Time, y=Value, ymin=Value.min, ymax=Value.max, group=Group, fill=Group)) +
        geom_ribbon(alpha=.5) +
        geom_line(size=2) +
        scale_color_grey(guide=FALSE) +
        scale_fill_grey(guide=FALSE) +
        labs(x='t', y=expression(y[t])) +
        theme(axis.title.y=element_text(vjust=.5, angle=0))
}

plot_example_traj = function(tsdata) {
    dt_trends = tsdata_trends(tsdata) %>% copy
    ggplot() +
        geom_line(data=tsdata, aes(x=Time, y=Value, group=Id, color=Group), size=.1) +
        scale_color_manual(values=c('#222222', '#888888', '#DDDDDD'), guide=FALSE) +
        geom_line(data=dt_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='white', size=2.2) +
        geom_line(data=dt_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='black', size=1.5) +
        labs(x='t', y=expression(y[t])) +
        theme(axis.title.y=element_text(vjust=.5, angle=0))
}


#200: 4, 10, 11
#50: 2
#100: 11, 6, 37, 40
# RE ####
# low
p = (longdata_randquadG(dataseed=40, numtraj=100, numobs=10, numgroups=3, re=RE_NORM_LOW, noise=.01) %>% plot_example_traj) + coord_cartesian(ylim=c(-2, 2)); print(p)
ggsave(filename=file.path(SIM_FIG_DIR, 'data_normal_reLow.pdf'), plot=p, width=9, height=7, units='cm')
# high
p = (longdata_randquadG(dataseed=40, numtraj=100, numobs=10, numgroups=3, re=RE_NORM_HIGH, noise=.01) %>% plot_example_traj) + coord_cartesian(ylim=c(-2, 2)); print(p)
ggsave(filename=file.path(SIM_FIG_DIR, 'data_normal_reHigh.pdf'), plot=p, width=9, height=7, units='cm')

dtr1 = longdata_randquadG(dataseed=40, numtraj=100, numobs=10, numgroups=3, re=RE_NORM_LOW, noise=.01)
dtr2 = longdata_randquadG(dataseed=40, numtraj=100, numobs=10, numgroups=3, re=RE_NORM_HIGH, noise=.1)
dtr_trends = tsdata_trends(dtr1)
dtx = rbind(dtr1, dtr2, idcol='Re') %>%
    .[, Re := factor(Re, labels=c(TeX('$\\sigma_{\\zeta} = 0.1$,   $\\sigma_{\\epsilon} = 0.01$'),
                                  TeX('$\\sigma_{\\zeta} = 0.3$,   $\\sigma_{\\epsilon} = 0.1$')))]

p = ggplot() +
    geom_line(data=dtx, aes(x=Time, y=Value, group=Id, color=Group), size=.1) +
    scale_x_continuous(breaks=seq(0, 1, by=.2), labels=prettyNum) +
    scale_color_manual(values=c('#222222', '#888888', '#DDDDDD'), guide=FALSE) +
    geom_line(data=dtr_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='white', size=2) +
    geom_line(data=dtr_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='black', size=1.5) +
    labs(x=expression(italic(t)), y=expression(italic(y[t]))) +
    theme(axis.title.y=element_text(vjust=.5, angle=0)) +
    facet_grid(~Re, labeller=label_parsed); print(p)
ggsave(filename=file.path(SIM_FIG_DIR, 'data_normal_re.pdf'), plot=p, width=9, height=7, units='cm')

# Propnoise ####
longdata_randquadG_propnoise(dataseed=31, numtraj=50, numgroups=2, re=RE_NORM_LOW, numobs=25, propnoise=0.05) %>% plot_example_traj
#7, 11, 16, 18, 28!, 29!, 31

dtp = longdata_randquadG_propnoise(dataseed=31, numtraj=50, numgroups=2, re=RE_NORM_LOW, numobs=25, propnoise=0.05)
dtp_trends = tsdata_trends(dtp)
p = ggplot() +
    geom_line(data=dtp, aes(x=Time, y=Value, group=Id, color=Group), size=.1) +
    scale_x_continuous(breaks=seq(0, 1, by=.2), labels=prettyNum) +
    scale_color_manual(values=c('#777777', '#BBBBBB'), guide=FALSE) +
    geom_line(data=dtp_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='white', size=2) +
    geom_line(data=dtp_trends, aes(x=Time, y=Value, group=Group), linetype='solid', color='black', size=1.5) +
    labs(x=expression(italic(t)), y=expression(italic(y[t]))) +
    theme(axis.title.y=element_text(vjust=.5, angle=0)); print(p)
ggsave(filename=file.path(SIM_FIG_DIR, 'data_propnoise.pdf'), plot=p, width=9, height=6, units='cm')
