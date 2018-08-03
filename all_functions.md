# Call data from CPS ORG
df <- call.cps.org.data()

# Download and write economic forecast data from CBO
trends.df <- dwld_and_write_d1()    
# Select relevant indicators/variables
clean.df <- select_vals(trends.df)
# Compute growth rates for each indicator
growth.df <- compute_rates(clean.df)


# Get raw data on MW
state_mw_raw <- dwld_and_write_d2()
# Get clean data on MW
state_min_w <- clean_d2()

# Compute annual growth rate of wages from 2013 to 2016
wage.gr <- wage.gr.f()
# Compute annual growth rate of workers from 2013 to 2016
workers.gr <- workers.gr.f()
# Compute the gap between average wage growth and the growth of the lowest decile (assume by CBO)  
half.gap <- half.gap.f()
# 10 wage growth bins starting from lowest assumed value to 2 times the gap computed above
wage.gr.bins <- wage.gr.bins.f()
# Apply differential growth rate to wages (by deciles)
df <- wages.final.cps.org.f()

# Tag population of interest
df <- get.pop.int()
# Describing population of interest (weighted and unweighted)
table_1 <- cbind(f_table1(final_weights, "N"), f_table1(!is.na(final_weights), "Unweighted"))
# Table 2: summary statistics of wage variable (levels)
table_2 <- f_table2()
# Table 3: distribution of the population by wages levels
table_3 <- f_table3()
# Table 4: combination of tables 1 - 3
table_4 <- f_table4()

# Get data for treemap 1
universe.1 <- data_for_treemap1()
# Plot treemap with distribution of labor force in 2013
aux.1 <- treemap.1()

# Plot with distribution of wages under the status quo in 2013 and 2016
p <- two_dist()
print(p)

# Get data for treemap 2
universe.1 <- data_for_treemap2()
# Treemap with distribution of labor for and wages
aux.2 <- treemap.2()

# Compute the percentage of non-compliers, with and w/o adjusment for tipped workers
non.comp.stats <- f_non_comp_stats()
# Table 4 in DD (elements to compute N final)
table.n.final <- N.final.f()

# Elasticity from the literature  
eta.lit <- eta.lit.f(SA.eta.lit = param.eta.lit)
# Extrapolation factor used by CBO
factor.extrap <- factor.extrap.f()
# All remaining components required to compute effect on employment
stats3 <- final.other.comp()
# N final = gr * N_employed * % below min wage * compliers
n_final <- f_n_final()
# Adj Fact = 1/(% below min wage) * nominal variation/average variation
# adj_fact takes literal version from CBO report (last line: "Teen")
adj_fact <- f_adj_fact(SA.fract.minwage = param.fract.minwage,
                       SA.av.wage.var = param.av.wage.var,
                       frac.below.mw = table.n.final["% Salary below new MW ($P(\\hat{w} \\leq MW^{1})$)","Teen"]/100,
                       avg.wg.inc = stats3["$\\overline{\\%\\Delta w}$","Teen"]/100 )

# adj_fact2 takes methodological version from CBO report (last line: 1:2)
adj_fact2 <- f_adj_fact(SA.fract.minwage = param.fract.minwage,
                       SA.av.wage.var = param.av.wage.var,
                       frac.below.mw = table.n.final["% Salary below new MW ($P(\\hat{w} \\leq MW^{1})$)",1:2]/100,
                       avg.wg.inc = stats3["$\\overline{\\%\\Delta w}$",1:2]/100 )

# Elasticity = elasticities * Extrap * Adj Fact
elas_final <- f_elast(var_adj_fact = 4.5) # adj_fact | 1, 4.5
# Employmemt effect = N final * Elasticity * Avg variation - other factors
delta.e1 <- f_delta_e()


#ASEC

# Call data from CPS ASEC March 2013
df <- call.cps.asec.data()
# Add sensitivity analysis parameters to hours, weeks, weights
df <- add.base.vars()
# Merge state min wage info
df <- add_minw(df)

df <- f_pop_of_int()
table_5 <- f_table_5()
table_6 <- f_table_6()
df <- add.wage.var(df)
table_7 <- f_table_7()   


wage.gr <- wage.gr.asec.f()
workers.gr <- workers.gr.asec.f()  
half.gap <- half.gap.asec.f()
wage.gr.bins <- wage.gr.bins.asec.f()

df <- add.wages.1()
df$wages.final <- wages.final.asec.org.f()

table_8 <- f_table_8()


df <- wage.ripple.f()

# Get the number of workers whose wage would bebow 10.10 in the status quo (in millions)
N_benes <- sum(df$hhwgt.2016[df$wages.final <= 10.10 & df$pop_of_int==1], na.rm = TRUE)/1e6

# Compute total wage increase (yearly, in billions) -without ripple effects and before destroying jobs-
wage.inc <- with(df[df$below_min == 1 & df$pop_of_int==1, ],
      sum((10.10 - wages.final) * hhwgt.2016 * hrslyr * wkslyr , na.rm = TRUE) ) / 1e9

# Total gain with ripple effects but without destroying any jobs  
wage.inc.with.ripple <- df %>%
      with( sum((new.wage - wages.final) *  
                  hhwgt.2016 * hrslyr *
                  wkslyr , na.rm = TRUE) ) / 1e9

# Apply ripple effects
alpha.1 <- stats2["% of non compliers ($\\alpha_{1}$)", "Total"] * param.noncomp /100
set.seed(123)
df <- add.nocomp()

# Total gain with ripple effects but without destroying any jobs and accounting for non-compliance
wage.inc.with.ripple.non.comp <- df %>%
    #filter(below_min == 0)  %>%
      with( sum((new.wage.nocomp - wages.final) *  hhwgt.2016 * hrslyr * wkslyr , na.rm = TRUE) ) / 1e9

# Number of workers with wages below new min, that are eligible to receive wage inc (before job loses)
N_benes_compliance_below_min <-  sum(df$hhwgt.2016[(df$wages.final !=
                                                      df$new.wage.nocomp) & df$below_min==1],
                                     na.rm = TRUE)/1e6
#
N_benes_compliance_above_min <-  sum(df$hhwgt.2016[(df$wages.final !=
                                                      df$new.wage.nocomp) & df$below_min==0],
                                     na.rm = TRUE)/1e6
N_benes_compliance<- sum(df$hhwgt.2016[(df$wages.final !=
                                          df$new.wage.nocomp)],
                         na.rm = TRUE)/1e6

df <- job.killer()

wage.gain.total <-  df %>%
      summarise( "Total wage gain" =  sum( (new.wage.final - wages.final) *
                   hhwgt.2016 * hrslyr * wkslyr , na.rm = TRUE) / 1e9 ,
                "Total wage loss" = sum( (wages.final - cut.wage) *
                   hhwgt.2016 * hrslyr * wkslyr , na.rm = TRUE) / 1e9,
                "Total gain before JD" =  sum( (new.wage.final - cut.wage) *
                   hhwgt.2016 * hrslyr * wkslyr , na.rm = TRUE) / 1e9
                )  

non.wage.gr <- non.wage.gr.f()
df <- all.income.f()


losses <- with(df, sum(winners * hhwgt.2016) -
                 param.factor.1 * sum(losers * hhwgt.2016) )  - param.net.benef
pop.dist <- wtd.table( with(df, findInterval(x = sq.inc.pc,
                                             vec = c(-Inf,11740, 6*11740, Inf)) ) ,
                       weights = df$hhwgt.2016 )$sum.of.weights

losses.pc  <- as.numeric(losses) * param.dist.loss / pop.dist

df <- win.loss.f()    
