### Call data from CPS ORG
df <- call.cps.org.data()

### Download and write economic forecast data from CBO
trends.df <- dwld_and_write_d1()    
### Select relevant indicators/variables
clean.df <- select_vals(trends.df)
### Compute growth rates for each indicator
growth.df <- compute_rates(clean.df)


### Get raw data on MW
state_mw_raw <- dwld_and_write_d2()
### Get clean data on MW
state_min_w <- clean_d2()

### Compute annual growth rate of wages from 2013 to 2016
wage.gr <- wage.gr.f()
### Compute annual growth rate of workers from 2013 to 2016
workers.gr <- workers.gr.f()
### Compute the gap between average wage growth and the growth of the lowest decile (assume by CBO)  
half.gap <- half.gap.f()
### 10 wage growth bins starting from lowest assumed value to 2 times the gap computed above
wage.gr.bins <- wage.gr.bins.f()
### Apply differential growth rate to wages (by deciles)
df <- wages.final.cps.org.f()

### Tag population of interest
df <- get.pop.int()
### Describing population of interest (weighted and unweighted)
table_1 <- cbind(f_table1(final_weights, "N"), f_table1(!is.na(final_weights), "Unweighted"))
### Table 2: summary statistics of wage variable (levels)
table_2 <- f_table2()
### Table 3: distribution of the population by wages levels
table_3 <- f_table3()
### Table 4: combination of tables 1 - 3
table_4 <- f_table4()

### Get data for treemap 1
universe.1 <- data_for_treemap1()
### Plot treemap with distribution of labor force in 2013
aux.1 <- treemap.1()

### Plot with distribution of wages under the status quo in 2013 and 2016
p <- two_dist()
print(p)

### Get data for treemap 2
universe.1 <- data_for_treemap2()
### Treemap with distribution of labor for and wages
aux.2 <- treemap.2()

### Compute the percentage of non-compliers, with and w/o adjusment for tipped workers
non.comp.stats <- f_non_comp_stats()
### Table 4 in DD (elements to compute N final)
table.n.final <- N.final.f()

### Elasticity from the literature  
eta.lit <- eta.lit.f(SA.eta.lit = param.eta.lit)
### Extrapolation factor used by CBO
factor.extrap <- factor.extrap.f()
### All remaining components required to compute effect on employment
stats3 <- final.other.comp()
### N final = gr * N_employed * % below min wage * compliers
n_final <- f_n_final()
### Adj Fact = 1/(% below min wage) * nominal variation/average variation
### adj_fact takes literal version from CBO report (last line: "Teen")
adj_fact <- f_adj_fact(SA.fract.minwage = param.fract.minwage,
                       SA.av.wage.var = param.av.wage.var,
                       frac.below.mw = table.n.final["% Salary below new MW ($P(\\hat{w} \\leq MW^{1})$)","Teen"]/100,
                       avg.wg.inc = stats3["$\\overline{\\%\\Delta w}$","Teen"]/100 )

### adj_fact2 takes methodological version from CBO report (last line: 1:2)
adj_fact2 <- f_adj_fact(SA.fract.minwage = param.fract.minwage,
                       SA.av.wage.var = param.av.wage.var,
                       frac.below.mw = table.n.final["% Salary below new MW ($P(\\hat{w} \\leq MW^{1})$)",1:2]/100,
                       avg.wg.inc = stats3["$\\overline{\\%\\Delta w}$",1:2]/100 )

### Elasticity = elasticities * Extrap * Adj Fact
elas_final <- f_elast(var_adj_fact = 4.5) ### adj_fact | 1, 4.5
### Employmemt effect = N final * Elasticity * Avg variation - other factors
delta.e1 <- f_delta_e()

### ASEC

### Call data from CPS ASEC March 2013
df <- call.cps.asec.data()
### Add sensitivity analysis parameters to hours, weeks, weights
df <- add.base.vars()
### Merge state min wage info
df <- add_minw(df)

### Pop of interest: employed & (not self employed or self incorp) & (wage variable not zero and not missing)
df <- f_pop_of_int()
### Population size, employed and salaried
table_5 <- f_table_5()
### Descriptive stats of earnings, hours, and weeks.
table_6 <- f_table_6()
### Compute hourly wages, replace negative vales withs 0's
df <- add.wage.var(df)
### Summary stats for year 2013
table_7 <- f_table_7()   

### Computing annualized growth rate for wage
wage.gr <- wage.gr.asec.f()
### For workers
workers.gr <- workers.gr.asec.f()  
### Half of the groth gap between 1st and 10th decile.
half.gap <- half.gap.asec.f()
### Compute 10 rates of wage growth
wage.gr.bins <- wage.gr.bins.asec.f()

### Assign popultation to deciles according to 'wage' variable
df <- add.wages.1()
### Here we adjust min wages to 2016 levels
df$wages.final <- wages.final.asec.org.f()

### Descriptives of wage and population size (analogous to table 4 in CPS ORG)
table_8 <- f_table_8()
### Histogram of wages below $20 for 2013 and 2016
p2 <- two_hist_asec()

### Create new wage (after inc in min wage) & apply ripple effects
df <- wage.ripple.f()

### Get the number of workers whose wage would bebow 10.10 in the status quo (in millions)
n_benes <- n_benes_f()
### Compute total wage increase (yearly, in billions) -without ripple effects and before destroying jobs-
wage_inc <- wage_inc_f()
### Total gain with ripple effects but without destroying any jobs  
wage_inc_with_ripple <- wage_inc_with_ripple_f()

### Non-compliance parameter
alpha.1 <- table.n.final["% of non compliers ($\\alpha_{1}$)", "Total"] * param.noncomp /100
### Assign old wages to a % of the population (non-compliers)
### Total gain with ripple effects but without destroying any jobs and accounting for non-compliance
set.seed(123)
df <- add.nocomp()
### Total gain with ripple effects but without destroying any jobs and accounting for non-compliance
wage.inc.with.ripple.non.comp <- wage.inc.with.ripple.non.comp_f()
### Number of workers with wages below new min, that are eligible to receive wage inc (before job loses)
N_benes_compliance_below_min <- N_benes_compliance_below_min_f()
### Number of workers with wages above the new min, that receive a wage increase due to ripples.
N_benes_compliance_above_min <- N_benes_compliance_above_min_f()
### Total number of beneficiares of wage rise
N_benes_compliance <- sum(df$hhwgt.2016[(df$wages.final !=df$new.wage.nocomp)], na.rm = TRUE)/1e6

### Assign half of old wages to a % of the population (2* delta.e1)
df <- job.killer()
### Compute total wage increase after ripple effects (yearly in billions)
wage.gain.total <- wage.gain.total_f()

### Anualized growth rate for non-wage income
non.wage.gr <- non.wage.gr.f()
### Adjust non-wage income, compute per cap incomes, and compute amount won/loss per person  
df <- all.income.f()
### Computing statistic of income variation
aux1 <- inc_ch_stats();
losses.pc <- aux1[["losses.pc"]]; losses <- aux1[["losses"]]; pop.dist <- aux1[["pop.dist"]]
### Compute balance losses and classify income by PL groups
df <- win.loss.f()
### Compute quintiles based on percapita income (status quo)
df <- add.quintiles()
### Compute variation by hhld - plot all the effects
final_fig1 <- final_fig1_f()
### Compute variation by hhld - plot all the effects in same units (average per group)
final_fig2 <- final_fig2_f()
### Compute variation by hhld - plot all the effects in same units (average per group) with quintiles instead of poverty lines.
final_fig3 <- final_fig3_f()

### Table proposal for gains and losses by income group and income variation
table_9 <- table_9_f()

### Formatting output to match CBO's: aggregate effects
output.template1.final <- output.template1.final_f()
### Formatting output to match CBO's: distributional effects
output.template2.final <- output.template2.final_f()
