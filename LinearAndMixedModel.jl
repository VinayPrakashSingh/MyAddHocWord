sort!(model_data_final,cols=[:STORE])
TEST_SUMMARY=DataFrame()
TRAIN_SUMMARY=DataFrame()
df_corr=DataFrame()
a=0
model_data_final[:TEST_CONTROL]=map((x)-> ifelse(x ==2,1,0),model_data_final[:TEST_CONTROL]);
model_data_final[:PERIOD]=map((x)-> ifelse(x ==2,1,0),model_data_final[:PERIOD]);
relevel(model_data_final[:TEST_CONTROL],0)
relevel(model_data_final[:PERIOD],0)
levels(model_data_final[:TEST_CONTROL])
levels(model_data_final[:PERIOD])

for i in 1:100
	
	df_tot_weeks=DataFrame()
	df_tot_weeks=unique(model_data_final[:WEEK])
	test=DataFrame()
	train=DataFrame()
	for str in unique(model_data_final[:STORE])
		#str=50404
		n=floor(length(unique(model_data_final[model_data_final[:STORE].==str,:WEEK]))*0.7)
		df_weeks=DataFrame()
		df_weeks[:WEEK]=sample(model_data_final[model_data_final[:STORE].==str,:WEEK],convert(Int64,n),replace=false)
		train=vcat(train,join(model_data_final[model_data_final[:STORE].==str,:],df_weeks,on=:WEEK))
		test=vcat(test,join(model_data_final[model_data_final[:STORE].==str,:],df_weeks,on=:WEEK,kind=:anti))
	end
    
	s=by(train,[:STORE,:PERIOD],train->length(unique(train[:WEEK])))
	s[:ITR]=i
	TRAIN_SUMMARY=vcat(TRAIN_SUMMARY,s)
	
	s=by(test,[:STORE,:PERIOD],test->length(unique(test[:WEEK])))
	s[:ITR]=i
	TEST_SUMMARY=vcat(TEST_SUMMARY,s)
	
    ########Fixed effect	
	formula=eval(parse(string(string(cfg["MEASURE_ACV"]),string(cfg["PID_CNT"]), "~ TEST_CONTROL * PERIOD ")))
	#model_full = fit!(lm(formula, train, weights = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))])))
	#model_full = glm(formula, model_data_final,Normal(),IdentityLink(), wts  = convert(Array{Float64,1}, model_data_final[Symbol("ACV",string(cfg["PID_CNT"]))]))
	model_full = glm(formula, train,Normal(),IdentityLink(), wts  = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))]))
	#model_full = lm(formula, train)
    #model_full = lm(formula, model_data_final)
	FixedEff=DataFrame(transpose(coef(model_full)));names!(FixedEff,[:Intercept,:TEST_CONTROL ,:PERIOD,:TEST_CONTROL_PERIOD])

	predictedDOLACV=StatsBase.predict(model_full,train);
	#predictedDOLACV=StatsBase.predict(model_full,model_data_final[model_data_final[:STORE].==33423,:]);
	a=cor(train[:DOLACV1],predictedDOLACV)
	#cor(model_data_final[:DOLACV1],predictedDOLACV)
	predictedDOLACV=StatsBase.predict(model_full,test);
	a=vcat(a,cor(test[:DOLACV1],predictedDOLACV))

    ########Random effect
	
	formula=eval(parse(string(string(cfg["MEASURE_ACV"]),string(cfg["PID_CNT"]), "~ TEST_CONTROL * PERIOD + (1|STORE)")))
	model_full_1 = fit!(lmm(formula, train, weights = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))])))
    #model_full_1 = fit!(lmm(formula, model_data_final, weights = convert(Array{Float64,1}, model_data_final[Symbol("ACV",string(cfg["PID_CNT"]))])))
	FixedEff=DataFrame(transpose(coef(model_full_1)));names!(FixedEff,[:Intercept,:TEST_CONTROL ,:PERIOD,:TEST_CONTROL_PERIOD])
	RanEff =DataFrame()
	RanEff[:STORE] =unique(model_full_1.mf.df[:STORE])
	RanEff = hcat(RanEff,DataFrame(transpose(ranef(model_full_1)[1])));names!(RanEff,[:STORE,:RANEFF])

	train1=deepcopy(train[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1]])
	#train1=deepcopy(model_data_final[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1]])
	#train1[:TEST_CONTROL]=map((x)-> ifelse(x ==2,1,0),train1[:TEST_CONTROL]);
	#train1[:PERIOD]=map((x)-> ifelse(x ==2,1,0),train1[:PERIOD]);
	train1=join(train1,FixedEff, kind = :cross)
	train1=join(train1,RanEff,on = :STORE, kind = :inner)
	train1[:predicted] = train1[:Intercept].+ (train1[:TEST_CONTROL_1].* train1[:TEST_CONTROL]).+ (train1[:PERIOD_1].* train1[:PERIOD]) .+ (train1[:TEST_CONTROL_PERIOD].* train1[:TEST_CONTROL].* train1[:PERIOD]) .+ train1[:RANEFF]
	a=vcat(a,cor(train1[:DOLACV1],train1[:predicted]))
	writetable("RanEff_ranef.csv",RanEff,header=true);
	#train1[train1[:STORE].==33423,:]


	test1=deepcopy(test[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1]])
	#test1[test1[:test1_CONTROL].==2,:TEST_CONTROL]=0
	#test1[:PERIOD]=map((x)-> ifelse(x ==2,1,0),test1[:PERIOD]);
	test1=join(test1,FixedEff, kind = :cross)
	test1=join(test1,RanEff,on = :STORE, kind = :inner)
	test1[:predicted] = test1[:Intercept].+ (test1[:TEST_CONTROL_1].* test1[:TEST_CONTROL]).+ (test1[:PERIOD_1].* test1[:PERIOD]) .+ (test1[:TEST_CONTROL_PERIOD].* test1[:TEST_CONTROL].* test1[:PERIOD]) .+ test1[:RANEFF]
	a=vcat(a,cor(test1[:DOLACV1],test1[:predicted]))
	
	df_corr=vcat(df_corr,DataFrame(transpose(a)))

	########Random slope
	formula=eval(parse(string(string(cfg["MEASURE_ACV"]),string(cfg["PID_CNT"]), "~ TEST_CONTROL * PERIOD + (0+",cfg["MEASURE_ACV"],"0|STORE)")))
	model_full_1 = fit!(lmm(formula, train, weights = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))])))
	#model_full_1 = fit!(lmm(formula, model_data_final, weights = convert(Array{Float64,1}, model_data_final[Symbol("ACV",string(cfg["PID_CNT"]))])))

	FixedEff=DataFrame(transpose(coef(model_full_1)));names!(FixedEff,[:Intercept,:TEST_CONTROL ,:PERIOD,:TEST_CONTROL_PERIOD])
	RanEff =DataFrame()
	RanEff[:STORE] =unique(model_full_1.mf.df[:STORE])
	RanEff = hcat(RanEff,DataFrame(transpose(ranef(model_full_1)[1])));names!(RanEff,[:STORE,:SLOPE])

	train1=deepcopy(train[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]])
	#train1=model_data_final[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]]
	#train1[:TEST_CONTROL]=map((x)-> ifelse(x ==2,1,0),train1[:TEST_CONTROL]);
	#train1[:PERIOD]=map((x)-> ifelse(x ==2,1,0),train1[:PERIOD]);
	train1=join(train1,FixedEff, kind = :cross)
	train1=join(train1,RanEff,on = :STORE, kind = :inner)
	train1[:predicted] = train1[:Intercept].+ (train1[:TEST_CONTROL_1].* train1[:TEST_CONTROL]).+ (train1[:PERIOD_1].* train1[:PERIOD]) .+ (train1[:TEST_CONTROL_PERIOD].* train1[:TEST_CONTROL].* train1[:PERIOD]) .+ train1[:SLOPE] .* train1[:DOLACV0]
	a=vcat(a,cor(train1[:DOLACV1],train1[:predicted]))
	writetable("RanEff_slope.csv",RanEff,header=true);
	#train1[train1[:STORE].==33423,:]
    

	test1=deepcopy(test[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]])
	#test[:TEST_CONTROL]=map((x)-> ifelse(x ==2,1,0),test[:TEST_CONTROL]);
	#test[:PERIOD]=map((x)-> ifelse(x ==2,1,0),test[:PERIOD]);
	test1=join(test1,FixedEff, kind = :cross)
	test1=join(test1,RanEff,on = :STORE, kind = :inner)
	test1[:predicted] = test1[:Intercept].+ (test1[:TEST_CONTROL_1].* test1[:TEST_CONTROL]).+ (test1[:PERIOD_1].* test1[:PERIOD]) .+ (test1[:TEST_CONTROL_PERIOD].* test1[:TEST_CONTROL].* test1[:PERIOD]) .+ test1[:SLOPE] .* test1[:DOLACV0]
	a=vcat(a,cor(test1[:DOLACV1],test1[:predicted]))
	
	df_corr=vcat(DataFrame(transpose(a)))
	
	########Random Intercept and Random slope
	formula=eval(parse(string(string(cfg["MEASURE_ACV"]),string(cfg["PID_CNT"]), "~ TEST_CONTROL * PERIOD + (1+",cfg["MEASURE_ACV"],"0|STORE)")))
	model_full_1 = fit!(lmm(formula, train, weights = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))])))
	#model_full_1 = fit!(lmm(formula, model_data_final, weights = convert(Array{Float64,1}, model_data_final[Symbol("ACV",string(cfg["PID_CNT"]))])))

	FixedEff=DataFrame(transpose(coef(model_full_1)));names!(FixedEff,[:Intercept,:TEST_CONTROL ,:PERIOD,:TEST_CONTROL_PERIOD])
	RanEff =DataFrame()
	RanEff[:STORE] =unique(model_full_1.mf.df[:STORE])
	RanEff = hcat(RanEff,DataFrame(transpose(ranef(model_full_1)[1])));names!(RanEff,[:STORE,:RANEFF,:SLOPE])

	train1=deepcopy(train[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]])
	#train1=model_data_final[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]]
	train1[:TEST_CONTROL]=map((x)-> ifelse(x ==2,1,0),train1[:TEST_CONTROL]);
	train1[:PERIOD]=map((x)-> ifelse(x ==2,1,0),train1[:PERIOD]);
	train1=join(train1,FixedEff, kind = :cross)
	train1=join(train1,RanEff,on = :STORE, kind = :inner)
	train1[:predicted] = train1[:Intercept].+ (train1[:TEST_CONTROL_1].* train1[:TEST_CONTROL]).+ (train1[:PERIOD_1].* train1[:PERIOD]) .+ (train1[:TEST_CONTROL_PERIOD].* train1[:TEST_CONTROL].* train1[:PERIOD]) .+ train1[:RANEFF] .+ train1[:SLOPE].* train1[:DOLACV0]
	a=vcat(a,cor(train1[:DOLACV1],train1[:predicted]))
	writetable("RanEff_int_slope.csv",RanEff,header=true);
	#train1[train1[:STORE].==33423,:]


	test1=deepcopy(test[:,[:TEST_CONTROL,:PERIOD,:STORE,:DOLACV1,:DOLACV0]])
	#test1[test1[:TEST_CONTROL].==2,:TEST_CONTROL]=0
	#test1[:PERIOD]=map((x)-> ifelse(x ==2,1,0),test1[:PERIOD]);
	test1=join(test1,FixedEff, kind = :cross)
	test1=join(test1,RanEff,on = :STORE, kind = :inner)
	test1[:predicted] = test1[:Intercept].+ (test1[:TEST_CONTROL_1].* test1[:TEST_CONTROL]).+ (test1[:PERIOD_1].* test1[:PERIOD]) .+ (test1[:TEST_CONTROL_PERIOD].* test1[:TEST_CONTROL].* test1[:PERIOD]) .+ test1[:RANEFF]
	a=vcat(a,cor(test1[:DOLACV1],test1[:predicted]))
	
	df_corr=vcat(DataFrame(transpose(a)))
	
end


writetable(root*"/TRAIN_SUMMARY.csv",TRAIN_SUMMARY,header = true);
writetable(root*"/TEST_SUMMARY.csv",TEST_SUMMARY,header = true);
writetable(root*"/df_corr.csv",df_corr,header = true);
