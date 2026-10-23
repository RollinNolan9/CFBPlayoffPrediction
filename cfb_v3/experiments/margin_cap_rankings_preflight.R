for (m in c("config","store","features","coaches","adapters","models","coach_migration","historical_data","preseason","bridge","workflow","live_data")) source(file.path("cfb_v2",paste0(m,".R")))
b <- readRDS("cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z_model.rds")
cat("Bundle fields:",names(b),"\n")
cat("Foundation rows/seasons/lambda:",b$model$foundation$trained_rows,b$model$foundation$trained_seasons,b$model$foundation$ridge$lambda,"\n")
cat("Foundation features:",b$model$foundation$features,"\n")
cat("Coach-related bundle fields:\n")
str(b[grep("coach|lambda|validation",names(b))],max.level=2)
cat("Config weight settings:\n")
str(b$config$weights)
raw <- read.csv("cfb_v3/inbox/training_games.csv")
cat("Raw row counts byseason:\n");print(table(raw$season))
prepared <- readRDS("cfb_v3/output/experiments/margin_caps/20260927T211431Z/uncapped_training_inputs.rds")
cat("Prepared rows:",nrow(prepared$data),"weight range",range(prepared$weights),"\n")
for(share in c(.65,.70)) {
 d <- prepared$data
 d$coach_rating_diff <- d[[if(share==.65) "coach_rating_65_35_diff" else "coach_rating_70_30_diff"]]
 for(norm in c(FALSE,TRUE)) {
  w <- prepared$weights; if(norm) w <- w/max(w)
  fit <- fit_weighted_ridge(d,"margin",b$model$foundation$features,w,b$model$foundation$ridge$lambda)
  cat("share",share,"normalized",norm,"coeferror",max(abs(fit$coefficients-b$model$foundation$ridge$coefficients)),"\n")
 }
}
