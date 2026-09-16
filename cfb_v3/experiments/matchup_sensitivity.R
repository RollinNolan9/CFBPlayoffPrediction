run_matchup_sensitivity <- function(project,prepared_dir) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(prepared_dir)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_synthetic_sensitivity"),"")
  data <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  warmup <- data$season<2022
  center <- mean(data$mx_style_preference[warmup])
  scale <- sd(data$mx_style_preference[warmup])
  if(!is.finite(scale) || scale<=0)stop("Synthetic signal has no warm-up variation.")
  z <- (data$mx_style_preference-center)/scale
  blocks <- split(seq_len(nrow(data)),paste(data$season,data$feature_week_start,data$is_cfp))
  rows <- list()
  for(seed in 947501:947516) {
    set.seed(seed); noise <- data$market_residual
    for(i in blocks)if(length(i)>1)noise[i] <- data$market_residual[sample(i,length(i),replace=FALSE)]
    for(effect in c(0,1,2,4)) {
      message("Synthetic sensitivity seed ",seed," / injected points per warm-up SD ",effect)
      synthetic <- data
      synthetic$market_residual <- noise+effect*z
      synthetic$margin <- synthetic$market_margin+synthetic$market_residual
      forecasts <- matchup_walk(synthetic)$predictions
      for(name in c("external_market","linear_components","style")) {
        x <- forecasts[forecasts$model==name, ]
        rows[[paste(seed,effect,name)]] <- data.frame(seed=seed,injected_points_per_warmup_sd=effect,
          model=name,games=nrow(x),synthetic_mae=mean(abs(x$expected_margin-x$margin)),
          scope="SIMULATED_OUTCOMES_NOT_REAL_BACKTEST")
      }
    }
    write.csv(do.call(rbind,rows),file.path(output,"synthetic_draws.csv"),row.names=FALSE)
  }
  draws <- do.call(rbind,rows)
  paired <- list()
  for(effect in c(0,1,2,4))for(control in c("external_market","linear_components")) {
    x <- draws[draws$injected_points_per_warmup_sd==effect, ]
    style <- x[x$model=="style", ]; base <- x[x$model==control, ]
    gain <- base$synthetic_mae[match(style$seed,base$seed)]-style$synthetic_mae
    paired[[paste(effect,control)]] <- data.frame(injected_points_per_warmup_sd=effect,
      control=control,draws=length(gain),mean_style_mae_improvement=mean(gain),
      min_improvement=min(gain),max_improvement=max(gain),fraction_draws_style_improves=mean(gain>0))
  }
  write.csv(do.call(rbind,paired),file.path(output,"synthetic_comparison.csv"),row.names=FALSE)
  saveRDS(list(prepared_dir=prepared_dir,created=external_stamp(),seeds=947501:947516,
    warmup_center=center,warmup_scale=scale,
    limits="Synthetic method sensitivity only. Sixteen draws are not a calibrated power estimate. Source residual permutations break actual team associations. No real selections or predictions generated."),
    file.path(output,"provenance.rds"))
  for(file in c("matchup_sensitivity.R","matchup_research.R","MATCHUP_PROTOCOL.md"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during synthetic test.")
  external_seal(output)
  output
}
