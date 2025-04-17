#' @description This vig schedule uses logit vigging methods.
#' These functions help you vig and un-vig a probability in logit odds space.
#' @param fair_prob Fair probability (non-vigged).
#' @param vig_cents Base cent difference.
#' @return Logit vigged probability.
#' @examples
#' \donttest{
#'
#' logit_fair2vig(0.65, vig_cents = 20)
#' #> 0.6830971
#'
#' logit_vig2fair(0.6830971, vig_cents = 20)
#' #> 0.65
#' }
#' @rdname logit_vigging
#' @export
#' @keywords internal
logit_fair2vig = function(fair_prob, vig_cents = 12, strategy = "logit001") {
  strategy_params = get_logit_parameters(strategy)
  params = strategy_params$parameters
  inflection_fair_prob = strategy_params$inflection_fair_prob
  inflection_fair_prob2 = strategy_params$inflection_fair_prob2
  
  k = params[11]
  
  a = if_else(
    fair_prob >= 0.5,
    params[1] + (k * vig_cents + (1 - k) * 8) * params[2],
    params[5] + (k * vig_cents + (1 - k) * 8) * params[6]
  )
  b = if_else(
    fair_prob >= 0.5,
    params[3] + (k * vig_cents + (1 - k) * 8) * params[4],
    params[7] + (k * vig_cents + (1 - k) * 8) * params[8]
  )
  c = boot::logit(0.5 + get_base_vig(vig_cents)) -
    (a * (boot::logit(0.5 + 0.0003))^2 + b * boot::logit(0.5 + 0.00001))
  
  result = if_else(
    fair_prob >= 0.5,
    boot::inv.logit(
      a * (boot::logit(fair_prob + 0.00001))^2 + b * boot::logit(fair_prob +
                                                                   0.00001) + params[9] * pmin(
                                                                     boot::logit(inflection_fair_prob + 0.00001),
                                                                     boot::logit(fair_prob + 0.00001)
                                                                   ) + c
    ),
    boot::inv.logit(
      a * (boot::logit(fair_prob + 0.00001))^2 + b * boot::logit(fair_prob +
                                                                   0.00001) - params[10] * pmax(
                                                                     boot::logit(inflection_fair_prob2 + 0.00001),
                                                                     boot::logit(fair_prob + 0.00001)
                                                                   ) + c
    )
  )
  
  result
}

#' Convert Logit Probablity to Fair Probablity
#' @param vigged_prob Probability (vigged)
#' @rdname logit_vigging
#' @keywords internal
#' @export
logit_vig2fair = function(vigged_prob, vig_cents = 12, strategy = "logit001") {
  strategy = get_logit_parameters(strategy)
  params = strategy$parameters
  inflection_fair_prob = strategy$inflection_fair_prob
  inflection_fair_prob2 = strategy$inflection_fair_prob2
  
  k = params[11]
  
  a = if_else(
    vigged_prob >= 0.5 + get_base_vig(vig_cents),
    params[1] + (k * vig_cents + (1 - k) * 8) * params[2],
    params[5] + (k * vig_cents + (1 - k) * 8) * params[6]
  )
  b = if_else(
    vigged_prob >= 0.5 + get_base_vig(vig_cents),
    params[3] + (k * vig_cents + (1 - k) * 8) * params[4],
    params[7] + (k * vig_cents + (1 - k) * 8) * params[8]
  )
  c = boot::logit(0.5 + get_base_vig(vig_cents)) -
    (a * (boot::logit(0.5 + 0.00001))^2 + b * boot::logit(0.5 + 0.00001))
  
  b = if_else(
    vigged_prob >= 0.5 + get_base_vig(vig_cents),
    if_else(
      vigged_prob < logit_fair2vig(inflection_fair_prob, vig_cents),
      b + params[9], b
    ),
    if_else(
      vigged_prob >= logit_fair2vig(inflection_fair_prob2, vig_cents),
      b - params[10], b
    )
  )
  
  c = if_else(
    vigged_prob >= 0.5 + get_base_vig(vig_cents),
    if_else(
      vigged_prob >= logit_fair2vig(inflection_fair_prob, vig_cents),
      c + params[9] * boot::logit(inflection_fair_prob + 1e-05), c
    ),
    if_else(
      vigged_prob < logit_fair2vig(inflection_fair_prob2, vig_cents),
      c - params[10] * boot::logit(inflection_fair_prob2 + 1e-05), c
    )
  )
  
  y = boot::logit(vigged_prob)
  x = (sqrt(b^2 - 4 * a * (c - y)) - b) / (2 * a)
  
  result = boot::inv.logit(x) - 0.00001
  
  result
}

# Helpers -----------------------------------------------------------------

# How much vig on a 0.5 fair prob line.
get_base_vig = function(vig_cents) {
  (100 + vig_cents / 2) / (200 + vig_cents / 2) - 0.5
}

# Parameters defining how much vig to apply on 0.5/0.5 fair prob line.
get_logit_parameters = function(strategy) {
  inflection_fair_prob = 0.54
  inflection_fair_prob2 = 0.46
  logit_001 =  c(-0.0042, 0.0031, 0.9966, 0.0027, -0.0042, 0.0031, 0.9966, 0.0027, 0, 0, 1)
  logit_002 =  c(-0.0042, 0.0027, 0.9966, 0.0030, -0.0042, 0.0028, 0.9966, 0.0027, 0, 0, 0.75)
  logit_003 =  c(-0.004267786,  0.001011020,  0.996610340,  0.003231799, -0.004214178,  0.002450558,  0.996611782,  0.002989520, 0, 0, 0.75)
  logit_004 =  c(-0.004144269,  0.002184394,  0.996283973,  0.000131300, -0.004259463,  0.001986707,  0.996360759,  0.000604800, 0, 0, 0.75)
  logit_005 =  c(-0.004183693,  0.001218780,  0.996292250,  0.000334086, -0.004272803,  0.001331089,  0.996363507,  0.000589097, 0, 0, 0.75)
  # Inverse of logit_003
  logit_006 = c(-0.004113346, 0.002572101, 0.996102878, -0.002369656, -0.004372242, 0.000659731, 0.996112412, -0.002517895, 0, 0, 0.75 )
  logit_100 = c(-0.004, 0.0030, 0.997, 0.0040, -0.0012, 0.0026, 0.998, 0.0010, 0, 0, 1)
  logit_101 =  c( 0.000, 0.0020, 1, 0.0000,  0.0000, 0.0030, 1, 0.0000, 0, 0, 1)
  logit_102 =  c(-0.004, 0.0030, 0.997, 0.0044, -0.0012, 0.0018, 0.998, 0.0026, 0.07, 0.07, 1)
  logit_103 =  c(-0.004, 0.0032, 0.997, 0.0030, -0.0012, 0.0015, 0.998, 0.0013, 0.07, 0.07, 1)
  logit_104 = c(-0.001401053, 0.001006366, 0.998369239,  0.003197390, -0.004123932,  0.002772688,  0.996690523, 0.003515889, 0.07, 0.07, 0.75)
  logit_105 = c(-0.001269485, 0.002256212, 0.998029800, -0.000027269, -0.004169340,  0.002342754,  0.996392643, 0.000684890, 0.07, 0.07, 0.75)
  
  parameters = switch(
    tolower(strategy),
    "logit001" = logit_001,
    "logit002" = logit_002,
    "logit003" = logit_003,
    "logit004" = logit_004,
    "logit005" = logit_005,
    "logit006" = logit_006,
    "logit100" = logit_100,
    "logit101" = logit_101,
    "logit102" = logit_102,
    "logit103" = logit_103,
    "logit104" = logit_104,
    "logit105" = logit_105
  )
  
  list(
    parameters = parameters,
    inflection_fair_prob = inflection_fair_prob,
    inflection_fair_prob2 = inflection_fair_prob2
  )
}

logit_strategies = list(
  "default" = "logit_001",
  "logit001" = "logit_001",
  "logit002" = "logit_002",
  "logit003" = "logit_003",
  "logit004" = "logit_004",
  "logit005" = "logit_005",
  "logit100" = "logit_100",
  "logit101" = "logit_101",
  "logit102" = "logit_102",
  "logit103" = "logit_103",
  "logit104" = "logit_104",
  "logit105" = "logit_105"
)
