mround <- function(x,base){
  base*round(x/base)
}

translate_to_tz = function(x, from_tz = as.character(), to_tz = as.character()) {
  
  x = lubridate::force_tz(x, tzone = from_tz)
  x = lubridate::with_tz(x, tzone = to_tz)
  x
  
}

birdie_board_lineup <- function(players,
                                bbperiod = c("front 9", "back 9", "full 18")) { 
  
  players = players[players != "Select Player"]
  field = pull_from_mongo(tour = "pga", db = "PreWorkFlow", collection = "Field") 
  current_round = field$current_round[1]
  
  field = field %>%
    dplyr::filter(playername %in% players) %>%
    dplyr::filter(round == current_round)
  
  
  if (bbperiod == "front 9") {
    holes <- c(1,2,3,4,5,6,7,8,9)
  } else if (bbperiod == "back 9") {
    holes <- c(10,11,12,13,14,15,16,17,18)  
  } else {
    holes <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
  }
  
  df = pull_from_mongo(tour = "pga", "PreWorkFlow", "PlayerHoleDistributions") %>%
    dplyr::filter(round == current_round) %>%
    dplyr::filter(playername %in% players) %>%
    dplyr::filter(hole %in% holes) %>%
    dplyr::mutate(prb = round(birdie + eagle, digits = 3)) %>%
    dplyr::select(
      last_updated, tour, qs_id, playername, round, hole, hole_par, hole_yards, eagle, birdie, par, bogey, double, prb)
  
  df = tidyr::pivot_wider(df, id_cols = c("hole"), names_from = "playername", values_from = "prb")
  return(df)
}

birdie_board_ticket <- function(players,
                                bbperiod = c("front 9", "back 9", "full 18"), 
                                vig_cents = 20) {    

  players = players[players != "Select Player"]
  field = pull_from_mongo(tour = "pga", db = "PreWorkFlow", collection = "Field") 
  current_round = field$current_round[1]
  
  field = field %>%
    dplyr::filter(playername %in% players) %>%
    dplyr::filter(round == current_round)
  
  
  if (bbperiod == "front 9") {
    holes <- c(1,2,3,4,5,6,7,8,9)
  } else if (bbperiod == "back 9") {
    holes <- c(10,11,12,13,14,15,16,17,18)  
  } else {
    holes <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
  }
  
  df = pull_from_mongo(tour = "pga", "PreWorkFlow", "PlayerHoleDistributions") %>%
    dplyr::filter(round == current_round) %>%
    dplyr::filter(playername %in% players) %>%
    dplyr::filter(hole %in% holes) %>%
    dplyr::mutate(prb = round(birdie + eagle, digits = 3)) %>%
    dplyr::select(
      last_updated, tour, event_name, course_name, course_id,
      qs_id, playername, round, hole, hole_par, hole_yards, eagle, birdie, par, bogey, double, prb)
  
  df = tidyr::pivot_wider(df, id_cols = c("hole"), names_from = "playername", values_from = "prb")
  
  if (nrow(df) < 1) {
    df <- NULL
  } else {
    df[2:ncol(df)] <- 1 - df[2:ncol(df)]
    x <- df[2:ncol(df)]
    bbprob = apply(x,1,prod)
    bbprob = 1 - (bbprob)
    players <- c(colnames(df[2:ncol(df)]))
    
    fair = prod(bbprob)
    vigged = logit_fair2vig(fair, vig_cents, strategy = "logit004")
    
    fair = ifelse(fair > 0.5, paste0("-", round(100 * fair / (1 - fair))), paste0("+", round(100 * (1 - fair) / fair)))
    vigged = ifelse(vigged > 0.5, paste0("-", mround(100 * vigged / (1 - vigged), 100)), paste0("+", mround(100 * (1 - vigged) / vigged, 100)))
    bbplayers = paste(players, collapse = ", ")
    ticketNumber = as.integer(Sys.time())
    dateTime = field$teetime
    gameDateTime = min(dateTime, na.rm = TRUE)
    wagerDescription = paste0("Birdie Board / ", stringr::str_to_title(bbperiod), " / ", bbplayers)
    df = data.frame(ticketNumber = ticketNumber,
                    gameDateTime, 
                    wagerDateTime = Sys.time(), 
                    wagerDescription, 
                    mlPrice = vigged)
    
  }
  return(df)
}

get_live_data <- function(tour = "pga") {
  # Connect to MongoDB
  df = pull_from_mongo(tour, db = "4caddy", collection = "LiveWin")
  return(df)
}

modify_playername = function(df = data.frame()) {
  
  df = df %>%
    mutate_if(is.character, utf8::utf8_encode) %>%
    mutate(
      across(starts_with("playername"), ~ sub("(^.*),\\s(.*$)","\\2 \\1", .x)),
      across(starts_with("playername"), ~ textclean::replace_non_ascii(.x)),
      across(contains("playername"), ~ stringr::str_to_lower(.x)),
      across(contains("playername"), ~ gsub("\\(.*", "", .x)),
      across(contains("playername"), ~ stringr::str_replace_all(.x, "[.]", "")),
      across(contains("playername"), ~ stringr::str_replace_all(.x, "[,]", "")),
      across(contains("playername"), ~ stringr::str_replace_all(.x, "[-]", " ")),
      across(contains("playername"), ~ stringr::str_replace_all(.x, "  ", " ")),
      across(contains("playername"), ~ if_else(.x == "samuel bennett", "sam bennett", .x)),
      across(contains("playername"), ~ if_else(.x == "rodolfo cazaubon", "rodolfo cazaubon jr", .x)),
      across(contains("playername"), ~ if_else(.x == "raul pereda", "raul pereda de la huerta", .x)),
      across(contains("playername"), ~ if_else(.x == "nacho echavarria", "nicolas echavarria", .x)),
      across(contains("playername"), ~ if_else(.x == "matias simaski", "matias simasky", .x)),
      across(contains("playername"), ~ if_else(.x == "jack sparrow", "james sparrow", .x)),
      across(contains("playername"), ~ if_else(.x == "gabriel morganbirke", "gabriel morgan birke", .x)),
      across(contains("playername"), ~ if_else(.x == "emilio dominguez", "emilio puma dominguez", .x)),
      across(contains("playername"), ~ if_else(.x == "chris petefish", "christopher petefish", .x)),
      across(contains("playername"), ~ if_else(.x == "charles osborne", "ollie osborne", .x)),
      across(contains("playername"), ~ if_else(.x == "james carr vernon", "carr vernon", .x)),
      across(contains("playername"), ~ if_else(.x == "brandon jude rennie", "brandon jude rennie", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin henry poke", "benjamin poke", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin folletsmith", "benjamin follett smith", .x)),
      across(contains("playername"), ~ if_else(.x == "alfredo garciaheredia", "alfredo garcia heredia", .x)),
      across(contains("playername"), ~ if_else(.x == "angel hidalgo", "angel hidalgo portillo", .x)),
      across(contains("playername"), ~ if_else(.x == "ignacio elvira mijares", "nacho elvira", .x)),
      across(contains("playername"), ~ if_else(.x == "michael lorenzo vera", "mike lorenzo vera", .x)),
      across(contains("playername"), ~ if_else(.x == "pep angles ros", "pep angles", .x)),
      across(contains("playername"), ~ if_else(.x == "siwoo kim", "si woo kim", .x)),
      across(contains("playername"), ~ if_else(.x == "sunghoon kang", "sung kang", .x)),
      across(contains("playername"), ~ if_else(.x == "samuel stevens", "sam stevens", .x)),
      across(contains("playername"), ~ if_else(.x == "matthew fitzpatrick", "matt fitzpatrick", .x)),
      across(contains("playername"), ~ if_else(.x == "mj viljoen", "m.j. viljoen", .x)),
      across(contains("playername"), ~ if_else(.x == "guillermo mito pereira", "mito pereira", .x)),
      across(contains("playername"), ~ if_else(.x == "bai bobby zhengkai", "zheng kai bai", .x)),
      across(contains("playername"), ~ if_else(.x == "cam davis", "cameron davis", .x)),
      across(contains("playername"), ~ if_else(.x == "vincent whaley", "vince whaley", .x)),
      across(contains("playername"), ~ if_else(.x == "joohyung kim", "tom kim", .x)),
      across(contains("playername"), ~ if_else(.x == "seonghyeon kim", "sh kim", .x)),
      across(contains("playername"), ~ if_else(.x == "kyoung hoon lee", "kh lee", .x)),
      across(contains("playername"), ~ if_else(.x == "kyounghoon lee", "kh lee", .x)),
      across(contains("playername"), ~ if_else(.x == "jayden schaper", "jayden trey schaper", .x)),
      across(contains("playername"), ~ if_else(.x == "yechun yuan" | .x == "yechun carl yuan", "carl yuan", .x)),
      across(contains("playername"), ~ if_else(.x == "samuel stevens", "sam stevens", .x)),
      across(contains("playername"), ~ if_else(.x == "chun an yu", "kevin yu", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin taylor", "ben taylor", .x)),
      across(contains("playername"), ~ if_else(.x == "robby shelton iv", "robby shelton", .x)),
      across(contains("playername"), ~ if_else(.x == "marty dou zecheng", "zecheng dou", .x)),
      across(contains("playername"), ~ if_else(.x == "sean o'hair", "sean o'hair", .x)),
      across(contains("playername"), ~ if_else(.x == "stephen allan", "steve allan", .x)),
      across(contains("playername"), ~ if_else(.x == "shahriffuddin ariffin", "shahriffudin ariffin", .x)),
      across(contains("playername"), ~ if_else(.x == "alexander beach", "alex beach", .x)),
      across(contains("playername"), ~ if_else(.x == "frank bensel jr", "frank bensel", .x)),
      across(contains("playername"), ~ if_else(.x == "grady brame jr", "grady brame", .x)),
      across(contains("playername"), ~ if_else(.x == "james beck iii", "jimmy beck", .x)),
      across(contains("playername"), ~ if_else(.x == "tomas bessa", "tomas guimaraes bessa", .x)),
      across(contains("playername"), ~ if_else(.x == "alexander wennstam", "alex wennstam", .x)),
      across(contains("playername"), ~ if_else(.x == "wu ashun", "ashun wu", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin cook", "ben cook", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin griffin", "ben griffin", .x)),
      across(contains("playername"), ~ if_else(.x == "benjamin follet smith", "benjamin follett smith", .x)),
      across(contains("playername"), ~ if_else(.x == "bernhard ritthammer", "bernd ritthammer", .x)),
      across(contains("playername"), ~ if_else(.x == "chris crawford", "christopher crawford", .x)),
      across(contains("playername"), ~ if_else(.x == "dru love", "dru love iv", .x)),
      across(contains("playername"), ~ if_else(.x == "frank bensel jr", "frank bensel", .x)),
      across(contains("playername"), ~ if_else(.x == "gregory odom jr", "greg odom jr", .x)),
      across(contains("playername"), ~ if_else(.x == "jake marriott", "jake marriot", .x)),
      across(contains("playername"), ~ if_else(.x == "jbe' kruger", "jbe kruger", .x)),
      across(contains("playername"), ~ if_else(.x == "jd fernandez", "juan diego fernandez", .x)),
      across(contains("playername"), ~ if_else(.x == "jose m olazabal", "jose maria olazabal", .x)),
      across(contains("playername"), ~ if_else(.x == "kk limbhasut", "khemkhon limbhasut", .x)),
      across(contains("playername"), ~ if_else(.x == "marc f haastrup", "marc flindt haastrup", .x)),
      across(contains("playername"), ~ if_else(.x == "mark a lawrence jr", "mark lawrence jr", .x)),
      across(contains("playername"), ~ if_else(.x == "matti schmid", "matthias schmid", .x)),
      across(contains("playername"), ~ if_else(.x == "chris dimarco", "cristian dimarco", .x)),
      across(contains("playername"), ~ if_else(.x == "chris o'neill", "christopher o'neill", .x)),
      across(contains("playername"), ~ if_else(.x == "colm campbell jr", "colm campbell", .x)),
      across(contains("playername"), ~ if_else(.x == "dan mccarthy", "daniel mccarthy", .x)),
      across(contains("playername"), ~ if_else(.x == "danny list", "daniel list", .x)),
      across(contains("playername"), ~ if_else(.x == "dave coupland", "david coupland", .x)),
      across(contains("playername"), ~ if_else(.x == "gregory odom jr", "greg odom jr", .x)),
      across(contains("playername"), ~ if_else(.x == "chen guxin", "guxin chen", .x)),
      across(contains("playername"), ~ if_else(.x == "chao haimeng", "haimeng chao", .x)),
      across(contains("playername"), ~ if_else(.x == "wu hongfu", "hongfu wu", .x)),
      across(contains("playername"), ~ if_else(.x == "wu tuxuan", "tuxuan wu ", .x)),
      across(contains("playername"), ~ if_else(.x == "wu weihuang", "weihuang wu", .x)),
      across(contains("playername"), ~ if_else(.x == "jj grey", "jonathan grey", .x)),
      across(contains("playername"), ~ if_else(.x == "joe dean", "joseph dean", .x)),
      across(contains("playername"), ~ if_else(.x == "joe deraney", "joseph deraney", .x)),
      across(contains("playername"), ~ if_else(.x == "joe lane", "joseph lane", .x)),
      across(contains("playername"), ~ if_else(.x == "s chikkarangappa", "chikkarangappa s", .x)),
      across(contains("playername"), ~ if_else(.x == "wc liang", "liang wenchong", .x)),
      across(contains("playername"), ~ if_else(.x == "wen tang lin", "lin wen tang", .x)),
      across(contains("playername"), ~ if_else(.x == "wenyi huang", "huang wenyi", .x)),
      across(contains("playername"), ~ if_else(.x == "yuxin lin", "lin yuxin", .x)),
      across(contains("playername"), ~ if_else(.x == "xinjun zhang", "zhang xinjun", .x)),
      across(contains("playername"), ~ if_else(.x == "bowen xiao", "xiao bowen", .x)),
      across(contains("playername"), ~ if_else(.x == "chao song", "song chao", .x)),
      across(contains("playername"), ~ if_else(.x == "cheng jin", "jin cheng", .x)),
      across(contains("playername"), ~ if_else(.x == "chengyao ma", "ma chengyao", .x)),
      across(contains("playername"), ~ if_else(.x == "daxing jin", "jin daxing", .x)),
      across(contains("playername"), ~ if_else(.x == "dinggen chen", "chen dinggen", .x)),
      across(contains("playername"), ~ if_else(.x == "dong su", "su dong", .x)),
      across(contains("playername"), ~ if_else(.x == "dongyu wang", "wang dongyu", .x)),
      across(contains("playername"), ~ if_else(.x == "william gordon", "will gordon", .x)),
      across(contains("playername"), ~ if_else(.x == "guowu zhou", "zhou guowu", .x)),
      across(contains("playername"), ~ if_else(.x == "guxin chen", "chen guxin", .x)),
      across(contains("playername"), ~ if_else(.x == "haimeng chao", "chao haimeng", .x)),
      across(contains("playername"), ~ if_else(.x == "huilin zhang", "zhang huilin", .x)),
      across(contains("playername"), ~ if_else(.x == "jia zhang", "zhang jia", .x)),
      across(contains("playername"), ~ if_else(.x == "jiangfeng ye", "ye jiangfeng", .x)),
      across(contains("playername"), ~ if_else(.x == "jianshan li", "li jianshan", .x)),
      across(contains("playername"), ~ if_else(.x == "jin zhang", "zhang jin", .x)),
      across(contains("playername"), ~ if_else(.x == "ollie charles osborne", "ollie osborne", .x)),
      across(contains("playername"), ~ if_else(.x == "paul haley ii", "paul haley", .x)),
      across(contains("playername"), ~ if_else(.x == "santiago tarrio ben", "santiago tarrio", .x)),
      across(contains("playername"), ~ if_else(.x == "ali al shahrani", "ali abdullah rahman al shahrani", .x)),
      across(contains("playername"), ~ if_else(.x == "abdul rahman abdulla al shahrani", "ali abdullah rahman al shahrani", .x)),
      across(contains("playername"), ~ stringr::str_to_title(.x)),
      across(contains("playername"), ~ gsub(" Ii", " II", .x)),
      across(contains("playername"), ~ gsub(" Iv", " IV", .x)),
      across(contains("playername"), ~ gsub(" Iii", " III", .x)),
      across(contains("playername"), ~ gsub(" IIi", " III", .x)),
      across(contains("playername"), ~ gsub("Jb ", "JB ", .x)),
      across(contains("playername"), ~ gsub("Ct ", "CT ", .x)),
      across(contains("playername"), ~ gsub("Kj ", "KJ ", .x)),
      across(contains("playername"), ~ gsub("Jj ", "JJ ", .x)),
      across(contains("playername"), ~ gsub("Tj ", "TJ ", .x)),
      across(contains("playername"), ~ gsub("Jt ", "JT ", .x)),
      across(contains("playername"), ~ gsub("Da ", "DA ", .x)),
      across(contains("playername"), ~ gsub("Dj ", "DJ ", .x)),
      across(contains("playername"), ~ gsub("Dh ", "DH ", .x)),
      across(contains("playername"), ~ gsub("Mj ", "MJ ", .x)),
      across(contains("playername"), ~ gsub("Aj ", "AJ ", .x)),
      across(contains("playername"), ~ gsub("Sh ", "SH ", .x)),
      across(contains("playername"), ~ gsub("Kh ", "KH ", .x)),
      across(contains("playername"), ~ gsub(" O'h", " O'H", .x)),
      across(contains("playername"), ~ gsub(" Mccarthy", " McCarthy", .x)),
      across(contains("playername"), ~ gsub(" Mcgreevy", " McGreevy", .x)),
      across(contains("playername"), ~ gsub(" Mcnealy", " McNealy", .x)),
      across(contains("playername"), ~ gsub(" Nesmith", " NeSmith", .x)),
      across(contains("playername"), ~ gsub(" Mcinerney", " McInerney", .x)),
      across(contains("playername"), ~ gsub(" Mccain", " McCain", .x)),
      across(contains("playername"), ~ gsub(" Mclardy", " McLardy", .x)),
      across(contains("playername"), ~ gsub(" Schonewillie", " Schonewille", .x)),
      across(contains("playername"), ~ gsub(" O'n", " O'N", .x)),
      across(contains("playername"), ~ gsub(" O'r", " O'R", .x)),
      across(contains("playername"), ~ gsub(" O'b", " O'B", .x)),
      across(contains("playername"), ~ gsub(" O'k", " O'K", .x)),
      across(contains("playername"), ~ gsub(" O'l", " O'L", .x)),
      across(contains("playername"), ~ gsub(" O't", " O'T", .x)),
      across(contains("playername"), ~ gsub(" O'c", " O'C", .x)),
      across(contains("playername"), ~ gsub(" O'm", " O'M", .x)),
      across(contains("playername"), ~ gsub(" O'd", " O'D", .x)),
      across(contains("playername"), ~ gsub(" O'g", " O'G", .x)),
      across(contains("playername"), ~ gsub(" O's", " O'S", .x)),
      across(contains("playername"), ~ gsub(" D'a", " D'A", .x)),
      across(contains("playername"), ~ gsub(" D'h", " D'H", .x)),
      across(contains("playername"), ~ gsub(" De'a", " De'A", .x)),
      across(contains("playername"), ~ gsub(" Jnr", " Jr", .x)),
      across(contains("playername"), ~ gsub(" DA Silva", " Da Silva", .x)),
      across(contains("playername"), ~ gsub(" Almansour", " Al Mansour", .x)),
      across(contains("playername"), ~ gsub("Abdulrahman", "Abdul Rahman", .x)),
      across(contains("playername"), ~ trimws(.x, which = "both"))
      
    )
  
  return(df)
  
}

tournament_info <- function() {
  tournament_1 = pull_from_mongo("pga", "PreWorkFlow", "Field") |>
    distinct(event_name, course_name, current_round)
  
  tournament_2 = pull_from_mongo("pga", "LiveWorkFlow", "Field") |>
    distinct(event_name, course_name, current_round)
  
  if (tournament_1$event_name == tournament_2$event_name) {
    tournament = tournament_2
  } else {
    tournament = tournament_1
  }
  return(tournament)
}


players = pull_from_mongo(tour = "pga", db = "PreWorkFlow", collection = "Field") %>%
  dplyr::distinct(playername) %>%
  dplyr::arrange(playername)