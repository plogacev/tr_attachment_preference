
library(tidyverse)
library(magrittr)
library(ggplot2)


path_questions <- "../workspace/data_questions.feather"
path_spr <- "../workspace/data_spr.feather"
path_results <- "../data/ibex/results/results"

df <- read.csv(path_results, row.names = NULL, comment.char = "#", header = F, col.names = 1:12)
colnames(df) <- c("time", "ip_md5", "controller", "item_id", "element_id", "exp_condition", "item", "val1", "val2", "val3", "val4", "sentence")

df$subject <- with(df, paste(time, ip_md5)) %>% as.factor %>% as.integer %>% sprintf("S[%s]", .) %>% as.factor()
df %<>% dplyr::select(-time, -ip_md5)

# extract responses to questions in form
form <- subset(df, controller == "Form")
form %<>% dplyr::select(-item_id, -element_id, -exp_condition, -item, -val3:-sentence)
form %<>% subset(val1 != "_REACTION_TIME_")
form %<>% tidyr::pivot_wider(names_from = "val1", values_from = "val2")
View(form) # because of a copy-and-paste error 'male' in the natturk column means 'yes' (native speaker)

# extract spr reading times
spr <- subset(df, controller == "DashedSentence")
spr %<>% subset(exp_condition != "practice")
spr$condition <- spr$exp_condition %>% stringr::str_split_fixed("_", n=2) %>% .[,2]
spr %<>% dplyr::rename("word_pos"=val1, "word"=val2, "RT"=val3)
spr %<>% dplyr::select(-val4, -sentence, -element_id, -item_id, -controller) 
spr %<>% dplyr::select(subject, condition, item, word_pos, word, RT )
spr$word_pos %<>% as.integer()

# extract question answers
questions <- subset(df, controller == "Question")
questions %<>% subset(exp_condition != "practice")
questions$condition <- questions$exp_condition %>% stringr::str_split_fixed("_", n=2) %>% .[,2]
questions %<>% dplyr::select(-controller, -item_id, -element_id, -exp_condition, -sentence)
questions %<>% dplyr::rename( "question"=val1, "answer"=val2, "answer_np1"=val3, "RT"=val4)
questions %<>% dplyr::select( subject, condition, item, question, answer, answer_np1, RT )
head(questions)
View(questions)

# separate fillers from non-fillers
spr_fillers <- spr %>% subset(condition == "filler")
spr %<>% subset(condition != "filler")
question_fillers <- questions %>% subset(condition == "filler")
questions %<>% subset(condition != "filler")



######################################
### merge in condition information ###
######################################

# mapping from answers (rows) to attachment conditions (columns)
#     a,d   b,e   c,f
#     N1    N2    amb
#    ----------------
conditions_info <- 
  data.frame(
    condition = c("a", "b", "c", "d", "e", "f"),
    attachment = c("N1", "N2", "ambiguous", "N1", "N2", "ambiguous") %>% as.factor(),
    NP2 = c("simple", "simple", "simple", "complex", "complex", "complex") %>% as.factor()
  )
questions %<>% dplyr::left_join( conditions_info )
spr %<>% dplyr::left_join( conditions_info )

###################################
### merge in filler information ###
###################################

answers_fillers <- readxl::read_xlsx("../data/stimuli/Fillers_MVP.xlsx", sheet = 1)

head(answers_fillers)#
answers_fillers %<>% dplyr::rename(item=exp_number) %>% 
                      dplyr::select(-actual_number, -sentence) %>%
                      dplyr::mutate(item = as.character(item))

# drop items 87 and 92, as they have the same questions in both versions (to-do: identify the correct ones later) 
answers_fillers %<>% subset(!item %in% c(87,92))

# drop items without correct answers 
answers_fillers %<>% subset(!item %in% c(58,64))

# merge in correct answers
question_fillers %<>% dplyr::left_join( answers_fillers )

# drop responses without data on the correct answer
question_fillers %<>% subset( !is.na(correct_answer) )

# determine whether the answer is correct
question_fillers %<>% mutate( answer_correct = ifelse(answer_number == 1, answer_np1, !answer_np1 ) ) %>% dplyr::select(-answer_np1)

fillers_accuracy <- question_fillers %>% group_by(subject) %>% dplyr::summarise(accuracy_fillers = mean(answer_correct))


#######################################################
### *** exclude odd subjects based on responses *** ###
#######################################################

accuracy_by_subject <-
  questions %>% 
  group_by(subject, attachment) %>% 
  dplyr::summarise(perc_np1 = mean(answer_np1)) %>%
  tidyr::pivot_wider(names_from = attachment, values_from = perc_np1) %>%
  dplyr::select(-ambiguous)
accuracy_by_subject %<>% mutate( avg_unambiguous_accuracy = (N1+(1-N2))/2 )

ultra_short_spr_rt <- 50
ultra_long_spr_rt <- 2000
anomalous_rts <- spr %>% group_by(subject, item) %>% 
            dplyr::summarise( anomalous_spr_rts = sum(RT < ultra_short_spr_rt | RT > ultra_long_spr_rt ) > 2 ) %>%
            dplyr::summarise( perc_anomalous_spr_rts = mean(anomalous_spr_rts) )
hist(anomalous_rts$perc_anomalous_spr_rts)

# exclude the all participants with the lowest accuracy, i.e. 60% and below
# NOTE: Wow! So many participants with low accuracy!
low_accuracy <- accuracy_by_subject %>% subset(avg_unambiguous_accuracy < .6) %>% .$subject %>% as.character()
ultrafast_rts <- anomalous_rts %>% subset(perc_anomalous_spr_rts > .1) %>% .$subject %>% as.character()
excluded_subjects <- c(low_accuracy, ultrafast_rts)

# # exclude participants with low exceptionally low accuracy or too many anomalous RTs
# questions %<>% subset(!subject %in% excluded_subjects)
# spr %<>% subset(!subject %in% excluded_subjects)

# merge in filler accuracy
questions %<>% left_join( fillers_accuracy )

# save formatted and filtered RTs
feather::write_feather(questions, path = path_questions)
feather::write_feather(spr, path = path_spr)

