
library(tidyverse)
library(magrittr)
library(ggplot2)


se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse() %>% gsub("\"", "", .)
  DV <- substitute(DV) %>% deparse() %>% gsub("\"", "", .)
  stopifnot( subject_var %in% colnames(df) && DV %in% colnames(df) )
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% 
    dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T), 
                     .groups = "drop")
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% 
    dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM) %>%
    ungroup()
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>%
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N), 
                     .groups = "drop" )
}



df <- read.csv("./results.txt", row.names = NULL, comment.char = "#", header = F, col.names = 1:12)
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
questions %<>% dplyr::rename( "question"=val1, "answer"=val2, "answer_correct"=val3, "RT"=val4)
questions %<>% dplyr::select( subject, condition, item, question, answer, answer_correct, RT )
head(questions)

# separate fillers from non-fillers
spr_fillers <- spr %>% subset(condition == "filler")
spr %<>% subset(condition != "filler")
question_fillers <- questions %>% subset(condition == "filler")
questions %<>% subset(condition != "filler")


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


# to-do: double check that the indicator in the answer_np1 'column' matches what's in the 'answer' column
questions %<>% dplyr::rename(answer_np1 = answer_correct)


#########################################################
### *** exclude weird subjects based on responses *** ###
#########################################################

View(accuracy_by_subject)

accuracy_by_subject <-
  questions %>% 
  group_by(subject, condition) %>% 
  dplyr::summarise(perc_np1 = mean(answer_np1)) %>%
  tidyr::pivot_wider(names_from = condition, values_from = perc_np1)
accuracy_by_subject %<>% mutate( avg_unambiguous_accuracy = (a+(1-b)+d+(1-e))/4 )
plot(sort(accuracy_by_subject$avg_unambiguous_accuracy))

accuracy_by_subject %<>% arrange(avg_unambiguous_accuracy)
head(accuracy_by_subject, 12)

# exclude the all participants with the lowest accuracy, i.e. 70% and below
# NOTE: Wow! So many participants with low accuracy! I wonder if it's because the task seems so easy, or because LING 411 students didn't like the course :)
low_accuracy <- accuracy_by_subject %>% subset(avg_unambiguous_accuracy < .7) %>% .$subject

questions %<>% subset(!subject %in% low_accuracy)
spr %<>% subset(!subject %in% low_accuracy)

# Find more weird subjects based on reading times
spr_too_fast <- subset(spr, RT < 50)
table(spr_too_fast$subject) %>% sort()
spr_too_slow <- subset(spr, RT > 5000)
table(spr_too_slow$subject) %>% sort()

# What's going on with the other ultra-fast subjects? How is their accuracy?
# I wonder what's going with subject S[60] -- lots of very short RTs, but crazy high accuracy. Did they use some sort of
# strategy, or did they rehearse the sentence during the question phase? The latter would predict very high question answer RTs. 
accuracy_by_subject %>% subset(subject %in% c("S[60]","S[6]","S[21]"))

# Not excluding these participants for now, but excluding all trials with excessively fast and excessively slow responses
fast_rt_threshold = 50
slow_rt_threshold = 3000
spr %<>% group_by(subject, item, condition) %>% 
  dplyr::mutate(is_anomalous = any(RT < fast_rt_threshold) | any(RT > slow_rt_threshold) ) %>%
  filter(!is_anomalous) %>%
  dplyr::select(-is_anomalous)


############################################
### *** Analysis: question responses *** ###
############################################

answer_np1 <- questions %>% group_by(condition, NP2, attachment) %>% dplyr::summarise(perc_np1 = mean(answer_np1), N = n())
answer_np1

answer_np1_se <- se_cousineau(questions, n_conditions=6, subject, DV = "answer_np1", group = c("condition", "attachment", "NP2"), is_proportion = T)

answer_np1_se %>% 
  ggplot(aes(attachment, M, fill = NP2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=M+1.96*SE, ymax=M-1.96*SE), width = .2, position = position_dodge(width=1), color = "black") + theme_bw()

ggsave(answer_np1_se, file = "responses.pdf")


#######################################
### *** Analysis: reading times *** ###
#######################################

nwords <- function(x) x %>% stringr::str_split(pattern = " ") %>% sapply(., length)

stimuli <- readxl::read_xlsx("./stimuli/RC Adjective Experiment.xlsx", sheet = 1)

# check which columns vary in length
stimuli %>% 
  dplyr::select(Adjunct:Remainder) %>% 
  dplyr::summarise_all( function(x) max(nwords(x)) - min(nwords(x))  ) %>%
  as.data.frame() %>% sort()

stimuli_nwords <-
  stimuli %>% group_by(item) %>%
  dplyr::select(item, `Verb of report`, Remainder) %>% 
  dplyr::summarise( nwords_rc_verb = nwords(`Verb of report`), 
                    nwords_remainder = nwords(Remainder) )
stimuli_nwords$item %<>% as.character()
spr %<>% left_join(stimuli_nwords)

spr %<>% mutate( region_pos = ifelse(nwords_rc_verb == 2 & word_pos >= 6, word_pos-1, word_pos),
                 region_pos = ifelse(NP2 == "simple" & region_pos >= 7, region_pos+1, region_pos),
                 region_pos = ifelse(nwords_remainder == 2 & region_pos >= 10, region_pos-1, region_pos)
)

spr %<>% 
  group_by(subject, condition, item, attachment, NP2, nwords_rc_verb, nwords_remainder, region_pos) %>%
  dplyr::summarize(RT = mean(RT), word = paste(word, collapse = " "), N = n())

regions_info <- 
  data.frame(
    region_pos = 1:9,
    region_label = c("pref1", "pref2", "rc1", "rc2", "rc3", "n1", "adj", "n2", "v")
  )

regions_info$region_label %<>% factor(levels = regions_info$region_label)
spr %<>% left_join( regions_info )


ggplot(spr, aes(RT)) + geom_histogram() 
summary(spr$RT)

spr$log_RT <- log(spr$RT)
avg_spr <- spr %>% group_by(region_label) %>% 
  do( se_cousineau(., n_conditions=6, subject, DV = "log_RT", group = c("attachment", "NP2"), is_proportion = F) )

avg_spr %>% subset(!(region_label %in% c("pref1", "pref2", "rc1", "rc2", "rc3"))) %>%
  ggplot(aes(attachment, exp(M), color = attachment, linetype = NP2, shape = NP2, group = NP2:attachment )) + 
  geom_point(position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin=exp(M-1.96*SE), ymax=exp(M+1.96*SE) ), width = .2, position = position_dodge(width = .5)) +
  facet_wrap(~region_label, scales = "free", nrow = 1) + ylab("RT") + xlab(NULL)

# This is amazing:
# - No effect of attachment at N1, at all!
# - In the complex condition, there is a slowdown in the N2 attachment condition, at the adjective and at N2.
# - In the simple condition, there is *no* slowdown, anywhere.

