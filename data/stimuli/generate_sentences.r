
library(tidyverse)
library(magrittr)

raw_items <- readxl::read_excel("./RC Adjective Experiment.xlsx")
View(raw_items)

# experimental conditions:
# a. no adjectives - N1 attachment
# b. no adjectives - N2 attachment
# c. no adjectives - ambiguous attachment
# d. adjectives - N1 attachment
# e. adjectives - N2 attachment
# f. adjectives - ambiguous attachment

raw_items %<>% mutate(a = paste(Adjunct, RC, `Verb of report`, N1_att_N1, N1_att_N2, Remainder))
raw_items %<>% mutate(b = paste(Adjunct, RC, `Verb of report`, N2_att_N1, N2_att_N2, Remainder))
raw_items %<>% mutate(c = paste(Adjunct, RC, `Verb of report`, amb_att_N1, amb_att_N2, Remainder))
raw_items %<>% mutate(d = paste(Adjunct, RC, `Verb of report`, N1_att_N1, N1_att_adjective, N1_att_N2, Remainder))
raw_items %<>% mutate(e = paste(Adjunct, RC, `Verb of report`, N2_att_N1, N2_att_adjective, N2_att_N2, Remainder))
raw_items %<>% mutate(f = paste(Adjunct, RC, `Verb of report`, amb_att_N1, amb_att_adjective, amb_att_N2, Remainder))

items <- raw_items %>% dplyr::select(item, a:f, Question, Answer1:Answer4)
items %<>% tidyr::pivot_longer(a:f, names_to = "condition", values_to = "sentence" )

items$attachment <- items$condition %>% dplyr::recode("c"="ambiguous", "f"="ambiguous",  "a"="N1","d"="N1",  "b"="N2","e"="N2")
items %<>% tidyr::pivot_longer(Answer1:Answer4, names_to = "answer_type", values_to = "answer" )


# mapping from answers (rows) to attachment conditions (columns)
#     a,d   b,e   c,f
#     N1    N2    amb
#    ----------------
# 1   -     N1    -
# 2   -     N2    N2
# 3   N1    -     N1  
# 4   N2    -     -
answers_mapping <- data.frame(attachment = rep(c("N1","N2","ambiguous"), each=4),
                              answer_type = c("Answer1","Answer2","Answer3","Answer4", "Answer1","Answer2","Answer3","Answer4", "Answer1","Answer2","Answer3","Answer4"),
                              answer_np   = c(NA,       NA,       "N1",     "N2",      "N1",     "N2",      NA,       NA,        NA,      "N2",     "N1",     NA)
                              )
items %<>% left_join( answers_mapping )
items %<>% subset(!is.na(answer_np))
items %<>% dplyr::select(-answer_type)

items$answer_np %<>% paste0("answer_", .)
items %<>% tidyr::pivot_wider(names_from = "answer_np", values_from = "answer")


sentence_template <-
'[["oranges_%s", %d], "DashedSentence", {s: "%s"}, "Question", {q: "%s", as: ["%s", "%s"], hasCorrect: 0}],'

ibex_sentences <- with(items, sprintf(sentence_template, condition, item, sentence, Question, answer_N1, answer_N2))
write_lines(ibex_sentences, file = "./ibex_stimuli/ibex_sentences")

system("cat ./ibex_stimuli/ibex_header ./ibex_stimuli/ibex_sentences ./ibex_stimuli/ibex_footer > ./ibex_stimuli.js")
