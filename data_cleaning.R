# ---- DATA CLEANING ----
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

transcripts <- read_rds("peaky_blinders_transcripts.rds")
peakyCorpus <- corpus(transcripts$Transcripts)

# Tokenise the transcripts
transcripts_tkns <- transcripts$Transcripts |>
  tokens(
    remove_numbers = T,
    remove_punct = T,
    remove_symbols = T
  ) |>
  tokens_remove(stopwords("en")) |>
  tokens_tolower()

# List most common character name phrases
collocations_names <- c(
  "mr winston", "laura mckee", "haydn stagg",
  "mrs connors", "luca changretta", "grace shelby",
  "mr levitt", "oswald mosley", "billy kimber",
  "winston churchill", "polly gray", "aunt polly",
  "michael gray", "mr solomons", "mr campbell",
  "mr gold", "jessie eden", "jimmy mccavern",
  "uncle jack", "freddie thorne", "mr thomas",
  "jack nelson", "johnny dogs", "aberama gold",
  "mr churchill", "arthur shelby", "mr nelson",
  "thomas shelby", "tommy shelby", "mr shelby",
  "mrs changretta", "billy grade", "evadne barwell",
  "elizabeth gray", "greta jurossi", "mrs thorne",
  "mr stagg"
)

# Create the collocations object
collocations <- transcripts_tkns |>
  textstat_collocations(min_count = 2) |>
  filter(collocation %in% collocations_names)

# Add underscore between name phrases
# e.g "mr shelby" > "mr_shelby"
transcripts_tkns2 <- transcripts_tkns |>
  tokens_compound(pattern = phrase(collocations$collocation))

# Now substitute different character names & nicknames into one
# Names & nicknames designating the same character
original_names <- c(
  "thomas", "tommy", "tom", "mr_thomas","thomas_shelby", "tommy_shelby", "mr_shelby",
  "ada", "mrs_thorne",
  "campbell", "mr_campbell",
  "polly","polly_gray", "aunt_polly", "elizabeth_gray",
  "arthur", "arthur_shelby",
  "grace", "grace_shelby",
  "freddie", "freddie_thorne", 
  "churchill", "winston_churchill", "mr_winston", "mr_churchill",
  "billy", "billy_kimber", 
  "alfie",  "mr_solomons",
  "michael",  "michael_gray", 
  "ruben", "oliver", 
  "leon", "romanov",
  "jessie","jessie_eden", 
  "luca",  "luca_changretta",
  "aberama","mr_gold", "aberama_gold",
  "ben", "younger", 
  "oswald", "mosley", "oswald_mosley", 
  "jimmy", "jimmy_mccavern", "mccavern",
  "jack","uncle_jack", "mr_nelson", "nelson", "jack_nelson",
  "laura", "laura_mckee", 
  "hydyn_stagg", "mr_stagg")

new_names <- c(
  rep("thomas", 7),
  rep("ada", 2),
  rep("campbell", 2),
  rep("polly", 4),
  rep("arthur", 2),
  rep("grace", 2),
  rep("freddie", 2), 
  rep("churchill", 4),
  rep("billy_kimber", 2), 
  rep("alfie", 2),
  rep("michael", 2), 
  rep("ruben", 2), 
  rep("romanov", 2),
  rep("jessie_eden", 2), 
  rep("luca_changretta", 2),
  rep("aberama", 3),
  rep("younger", 2), 
  rep("oswald_mosley", 3), 
  rep("jimmy_mccavern", 3),
  rep("jack_nelson", 5),
  rep("laura_mckee", 2), 
  rep("hayden_stagg", 2)
)

transcripts_tkns_final <- transcripts_tkns2 |> 
  tokens_replace(original_names, new_names)

# Based on this new text, build a co-occurrence matrix
cooccurrence_matrix <- fcm(transcripts_tkns_final,
  context = "window",
  count = "frequency",
  window = 25,
  tri = FALSE 
)

# Peaky Blinders character names and nicknames (including phrases)
characters <- c(
  "thomas", "tommy", "tom", "mr_thomas","thomas_shelby", "tommy_shelby", "mr_shelby", 
  "campbell", "mr_campbell", 
  "moss",
  "polly","polly_gray", "aunt_polly", 
  "arthur", "arthur_shelby",
  "grace", "grace_shelby",
  "freddie", "freddie_thorne", 
  "ada", "mrs_thorne",
  "john", 
  "johnny_dogs",
  "charlie", 
  "jeremiah",
  "isiah",
  "danny",
  "churchill", "winston_churchill", "mr_winston", "mr_churchill",
  "billy", "billy_kimber", 
  "darby", 
  "sabini", 
  "alfie",  "mr_solomons",
  "may", 
  "michael", "michael_gray", 
  "lizzie", 
  "esme", 
  "tatiana", 
  "izabella",
  "ruben", "oliver", 
  "leon", "romanov",
  "linda",
  "jessie","jessie_eden", 
  "luca",  "luca_changretta",
  "angel_changretta",
  "curly", 
  "bonnie",
  "aberama","mr_gold", "aberama_gold",
  "gina",
  "ben", "younger", 
  "oswald", "mosley", "oswald_mosley", 
  "jimmy", "jimmy_mccavern", "mccavern",
  "chang", 
  "barney", 
  "finn", 
  "frances", 
  "mitford", 
  "jack","uncle_jack", "mr_nelson", "nelson", "jack_nelson",
  "laura",  "laura_mckee", 
  "erasmus",
  "hayden_stagg", 
  "mrs_connors",
  "mr_levitt",
  "mrs_changretta",
  "anna",
  "barney",
  "billy_grade",
  "cyril",
  "evadne_barwell",
  "hughes",
  "goliath",
  "karl",
  "gretta_jurossi",
  "vicente"
)

# Filter co-occurrence matrix to only keep character interactions
cooccurrence_df <- convert(cooccurrence_matrix, to = "data.frame") |>
  filter(doc_id %in% characters)

new_order <- cooccurrence_df$doc_id

cooccurrence_df <- cooccurrence_df |> 
  select(doc_id, any_of(new_order)) |>
  column_to_rownames(var = "doc_id")
  

saveRDS(cooccurrence_df, "cooccurrence_df.rds")