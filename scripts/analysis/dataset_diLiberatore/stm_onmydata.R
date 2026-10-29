#### library ####
library(stm)
library(wordcloud)
library(dplyr)
library(tidyr)
library(furrr)
library(bigrquery)
library(tidyverse)
library(tidytext)
library(data.table)
library(readxl)

#### data loading ####
VY <- read.csv("LAST_VERSION_noFuture_FINAL.csv")
# rimuovo ID
VY <- VY[,-1]
# rimuovo risposte aperte
VY <- VY[,-74]
ans <- read_excel("english_documents.xlsx")   
ans <- ans[,-1]
id <- read.csv("ID.csv")
id<-data.frame(id[,-1])

# funzione che fa il preprocessing del testo, funzione del pacchetto stm
# textProcessor(documents= documenti,
#               metadata= dataset, # non ne abbiamo bisogno per l'analisi ma per analisi successive se si vogliono fare
#               removestopwords=TRUE,
#               language='eng',
#               verbose = TRUE, 
#               custompunctuation=c( choose punctuation to remove), 
#               customstopwords = c( choose the stopwords to remove ), 
#               stem=TRUE) # fa lo stemming delle parole


processed<-textProcessor(documents=ans$`,x.2`,metadata=VY, removestopwords=TRUE,language='eng',verbose = TRUE,custompunctuation=c("-"),customstopwords = c(
  "precariousness",
  "0o", "0s", "3a", "3b", "3d", "6b", "6o", "a", "a1", "a2", "a3", "a4", 
  "ab", "able", "about", "above", "abst", "ac", "accordance", "according", 
  "accordingly", "across", "act", "actually", "ad", "added", "adj", "ae", 
  "af", "affected", "affecting", "affects", "after", "afterwards", "ag", 
  "again", "against", "ah", "ain", "ain't", "aj", "al", "all", "allow", 
  "allows", "almost", "alone", "along", "already", "also", "although", 
  "always", "am", "among", "amongst", "amoungst", "amount", "an", "and", 
  "announce", "another", "any", "anybody", "anyhow", "anymore", "anyone", 
  "anything", "anyway", "anyways", "anywhere", "ao", "ap", "apart", 
  "apparently", "appear", "appreciate", "appropriate", "approximately", 
  "ar", "are", "aren", "arent", "aren't", "arise", "around", "as", "a's", 
  "aside", "ask", "asking", "associated", "at", "au", "auth", "av", 
  "available", "aw", "away", "awfully", "ax", "ay", "az", "b", "b1", 
  "b2", "b3", "ba", "back", "bc", "bd", "be", "became", "because", 
  "become", "becomes", "becoming", "been", "before", "beforehand", 
  "begin", "beginning", "beginnings", "begins", "behind", "being", 
  "believe", "below", "beside", "besides", "best", "better", "between", 
  "beyond", "bi", "bill", "biol", "bj", "bk", "bl", "bn", "both", 
  "bottom", "bp", "br", "brief", "briefly", "bs", "bt", "bu", "but", 
  "bx", "by", "c", "c1", "c2", "c3", "ca", "call", "came", "can", 
  "cannot", "cant", "can't", "cause", "causes", "cc", "cd", "ce", 
  "certain", "certainly", "cf", "cg", "ch", "changes", "ci", "cit", 
  "cj", "cl", "clearly", "cm", "c'mon", "cn", "co", "com", "come", 
  "comes", "con", "concerning", "consequently", "consider", "considering", 
  "contain", "containing", "contains", "corresponding", "could", 
  "couldn", "couldnt", "couldn't", "course", "cp", "cq", "cr", "cry", 
  "cs", "c's", "ct", "cu", "currently", "cv", "cx", "cy", "cz", "d", 
  "d2", "da", "date", "dc", "dd", "de", "definitely", "describe", 
  "described", "despite", "detail", "df", "di", "did", "didn", "didn't", 
  "different", "dj", "dk", "dl", "do", "does", "doesn", "doesn't", 
  "doing", "don", "done", "don't","dont", "didnt", "doesnt", "down", "downwards", "dp", "dr", 
  "ds", "dt", "du", "due", "during", "dx", "dy", "e", "e2", "e3", "ea", 
  "each", "ec", "ed", "edu", "ee", "ef", "effect", "eg", "ei", "eight", 
  "eighty", "either", "ej", "el", "eleven", "else", "elsewhere", "em", 
  "empty", "en", "end", "ending", "enough", "entirely", "eo", "ep", 
  "eq", "er", "es", "especially", "est", "et", "et-al", "etc", "eu", 
  "ev", "even", "ever", "every", "everybody", "everyone", "everything", 
  "everywhere", "ex", "exactly", "example", "except", "ey", "f", "f2", 
  "fa", "far", "fc", "few", "ff", "fi", "fifteen", "fifth", "fify", 
  "fill", "find", "fire", "first", "five", "fix", "fj", "fl", "fn", 
  "fo", "followed", "following", "follows", "for", "former", "formerly", 
  "forth", "forty", "found", "four", "fr", "from", "front", "fs", 
  "ft", "fu", "full", "further", "furthermore", "fy", "g", "ga", "gave", 
  "ge", "get", "gets", "getting", "gi", "give", "given", "gives", 
  "giving", "gj", "gl", "go", "goes", "going", "gone", "got", "gotten", 
  "gr", "greetings", "gs", "gy", "h", "h2", "h3", "had", "hadn", 
  "hadn't", "happens", "hardly", "has", "hasn", "hasnt", "hasn't", 
  "have", "haven", "haven't", "having", "he", "hed", "he'd", "he'll", 
  "hello", "help", "hence", "her", "here", "hereafter", "hereby", 
  "herein", "heres", "here's", "hereupon", "hers", "herself", "hes", 
  "he's", "hh", "hi", "hid", "him", "himself", "his", "hither", "hj", 
  "ho", "home", "hopefully", "how", "howbeit", "however", "how's", 
  "hr", "hs", "http", "hu", "hundred", "hy", "i", "i2", "i3", "i4", 
  "i6", "i7", "i8", "ia", "ib", "ibid", "ic", "id", "i'd", "ie", "if", 
  "ig", "ignored", "ih", "ii", "ij", "il", "i'll", "im", "i'm", "immediate", 
  "immediately", "importance", "important", "in", "inasmuch", "inc", 
  "indeed", "index", "indicate", "indicated", "indicates", "information", 
  "inner", "insofar", "instead", "interest", "into", "invention", 
  "inward", "io", "ip", "iq", "ir", "is", "isn", "isn't", "it", "itd", 
  "it'd", "it'll", "its", "it's", "itself", "iv", "i've", "ix", "iy", 
  "iz", "j", "jj", "jr", "js", "jt", "ju", "just", "k", "ke", "keep", 
  "keeps", "kept", "kg", "kj", "km", "know", "known", "knows", "ko", 
  "l", "l2", "la", "largely", "last", "lately", "later", "latter", 
  "latterly", "lb", "lc", "le", "least", "les", "less", "lest", "let", 
  "lets", "let's", "lf", "like", "liked", "likely", "line", "little", 
  "lj", "ll", "ll", "ln", "lo","lot", "look", "looking", "looks", "los", 
  "lr", "ls", "lt", "ltd", "m", "m2", "ma", "made", "mainly", "make", 
  "makes", "many", "may", "maybe", "me", "mean", "means", "meantime", 
  "meanwhile", "merely", "mg", "might", "mightn", "mightn't", "mill", 
  "mine", "minus", "miss", "ml", "mn", "mo", "more", "moreover", 
  "most", "mostly", "move", "mr", "mrs", "ms", "mt", "mu", "much", 
  "mug", "must", "mustn", "mustn't", "my", "myself", "n", "n2", "na", 
  "name", "namely", "nay", "nc", "nd", "ne", "near", "nearly", "necessarily", 
  "necessary", "need", "needn", "needn't", "needs", "neither", "never", 
  "nevertheless", "new", "next", "ng", "ni", "nine", "ninety", "nj", 
  "nl", "nn", "no", "nobody", "non", "none", "nonetheless", "noone", 
  "nor", "normally", "nos", "not", "noted", "nothing", "novel", "now", 
  "nowhere", "np", "nr", "ns", "nt", "ny", "o", "oa", "ob", "obtain", 
  "obtained", "obviously", "oc", "od", "of", "off", "often", "og", 
  "oh", "oi", "oj", "ok", "okay", "ol", "old", "om", "omitted", "on", 
  "once", "one", "ones", "only", "onto", "op", "or", "ord", "os", 
  "ot", "other", "others", "otherwise", "ought", "our", "ours", "ourselves", 
  "out", "outside", "over", "overall", "ow", "owing", "own", "p", 
  "p1", "p2", "p3", "page", "pagecount", "pages", "par", "part", 
  "particular", "particularly", "past", "pc", "pd", "pe", "per", 
  "perhaps", "pf", "ph", "pi", "pj", "pk", "pl", "placed", "please", 
  "plus", "pm", "pn", "po", "poorly", "possible", "possibly", "potentially", 
  "pp", "pq", "pr", "predominantly", "present", "presumably", "previously", 
  "primarily", "probably", "promptly", "proud", "provides", "ps", 
  "pt", "pu", "put", "py", "q", "qj", "qu", "que", "quickly", "quite", 
  "qv", "r", "r2", "ra", "ran", "rather", "rc", "rd", "re", "readily", 
  "really", "reasonably", "recent", "recently", "ref", "refs", "regarding", 
  "regardless", "regards", "related", "relatively", "research-articl", 
  "respectively", "resulted", "resulting", "results", "rf", "rh", 
  "ri", "right", "rj", "rl", "rm", "rn", "ro", "rq", "rr", "rs", "rt", 
  "ru", "run", "rw", "ry", "s", "s2", "sa", "said", "same", "saw", 
  "say", "saying", "says", "sc", "sd", "se", "sec", "second", "secondly", 
  "section", "see", "seeing", "seem", "seemed", "seeming", "seems", 
  "seen", "self", "selves", "sensible", "sent", "serious", "seriously", 
  "seven", "several", "sf", "shall", "shan", "shan't", "she", "shed", 
  "she'd", "she'll", "she's", "shes", "should", "shouldn", "shouldn't", 
  "show", "showed", "shown", "showns", "shows", "si", "side", "significant", 
  "significantly", "similar", "similarly", "since", "sincere", "six", 
  "sixty", "sj", "sl", "slightly", "sm", "sn", "so", "some", "somebody", 
  "someday", "somehow", "someone", "something", "sometime", "sometimes", 
  "somewhat", "somewhere", "soon", "sorry", "sp", "specifically", 
  "specified", "specify", "specifying", "sq", "sr", "ss", "st", "still", 
  "stop", "strongly", "su", "sub", "substantially", "successfully", 
  "such", "sufficiently", "suggest", "sup", "sure", "sy", "system", 
  "sz", "t", "t1", "t2", "t3", "tb", "tc", "td", "te", "technique", 
  "technically", "tell", "ten", "tends", "text", "tf", "tg", "th", 
  "than", "thank", "thanks", "thanx", "that", "that'll", "thats", 
  "that's", "that've", "the", "their", "theirs", "them", "themselves", 
  "then", "thence", "there", "thereafter", "thereby", "there'd", "therefore", 
  "therein", "there'll", "thereof", "therere", "theres", "there's", 
  "thereupon", "there've", "these", "they", "theyd", "they'd", "they'll", 
  "theyre", "they're", "they've", "thick", "thin", "think", "third", 
  "this", "thorough", "thoroughly", "those", "thou", "though", "thoughh", 
  "thought", "thousand", "throug", "through", "throughout", "thru", 
  "thus", "til", "tip", "to", "together", "too", "thing", "took", "toward", 
  "towards", "tp", "tq", "tr", "tried", "tries", "truly", "try", "trying", 
  "ts", "tt", "twelve", "twenty", "twice", "two", "tx", "u", "u201d", 
  "u201c", "u2122", "ub", "uc", "ud", "ue", "uf", "ug", "uh", "ui", 
  "uj", "uk", "um", "un", "under", "underneath", "undoing", "unfortunately", 
  "unless", "unlike", "unlikely", "until", "unto", "uo", "up", "upon", 
  "upwards", "ur", "us", "use", "used", "useful", "uses", "using", 
  "usually", "ut", "uu", "ux", "uz", "v", "va", "value", "various", 
  "vc", "vd", "ve", "ve", "very", "via", "viz", "vj", "vl", "vo", 
  "vol", "vols", "volumtype", "vq", "vs", "vt", "vu", "w", "wa", 
  "want", "wants", "was", "wasn", "wasn't", "way", "we", "wed", 
  "we'd", "welcome", "well", "we'll", "well", "went", "we're", "we've", 
  "what", "whatever", "what'll", "whats", "what's", "when", "whence", 
  "whenever", "where", "whereafter", "whereas", "whereby", "wherein", 
  "wheres", "where's", "whereupon", "wherever", "whether", "which", 
  "while", "whilst", "whim", "whither", "who", "whod", "who'd", "whoever", 
  "whole", "who'll", "whom", "whomever", "whos", "who's", "whose", 
  "why", "wi", "widely", "will", "willing", "wish", "with", "within", 
  "without", "wo", "won", "won't", "words", "world", "would", "wouldn", 
  "wouldn't", "written", "ws", "www", "x", "x1", "x2", "x3", "x4", 
  "x5", "x6", "x7", "x8", "x9", "xa", "xb", "xc", "xd", "xe", "xf", 
  "xg", "xh", "xi", "xj", "xk", "xl", "xm", "xn", "xo", "xp", "xq", 
  "xr", "xs", "xt", "xu", "xv", "xw", "xx", "xy", "xz", "y", "y2", 
  "y3", "ya", "yahoo", "ye", "yeah", "year", "years", "yes", "yet", 
  "yj", "yl", "ym", "yn", "yo", "you", "you'd", "you'll", "your", 
  "you're", "yours", "yourself", "yourselves", "you've", "yr", "ys", 
  "yt", "yu", "z", "zero", "zi", "zz"
), stem=TRUE)


out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
## alcuni documenti vengono eliminati perche' vuoti.
## Creo quindi un dataset VY_filter dove tengo solo le righe con i documenti non vuoti
## sara' questo il dataset che useremo nelle analisi successive
index<-which(rownames(VY) %in% rownames(meta))
indexNOT<-which(!(rownames(VY) %in% rownames(meta)))
VY_filter <- VY[index, ]
id_filter <- data.frame(id[index,])
ans_lis_filter<-ans[index,]
ans_lis_filter<-ans_lis_filter$`,x.2`

#### stm - topics ####

## Numero di topics K da considerare

library(stm)
library(furrr)
plan(multisession)

# Find k: Approach 2
# Apprioccio piu' veloce ma meno preciso
# utilizziamo la funzione search k per cercare la K migliore
# scegliendo tra un range di possibili valori di K

set.seed(834)
system.time({
  findingk_ver2 <- searchK(documents = docs, 
                           vocab = vocab,
                           K = c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26), #specify K to try
                           N = 500, # matches 10% default
                           proportion = 0.5, # default
                           heldout.seed = 1234, # optional
                           M = 10, # default
                           cores = 1, # default=1
                           max.em.its = 75, #was 75
                           data = meta,
                           init.type = "Spectral",
                           verbose=TRUE
  )
})

# Plot
x11()
plot(findingk_ver2)

# Find k: Approach 1 
# approccio simile, piu' lento ma piu' preciso
set.seed(833)
system.time({
  findingk <- searchK(docs, vocab, K = c(14,15,16,17,18,19,20,21,22,23,24,25),
                      data = meta, verbose=FALSE
  )
})

# Plot
x11()
plot(findingk)
# scegliamo il k migliore guardando il plot
# idea base:
# vogliamo alta heldout likelihood
# bassi residui
# semantic coherence alta
# ci serve un trade off di queste misure

## nel mio caso ho scelto 20

#### 20 TOPICS ####


model_20K <- stm(documents = docs,
                 vocab = vocab, 
                 K = 20,
                 verbose = TRUE,seed=2024)


# per vedere quanti documenti in ogni topic se dovessimo considerare
# che un documento appartiene al topic con la probabilita' piu' alta
# non particolarmente informativo
theta_20K <- make.dt(model_20K)
Rank1_K20 <- NA #K = 12

for (i in 1:nrow(VY_filter)){
  column <- theta_20K[i,-1]
  maintopic <- colnames(column)[which(column==max(column))]
  Rank1_K20[i] <- maintopic
}
knitr::kable(broom::tidy(table(Rank1_K20)))

## estraggo theta
topic_matrix<-model_20K$theta
topic_matrix<-as.data.frame(topic_matrix)
colnames(topic_matrix) <- paste0("Topic", 1:20)
# format for table

# function that output the first n documents more relevant for a given topic
thoughts1 <- findThoughts(model_20K, texts = ans_lis_filter, n = 6, topics = 1)$docs[[1]]
thoughts2 <- findThoughts(model_20K, texts = ans_lis_filter, n = 6, topics = 2)$docs[[1]]
thoughts3 <- findThoughts(model_20K, texts = ans_lis_filter, n = 6, topics = 3)$docs[[1]]
thoughts4 <- findThoughts(model_20K, texts = ans_lis_filter, n = 6, topics = 4)$docs[[1]]
thoughts5 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 5)$docs[[1]]
thoughts6 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 6)$docs[[1]]
thoughts7 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 7)$docs[[1]]
thoughts8 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 8)$docs[[1]]
thoughts9 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 9)$docs[[1]]
thoughts10 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 10)$docs[[1]]
thoughts11 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 11)$docs[[1]]
thoughts12 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 12)$docs[[1]]
thoughts13 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 13)$docs[[1]]
thoughts14 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 14)$docs[[1]]
thoughts15 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 15)$docs[[1]]
thoughts16 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 16)$docs[[1]]
thoughts17 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 17)$docs[[1]]
thoughts18 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 18)$docs[[1]]
thoughts19 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 19)$docs[[1]]
thoughts20 <- findThoughts(model_20K, texts = ans_lis_filter, n=6, topics = 20)$docs[[1]]

# representation of these documents
par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts14, width = 30, main = "Topic 14")

x11()
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts1, width = 30, main = "Topic 1")
plotQuote(thoughts2, width = 30, main = "Topic 2")
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts4, width = 30, main = "Topic 4")
x11()
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts5, width = 30, main = "Topic 5")
plotQuote(thoughts6, width = 30, main = "Topic 6")
plotQuote(thoughts7, width = 30, main = "Topic 7")
plotQuote(thoughts8, width = 30, main = "Topic 8")
x11()
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts9, width = 30, main = "Topic 9")
plotQuote(thoughts10, width = 30, main = "Topic 10")
plotQuote(thoughts11, width = 30, main = "Topic 11")
plotQuote(thoughts12, width = 30, main = "Topic 12")
x11()
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts13, width = 30, main = "Topic 13")
plotQuote(thoughts14, width = 30, main = "Topic 14")
plotQuote(thoughts15, width = 30, main = "Topic 15")
plotQuote(thoughts16, width = 30, main = "Topic 16")
x11()
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts17, width = 30, main = "Topic 17")
plotQuote(thoughts18, width = 30, main = "Topic 18")
plotQuote(thoughts19, width = 30, main = "Topic 19")
plotQuote(thoughts20, width = 30, main = "Topic 20")

## expected proportion of the corpus that belongs to each topic.
x11()
plot(model_20K, type = "summary", xlim = c(0, 0.3), n=5)

## word cloud per topic
for (i in 1:20) {
  x11()
  cloud(model_20K, topic = i, scale = c(3, 0.4))
}
cloud(model_20K, topic = 14, scale = c(3, 0.4))
## whole corpus word cloud
x11()
cloud(model_20K, scale = c(3, 0.4))

## the structural topic model allow correlations between topics. 
## Positive correlations between topics indicate that both topics are 
## likely to be discussed within a document.

mod.out.corr <- topicCorr(model_20K,cutoff=0.3)
plot(mod.out.corr)

## creation of the final dataset
df_topics <- cbind(id_filter, VY_filter, topic_matrix)
df_top<-cbind(id_filter, topic_matrix)
# salvato in FINAL_CLEANED_END.csv





