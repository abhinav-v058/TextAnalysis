ToSentence<-function(text, lang = "en") {
  
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  
  text <- as.String(text)
  
  
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  sentences <- text[sentence.boundaries]
  
  return(sentences)
}


CalculateSentenceLengthTable<-function(acqC)
{
  for(j in seq(acqC))
  {
    maxCount<-0
    sen<-ToSentence(acqC[j])
    for(i in seq(sen))
    {
      print("DocumentID")
      print(j)
      print("SentenceID")
      print(i)
      print("Sentence Length")
      print(nchar(sen[i]))
    }
  }
}


removeNumPunct <-
  function(x) gsub("[^[:alpha:][:space:]]*", "", x)


RemovePunctuationOfSentence<-function(acqC)
{
  for(j in seq(acqC))
  {
    maxCount<-0
    sen<-ToSentence(acqC[j])
    for(i in seq(sen))
    {
      print(removeNumPunct(sen[i]))
    }
  }
}


PrintPartOfSpeech<-function(acqC)
{
  for(j in seq(acqC))
  {
    maxCount<-0
    sen<-ToSentence(acqC[j])
    for(i in seq(sen))
    {
      print("DocumentID")
      print(j)
      print("SentenceID")
      print(i)
      print("POS")
      print(nchar(sen[i]))
      
      if(initDict()) {
        filter <- getTermFilter("StartsWithFilter", "car", TRUE)
        getIndexTerms("NOUN", 5, filter)
      }
      
      if(initDict()) {
        filter <- getTermFilter("StartsWithFilter", "car", TRUE)
        getIndexTerms("ADJECTIVE", 5, filter)
      }
      
      if(initDict()) {
        filter <- getTermFilter("StartsWithFilter", "car", TRUE)
        getIndexTerms("ADVERB", 5, filter)
      }
      
      if(initDict()) {
        filter <- getTermFilter("StartsWithFilter", "car", TRUE)
        getIndexTerms("VERB", 5, filter)
      }
    }
  }
}