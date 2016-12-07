library(plyr)

classificar = function(input_age, input_income, input_student, input_credit_rating, dataset_treinamento){
  #Criando as tabelas de frequencia

  #idade
  per_age <- table(dataset_treinamento$age, dataset_treinamento$Class.buys_computer)

  #ganhos
  per_income <- table(dataset_treinamento$income, dataset_treinamento$Class.buys_computer)
  
  #estudante ?
  per_student <- table(dataset_treinamento$student, dataset_treinamento$Class.buys_computer)

  #nota de crédito
  per_credit_rating <- table(dataset_treinamento$credit_rating, dataset_treinamento$Class.buys_computer)

  class_buy <- table(dataset_treinamento$Class.buys_computer)
  class_buy.freq = prop.table(class_buy)
  
  freq_yes = class_buy.freq[["yes"]]

  freq_yes_age = per_age[input_age, "yes"] / sum(per_age)
  freq_yes_income = per_income[input_income, "yes"] / sum(per_income)
  freq_yes_student = per_student[input_student, "yes"] / sum(per_student)
  freq_yes_credit_rating = per_credit_rating[input_credit_rating, "yes"] / sum(per_credit_rating)
  
  buy = (freq_yes_age * freq_yes_income * freq_yes_student * freq_yes_credit_rating) * freq_yes
  
  if(buy > 0.5){
    classification = "yes"
  } else {
    classification = "no"
  }
  
  result <- c(input_age, input_income, input_student, input_credit_rating, classification)
  
  print(buy)
 }

ler_dados_entrada = function(){
  input_age = readline("Entre com a faixa etária : (youth, Middle Aged ou Senior) \n")
  
  input_income = readline("Entre com a faixa salarial (low, medium, high) \n ")
  
  input_student = readline("Estudante ? : (yes, no) \n ")
  
  input_credit_rating = readline("Qual a nota de credito ? (fair, excellent) \n")

  dataset = read.csv("dataset.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  
  classificar(input_age, input_income, input_student, input_credit_rating, dataset)
}

ler_dados_entrada()

