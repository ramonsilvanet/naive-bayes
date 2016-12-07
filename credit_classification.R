
classificar = function(input_age, input_income, input_student, input_credit_rating, dataset_treinamento){
  #Criando as tabelas de frequencia
  
  #age
  prop_per_age <- table(dataset_treinamento$age, dataset_treinamento$Class.buys_computer)
  freq_age <- table(dataset_treinamento$age)
  prop_age = freq_age[input_age] / sum(freq_age)
  
  #income
  prop_per_income <- table(dataset_treinamento$income, dataset_treinamento$Class.buys_computer)
  freq_income <- table(dataset_treinamento$income)
  prop_income = freq_income[input_income] / sum(freq_income)
  
  #student
  prop_per_student <- table(dataset_treinamento$student, dataset_treinamento$Class.buys_computer)
  freq_student <- table(dataset_treinamento$student)
  prop_student = freq_student[input_student] / sum(freq_student)
  
  #credit rating
  prop_per_credit_rating <- table(dataset_treinamento$credit_rating, dataset_treinamento$Class.buys_computer)
  freq_credit_rating <- table(dataset_treinamento$credit_rating)
  prop_credit_rating = freq_credit_rating[input_credit_rating] / sum(freq_credit_rating)
  
  #Apply the method
  class_buy <- table(dataset_treinamento$Class.buys_computer)
  class_buy.freq = prop.table(class_buy)
  
  prop_yes = class_buy.freq[["yes"]]
  prop_no = class_buy.freq[["no"]]
  
  #Prop by age
  prop_age_yes = prop_per_age[input_age, "yes"] / class_buy[["yes"]]
  prop_age_no = prop_per_age[input_age, "no"] / class_buy[["no"]]
  
  #prop by income
  prop_income_yes = prop_per_income[input_income, "yes"] / class_buy[["yes"]]
  prop_income_no = prop_per_income[input_income, "no"] / class_buy[["no"]]
  
  #prop by student
  prop_student_yes = prop_per_student[input_student, "yes"] / class_buy[["yes"]]
  prop_student_no = prop_per_student[input_student, "no"] / class_buy[["no"]]
  
  #prop by credit rating
  prop_credit_rating_yes = prop_per_credit_rating[input_credit_rating, "yes"] / class_buy[["yes"]]
  prop_credit_rating_no = prop_per_credit_rating[input_credit_rating, "no"] / class_buy[["no"]]
  
  prop_buy_yes = prop_age_yes * prop_income_yes * prop_student_yes * prop_credit_rating_yes *  prop_yes
  prop_buy_no  = prop_age_no * prop_income_no * prop_student_no * prop_credit_rating_no * prop_no
  
  if(prop_buy_yes > prop_buy_no){
    print("Comprará um computador")
  } else {
    print("Não comprará um computador")
  }
  
}

ler_dados_entrada = function(){
  input_age = readline("Entre com a faixa etária : (youth, middle aged ou senior) \n")
  check_age <- c("youth", "middle aged", "senior")
  if (!input_age %in% check_age) stop("Invalid 'age' value")
  
  input_income = readline("Entre com a faixa salarial (low, medium, high) \n ")
  check_income <- c("low", "medium", "high")
  if (!input_income %in% check_income) stop("Invalid 'income' value")
  
  input_student = readline("Estudante ? : (yes, no) \n ")
  check_student <- c("yes", "no")
  if (!input_student %in% check_student) stop("Invalid 'student' value")
  
  input_credit_rating = readline("Qual a nota de credito ? (fair, excellent) \n")
  check_credit_rating <- c("fair", "excellent")
  if (!input_credit_rating %in% check_credit_rating) stop("Invalid 'credit rating' value")
  
  dataset = read.csv("dataset.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  
  classificar(input_age, input_income, input_student, input_credit_rating, dataset)
}

ler_dados_entrada()