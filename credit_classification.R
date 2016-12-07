

classificar = function(input_age, input_income, input_student, input_credit_rating, dataset_treinamento){
  #Criando as tabelas de frequencia

  #idade
  ages = table(dataset_treinamento$age)
  ages.freq = prop.table(ages)
  
  #ganhos
  incomes = table(dataset_treinamento$income)
  incomes.freq = prop.table(incomes)
  
  #estudante ?
  student = table(dataset_treinamento$student)
  student.freq = prop.table(student)

  
  #nota de crédito
  credit_rating <- table(dataset_treinamento$credit_rating)
  credit_rating.freq = prop.table(credit_rating)

  buys_computer <- ages.freq[[input_age]] * incomes.freq[[input_income]] * student.freq[[input_student]] * credit_rating.freq[[input_credit_rating]]
  
  print( incomes.freq[[input_income]] )
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

