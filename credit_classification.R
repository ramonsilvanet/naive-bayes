dataset = read.csv("dataset.csv", header = TRUE, sep = ",", quote = "\"",
                   +          dec = ".", fill = TRUE, comment.char = "")


classificar(objeto, dataset_treinamento){
  #Criando as tabelas de frequencia
  
  #idade
  ages = dataset$age
  ages.freq = table(ages)
  ages.prop = prop.table(ages.freq)
  
  #ganhos
  incomes = dataset$income
  incomes.freq = table(incomes)
  incomes.prop = prop.table(incomes.freq)
  
  #estudante ?
  student = dataset$student
  student.freq = table(student)
  student.prop = prop.table(student.freq)
  
  #nota de crédito
  credit_rating <- dataset$credit_rating
  credit_rating.freq = table(credit_rating)
  credit_rating.prop = table(credit_rating.freq)
  
}