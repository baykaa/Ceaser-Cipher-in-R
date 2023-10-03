alphabet <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')

caesar <- function(start_text, shift_amount, cipher_direction) {
  end_text = ""
  #Let's seperate the letters
  split_text <- strsplit(start_text, split = "")
  split_text <- unlist(split_text)
  
  if (cipher_direction == "decode") {
    shift_amount <- shift_amount * -1
  }
  for (char in split_text) {
    if (char %in% alphabet) {
      position1 = match(char, alphabet)
      new_position = position1 + shift_amount
      
      #If index become (-), we need positive number for index!!!
      if (new_position < 0){
        new_position = 26 + new_position 
      }
      end_text = paste(end_text, alphabet[new_position], sep="")
    } else {
      end_text = paste(end_text, char, sep="")
    }
  }
  cat("Here's the", cipher_direction, "result:", end_text)
}

game_cont = TRUE
while(game_cont){
  direction <- readline("Type 'encode' to encrypt, type 'decode' to decrypt: ")
  text <- tolower(readline("Enter the text: "))
  shift_amount <- as.integer(readline("Enter the shift amount: "))
  
  #If shift amount is more than 26, get the modulus and use it
  if(shift_amount > 26){
    shift_amount = shift_amount %% 26
  }
  
  #Run the Caesar
  caesar(text, shift_amount, direction)
  
  restart = readline("Type 'yes' if you want to go again. Otherwise type 'no': ")
  if (restart == 'no'){
    game_cont = FALSE
    print("Game over!")
  }
}

