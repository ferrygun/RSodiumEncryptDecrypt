encrypt <- function() {
  key <- hash(charToRaw("iERPs3cr3t"))
  msg <- serialize("iERP", NULL)
  
  # Encrypt with a random nonce
  nonce <- random(24)
  base64_nonce_enc = openssl::base64_encode(nonce)
  #print(base64_nonce_enc)
  base64_nonce_dec = openssl::base64_decode(base64_nonce_enc)
  #print(base64_nonce_dec)
  
  cipher <- data_encrypt(msg, key, nonce)
  result <- list(openssl::base64_encode(cipher),base64_nonce_enc)
  
  return(result)
}

decrypt <- function(id, nonce) {
  tryCatch({
    key <- hash(charToRaw("iERPs3cr3t"))
    idk = openssl::base64_decode(id)
    base64_nonce_dec = openssl::base64_decode(nonce)
    
    # Decrypt with same key and nonce
    orig <- data_decrypt(idk, key, base64_nonce_dec)
    return(unserialize(orig))
  }, error = function(err) {
    return("err")
  })
}

result = encrypt()
print(result[[1]])
print(result[[2]])

id = "6tDvNb8Gstmfm8NmSYFLX20Eno1MwIv91hDwdBBrnYOnM/+7g8V2mzHzJeRyBeHQjmCfBQScUXY7Zc+v"
secret = "G+7yeTSezucVimTDXDzXPK/zy95Qq0h2"

original_str = decrypt(id, secret)
print(original_str)
