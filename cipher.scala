object cipher extends App {

  def encrypt(text: String, shift: Int): String = {
    String(text.toCharArray().map(i => {i match {
      case x if (x.toInt >= 'A'.toInt && x.toInt <= 'Z'.toInt) =>
        (((x.toInt - 'A'.toInt + shift) % 26) + 'A'.toInt).toChar;
      
      case y if (y.toInt >= 'a'.toInt && y.toInt <= 'z'.toInt) =>
        (((y.toInt - 'a'.toInt + shift) % 26) + 'a'.toInt).toChar;
      
      case _ => i
    }}))
  }

  def decrypt(text: String, shift: Int): String = {
    String(text.toCharArray().map(i => {i match {
      case x if (x.toInt >= 'A'.toInt && x.toInt <= 'Z'.toInt) => {((x.toInt - 'A'.toInt - shift) % 26) match {
        case a if (a < 0) => ('Z'.toInt + 1 + a).toChar;
        case b            => ('A'.toInt + b).toChar;
        
      }}

      case y if (y.toInt >= 'a'.toInt && y.toInt <= 'z'.toInt) => {((y.toInt - 'a'.toInt - shift) % 26) match {
        case a if (a < 0) => ('z'.toInt + 1 + a).toChar;
        case b            => ('a'.toInt + b).toChar;
      }}
      
      case _ => i
    }}))
  }

  def cipher(text: String, shift: Int, processor: (String, Int) => String ): String = {
    processor(text, shift);
  }

  println("Enter a text: ");
  var text: String = scala.io.StdIn.readLine();

  println("Enter a key: ");
  var shift: Int = scala.io.StdIn.readInt();

  var encrypted_text = cipher(text, shift, encrypt);
  printf("The string \"%s\" encrypted with the key %d is : %s\n", text, shift, encrypted_text);

  var decrypted_text = cipher(encrypted_text, shift, decrypt);
  printf("The string \"%s\" decrypted with the key %d is : %s\n", encrypted_text, shift, decrypted_text);

}
