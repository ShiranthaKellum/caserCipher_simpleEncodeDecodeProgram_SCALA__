object caeserCipherUserInput {
    def main (args : Array [String]): Unit = {
        val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "         // encypt and decypt base on this

        val encrypt = (letter : Char, shift : Int, alph : String) =>        // encription method (letter is the given letter, shift is the number of shiftings and the alph is for alphabet)
            alph ((alph.indexOf (letter.toUpper) + shift) % alph.size)        // this will prevent  over indexing error (indexing over length)

        val decrypt = (letter : Char, shift : Int, alph : String) =>        // decription method
            if (alph.indexOf (letter.toUpper) > shift - 1)                    
                alph (alph.indexOf (letter.toUpper) - shift)

            else alph (alph.indexOf (letter.toUpper) - shift + alph.size)     // this will prevent the low indexing error (minus index valus)

        val cipher = (method : (Char, Int, String) => Char, text : String, shift : Int, alph : String) =>     // encrypt and decrypt masseges
            text.map (method (_, shift, alph))

        val massage = scala.io.StdIn.readLine()                                         // get massege

        val deMassege = cipher (encrypt, massage, 5, alphabet)

        println ("Encripted massege  : " + deMassege)         

        print ("Decripted masseage : " + cipher (decrypt, deMassege, 5, alphabet))

    }
}