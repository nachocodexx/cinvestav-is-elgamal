package mx.cinvestav

import cats.implicits._
import cats.data.State

import java.math.BigInteger
import java.util.Random
import scala.annotation.tailrec
import mx.cinvestav.domain._

case class ProgramState(c1:Computer, c2:Computer, q:BigInteger, g:BigInteger)
object ElGamalMain extends App{
  type RawData    = Array[Byte]
  val NoData      = Array.emptyByteArray
  type CipherText = (BigInteger,BigInteger)

  def log[A](x:A):State[ProgramState,A] = State[ProgramState,A] { state =>
    val _c1 = state.c1
    val _c2 = state.c2
    def printComputer(c: Computer): Unit = {
      println(s"${c.name}(PK): ${c.publicKey.getOrElse("No key")}")
      println(s"${c.name}(SK): ${c.privateKey.getOrElse("No key")}")
      println("________________________________________")
    }
    println(s"g(generator): ${state.g}")
    println(s"q(mod): ${state.q}")
    println("________________________________________")
    printComputer(_c1)
    printComputer(_c2)
    (state,x)
  }

  @tailrec
  def generateRandomBigInteger(n:BigInteger):BigInteger = {
    val nn = n.subtract(new BigInteger("1"))
    val bLen = nn.bitLength()
    val rand = new Random()
    val x = new BigInteger(bLen,rand)
    if(x.compareTo(n) >0 ) generateRandomBigInteger(n)
    else  x
  }
  def generateSecretKey:State[ProgramState,RawData]=
    State[ProgramState,RawData]{ s=>
      val c1= s.c1.copy(privateKey = Option(generateRandomBigInteger(s.q)))
      val c2= s.c2.copy(privateKey = Option(generateRandomBigInteger(s.q)))
      (s.copy(c1,c2),Array.empty[Byte])
    }

  def generatePublicKeys:State[ProgramState,RawData] = State[ProgramState,RawData] { state=>
    val _c1 = state.c1
    val _c2 = state.c2
    val c1 =_c1.copy(publicKey = _c1.privateKey.map(state.g.modPow(_,state.q)))
    val c2 =_c2.copy(publicKey = _c2.privateKey.map(state.g.modPow(_,state.q)))
    (state.copy(c1,c2),NoData)
  }
  def encrypt(msg:String):State[ProgramState,CipherText] =State[ProgramState,CipherText]{ state=>
    val _c1  =state.c1
    val y = generateRandomBigInteger(state.q)
    val s = _c1.publicKey.map(_.modPow(y,state.q))
    val ct1 = state.g.modPow(y,state.q)
    val ct2 = s.map(new BigInteger(msg.getBytes).multiply(_).mod(state.q)).get
//    LOGGING
    println("****************ENCRYPTION**************")
    println(s"y: $y")
    println(s"s: ${s.get}")
    println(s"C1: $ct1")
    println(s"C2: $ct2")
    println("________________________________________")
    (state,(ct1,ct2))
  }
 def decrypt(ct:CipherText): State[ProgramState,RawData]  = State[ProgramState,RawData]{state=>
   val s        = state.c1.privateKey.map(ct._1.modPow(_,state.q))
   val sInverse = s.map(_.modInverse(state.q))
//   val sInv     = state.c1.privateKey.map(x=>ct._1.modPow(state.q.subtract(x),state.q).mod(state.q))
   val m        = sInverse.map(ct._2.multiply).map(_.mod(state.q))
   val _m = m.map(_.toByteArray).get
   println("****************DECRYPTION**************")
   println(s"s: ${s.get}")
   println(s"s^-1: ${sInverse.get}")
   println(s"s^-1 * s: ${sInverse.get.multiply(s.get).mod(state.q)}")
   println(s"m: ${m.get}")
   println(s"m(String): ${new String(_m)}")
   println("________________________________________")
   (state,_m)
}


  val rand = new Random()
  val g= new BigInteger("25195908475657893494027183240048398571429282126204032027777137836043662020707595556264018525880784406918290641249515082189298559149176184502808489120072844992687392807287776735971418347270261896375014971824691165077613379859095700097330459748808428401797429100642458691817195118746121515172654632282216869987549182422433637259085141865462043576798423387184774447920739934236584823824281198163815010674810451660377306056201619676256133844143603833904414952634432190114657544454178424020924616515723350778707749817125772467962926386356373289912154831438167899885040445364023527381951378636564391212010397122822120720357")
  val q = new BigInteger("28000683183178967691045327604549384273200737889408995067805870891607724272230730502935636255532870616923025398319616632616092747176572652764654213686197067978724897059080696115029373038734138776497829382209184353226737118067832131611204962781402651965177616959574821618486012482587117231822248731517319319566924904448200435653538174582490406853948962841924553170338450032277400949816544697301833261918343852251257502237396393086633583807821908438408392306669481101256146839747375312263603398130110605035799543160744850445683794785163914245886097291108437856051664713997308071095372852050222033510254270476986339211627")
  val c1 = Computer("C1")
  val c2 = Computer("C2")

  val app = generateSecretKey >> generatePublicKeys >> encrypt("HOLA").flatMap(decrypt)
  app.run(ProgramState(c1,c2,q,g)).value


}
