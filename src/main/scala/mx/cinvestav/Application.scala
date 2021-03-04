package mx.cinvestav

import cats.data.State
import cats.implicits._

import java.math.BigInteger
import java.util.Random

case class Computer1(name:String, publicK:Option[BigInteger]=None, privateK:Option[BigInteger]=None,
                     sharedK:Option[BigInteger]=None)
case class ProgramState1(
                         c1:Computer1,
                         c2:Computer1,
                         n:BigInteger,
                         g:BigInteger
                       )
object Application extends  App {
  val g= new BigInteger("25195908475657893494027183240048398571429282126204032027777137836043662020707595556264018525880784406918290641249515082189298559149176184502808489120072844992687392807287776735971418347270261896375014971824691165077613379859095700097330459748808428401797429100642458691817195118746121515172654632282216869987549182422433637259085141865462043576798423387184774447920739934236584823824281198163815010674810451660377306056201619676256133844143603833904414952634432190114657544454178424020924616515723350778707749817125772467962926386356373289912154831438167899885040445364023527381951378636564391212010397122822120720357")
  val n = new BigInteger("28000683183178967691045327604549384273200737889408995067805870891607724272230730502935636255532870616923025398319616632616092747176572652764654213686197067978724897059080696115029373038734138776497829382209184353226737118067832131611204962781402651965177616959574821618486012482587117231822248731517319319566924904448200435653538174582490406853948962841924553170338450032277400949816544697301833261918343852251257502237396393086633583807821908438408392306669481101256146839747375312263603398130110605035799543160744850445683794785163914245886097291108437856051664713997308071095372852050222033510254270476986339211627")
  val c1 = Computer1("C1")
  val c2 = Computer1("C2")
  val compareSecretKey:State[ProgramState1,Boolean] =
    State[ProgramState1,Boolean]{ s=>
      val s1 = for {
        x <- s.c1.sharedK
        y <- s.c2.sharedK
      } yield x.equals(y)
      (s,s1.getOrElse(false))
      }

  val computeSecretKey:State[ProgramState1,Boolean] =
    State[ProgramState1,Boolean]{ s=>
      val sharedKey  =(c1:Computer1, c2:Computer1) => for {
        publicK2  <- c2.publicK
        privateK1 <- c1.privateK
        sharedK   <- Option(publicK2.modPow(privateK1,s.n))
      } yield sharedK
      val c1 = s.c1.copy(sharedK = sharedKey(s.c1,s.c2))
      val c2 = s.c2.copy(sharedK = sharedKey(s.c2,s.c1))
      (s.copy(c1,c2),false)
    }
  val generatePublicKey:State[ProgramState1,Boolean] =
    State[ProgramState1,Boolean]{ s=>
      val c1 = s.c1.copy(publicK = s.c1.privateK.map(p1=>s.g.modPow(p1,s.n)))
      val c2 = s.c2.copy(publicK = s.c2.privateK.map(p2=>s.g.modPow(p2,s.n)))
      (s.copy(c1,c2),false)
    }

  val initComputers:State[ProgramState1,Boolean] =
    State[ProgramState1,Boolean]{ s=>
      val seed = 123456
      val random =  new Random(seed)
      val c1 = s.c1.copy(privateK = Some(BigInteger.probablePrime(2048,random)))
      val c2 = s.c2.copy(privateK = Some(BigInteger.probablePrime(2048,random)))
      (s.copy(c1,c2),false)
    }

  val app2 = initComputers >> generatePublicKey>>computeSecretKey>>compareSecretKey

  val app = for {
    _ <- initComputers
    _ <- generatePublicKey
    _ <- computeSecretKey
    y <- compareSecretKey
  } yield y

  val response = app2.run(ProgramState1(c1,c2,g,n))
  val s = response.value._2
  println(s)

}
