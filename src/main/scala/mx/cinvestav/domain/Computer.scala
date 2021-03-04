package mx.cinvestav.domain

import java.math.BigInteger

case class Computer(name:String, publicKey:Option[BigInteger]=None,privateKey:Option[BigInteger]=None)
