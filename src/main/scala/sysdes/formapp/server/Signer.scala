package sysdes.formapp.server

import java.security.KeyFactory
import java.security.Signature
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64

class Signer(pub64: String, prv64: String) {
  private val kf  = KeyFactory.getInstance("EC")
  private val pub = kf.generatePublic(new X509EncodedKeySpec(Base64.getDecoder.decode(pub64)))
  private val prv = kf.generatePrivate(new PKCS8EncodedKeySpec(Base64.getDecoder.decode(prv64)))

  private val sig = Signature.getInstance("SHA256withECDSA")

  def sign(src: String): String = {
    val src64 = encodeURLSafe64(src.getBytes)
    println("src: " + src)
    val sig = Signature.getInstance("SHA256withECDSA")
    sig.initSign(prv)
    sig.update(src64.getBytes)
    val sig64 = encodeURLSafe64(sig.sign)
    src64 + "." + sig64
  }

  def verify(payload: String): Option[String] = {
    val s"$data.$sign" = payload
    val sig = Signature.getInstance("SHA256withECDSA")
    sig.initVerify(pub)
    sig.update(data.getBytes)
    if (sig.verify(decodeURLSafe64(sign))) {
      Some(new String(decodeURLSafe64(data)))
    } else {
      None
    }
  }

  private def encodeURLSafe64(src: Array[Byte]): String = {
    val raw = new String(Base64.getEncoder.encode(src))
    raw
      .replace("+", "-")
      .replace("/", "_")
      .replace("=", "")
  }

  private def decodeURLSafe64(src: String): Array[Byte] = {
    val padSize = (4 - src.length % 4) % 4
    val correct = src
        .replace("-", "+")
        .replace("_", "/") + ("=" * padSize)
    Base64.getDecoder.decode(correct)
  }
}
