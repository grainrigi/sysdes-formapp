package sysdes.formapp.server

import java.io.{
  BufferedInputStream,
  BufferedOutputStream,
  DataInputStream,
  DataOutputStream,
  EOFException,
  File,
  FileInputStream,
  FileOutputStream,
  RandomAccessFile
}

class PersistedStore(filename: String) {
  private var store: Map[String, String] = Map()

  private val outStream = initialize

  private val buffer: Array[Byte] = new Array(256)

  def get(key: String): Option[String] = store.get(key)

  def append(key: String, value: String) = {
    store += key -> value

    // append to AOF
    val keyBytes   = key.getBytes
    val valueBytes = value.getBytes
    outStream.writeInt(keyBytes.length)
    outStream.write(keyBytes)
    outStream.writeInt(valueBytes.length)
    outStream.write(valueBytes)

    // sync everytime
    outStream.flush
  }

  def initialize(): DataOutputStream = {
    println("initializing")
    val f: File = new File(filename)
    if (f.exists() && f.length > 4) {
      println("found file")
      val inStream  = new RandomAccessFile(f, "rw")
      val signature = inStream.readInt
      if (signature == 0x414f4620) /* AOF  */ {
        println("is AOF File")
        var startPos: Long = 0
        try {
          while (true) {
            startPos = inStream.getFilePointer
            // Read key
            val keyLength = inStream.readInt
            val keyBuf    = new Array[Byte](keyLength)
            inStream.readFully(keyBuf)
            val key = new String(keyBuf)
            // Read value
            val valueLength = inStream.readInt
            val valueBuf    = new Array[Byte](valueLength)
            inStream.readFully(valueBuf)
            val value = new String(valueBuf)
            // Store it
            store += key -> value
          }
        } catch {
          case e: EOFException => {
            // end of file
            inStream.setLength(startPos)
            inStream.close
          }
        }
        // create output stream
        return new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename, true)))
      } else {
        inStream.close
      }
    }

    // File does not exist / not an AOF file

    // write only signatures
    val raf = new RandomAccessFile(f, "rw")
    raf.seek(0)
    raf.writeInt(0x414f4620)
    raf.setLength(4)
    raf.close

    // create output stream
    new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename, true)))
  }
}
