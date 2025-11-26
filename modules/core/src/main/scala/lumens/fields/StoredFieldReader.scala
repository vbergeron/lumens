package lumens.fields

import org.apache.lucene.util.BytesRef
import org.apache.lucene.document.Document
import org.apache.lucene.document.StoredValue

case class ReadError(message: String) extends Exception(message)

trait StoredFieldReader[A]:
    def read(doc: Document, name: String): Either[ReadError, A]

    def map[B](f: A => B): StoredFieldReader[B] =
        new StoredFieldReader[B]:
            def read(doc: Document, name: String): Either[ReadError, B] =
                StoredFieldReader.this.read(doc, name).map(f)

    def emap[B](f: A => Either[ReadError, B]): StoredFieldReader[B] =
        new StoredFieldReader[B]:
            def read(doc: Document, name: String): Either[ReadError, B] =
                StoredFieldReader.this.read(doc, name).flatMap(f)

object StoredFieldReader:
    def apply[A](using ev: StoredFieldReader[A]): StoredFieldReader[A] = ev

    def readStoredValue(doc: Document, name: String): Option[StoredValue] =
        Option(doc.getField(name)).flatMap(field => Option(field.storedValue()))

    given StoredFieldReader[String] with
        def read(doc: Document, name: String): Either[ReadError, String] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getStringValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))

    given StoredFieldReader[Int] with
        def read(doc: Document, name: String): Either[ReadError, Int] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getIntValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))

    given StoredFieldReader[Long] with
        def read(doc: Document, name: String): Either[ReadError, Long] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getLongValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))

    given StoredFieldReader[Float] with
        def read(doc: Document, name: String): Either[ReadError, Float] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getFloatValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))

    given StoredFieldReader[Double] with
        def read(doc: Document, name: String): Either[ReadError, Double] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getDoubleValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))

    given StoredFieldReader[BytesRef] with
        def read(doc: Document, name: String): Either[ReadError, BytesRef] =
            readStoredValue(doc, name) match
                case Some(value) => Right(value.getBinaryValue())
                case None        => Left(ReadError(s"Field '$name' not found or not stored"))
