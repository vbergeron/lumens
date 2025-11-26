package lumens.fields

import org.apache.lucene.index.IndexableField

case class StoredFieldDesc[A](name: String, codec: StoredFieldCodec[A]) extends ReadableFieldDesc[A]:
    def field(value: A): IndexableField =
        codec.write(name, value)

    def reader: StoredFieldReader[A] = codec

    def imap[B](f: A => B)(g: B => A): StoredFieldDesc[B] =
        StoredFieldDesc[B](name, codec.imap(f)(g))

    def iemap[B](f: A => Either[ReadError, B])(g: B => A): StoredFieldDesc[B] =
        StoredFieldDesc[B](name, codec.iemap(f)(g))
