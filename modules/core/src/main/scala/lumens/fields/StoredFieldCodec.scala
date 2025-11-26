package lumens.fields

trait StoredFieldCodec[A] extends StoredFieldReader[A], StoredFieldWriter[A]:
    def imap[B](f: A => B)(g: B => A): StoredFieldCodec[B] =
        val reader: StoredFieldReader[B] = StoredFieldCodec.this.map(f)
        val writer: StoredFieldWriter[B] = StoredFieldCodec.this.contramap(g)
        new StoredFieldCodec[B]:
            export reader.read
            export writer.write

    def iemap[B](f: A => Either[ReadError, B])(g: B => A): StoredFieldCodec[B] =
        val reader: StoredFieldReader[B] = StoredFieldCodec.this.emap(f)
        val writer: StoredFieldWriter[B] = StoredFieldCodec.this.contramap(g)
        new StoredFieldCodec[B]:
            export reader.read
            export writer.write
