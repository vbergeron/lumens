package lumens

import org.apache.lucene.store.*
import cats.effect.Resource
import cats.effect.kernel.Sync
import java.nio.file.Paths
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.StoredValue
import org.apache.lucene.util.BytesRef

object Directories:
    def memory[F[_]: Sync]: Resource[F, Directory] =
        Resource.fromAutoCloseable(Sync[F].delay(ByteBuffersDirectory()))

    def disk[F[_]: Sync](path: String): Resource[F, Directory] =
        Resource.fromAutoCloseable(Sync[F].delay(FSDirectory.open(Paths.get(path))))