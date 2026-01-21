package lumens.extract

import lumens.internal.MonadError
import scala.collection.Factory
import scala.jdk.CollectionConverters.*
import lumens.query.Query.*
import org.apache.lucene.search.TopFieldDocs
import scala.collection.mutable.Builder
import lumens.query.*
import org.apache.lucene.search.Query as LQuery
import org.apache.lucene.search.Sort
import org.apache.lucene.search.SortField
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.document.Document

final case class ExtractRunner[A](extract: Extract[A], searcher: IndexSearcher):
    def to[F[_], C[_]](using F: MonadError[F], factory: Factory[A, C[A]]): F[C[A]] =
        F.blocking:
            val query: LQuery             = buildQuery(extract.queries)
            val sort: Sort                = buildSort(extract.sorts)
            val topDocs: TopFieldDocs     = searcher.search(query, Int.MaxValue, sort)
            val builder: Builder[A, C[A]] = factory.newBuilder

            topDocs.scoreDocs.foreach: hit =>
                val doc: Document = searcher.storedFields().document(hit.doc)
                extract.output.decode(doc) match
                    case Right(value) => builder += value
                    case Left(error)  => F.error(error)
            builder.result()
    end to

    private def buildQuery(queries: List[LQuery]): LQuery =
        queries match
            case Nil           => new MatchAllDocsQuery()
            case single :: Nil => single
            case queries       =>
                queries
                    .foldLeft(bool)((b, q) => b.must(q))
                    .build()

    private def buildSort(sorts: List[SortField]): Sort =
        if sorts.isEmpty then Sort.RELEVANCE
        else new Sort(sorts*)
