package lumens.extract

import org.apache.lucene.search.Query as LQuery
import org.apache.lucene.search.SortField
import org.apache.lucene.search.Sort
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery as LBooleanQuery
import org.apache.lucene.document.Document
import org.apache.lucene.search.MatchAllDocsQuery
import lumens.fields.PersistentFieldDesc
import lumens.internal.Zippable
import lumens.internal.MonadError
import scala.collection.Factory
import scala.jdk.CollectionConverters.*
import lumens.query.Query.*
import org.apache.lucene.search.TopFieldDocs
import scala.collection.mutable.Builder
import lumens.query.*

final case class Extract[A](
    output: ExtractOutput[A],
    queries: List[LQuery],
    sorts: List[SortField]
):
    /** Adds a filter to the extract. */
    def filter(f: LQuery*): Extract[A] = copy(queries = queries ++ f.toList)

    /** Adds a boolean filter to the extract. */
    def filter(b: BooleanQueryBuilder): Extract[A] = copy(queries = queries :+ b.build())

    /** Adds a sort to the extract. */
    def sort(s: SortField*): Extract[A] = copy(sorts = sorts ++ s.toList)

    /** Runs the extract query against the given IndexSearcher. */
    def run(searcher: IndexSearcher): ExtractRunner[A] =
        ExtractRunner(this, searcher)

object Extract:
    /** Creates a new extract with the given output. */
    def fetch[A](output: ExtractOutput[A]): Extract[A] =
        Extract[A](output = output, queries = List.empty, sorts = List.empty)
