package lumens.query

import lumens.query.QueryIntent
import lumens.internal.MonadError
import scala.collection.Factory
import org.apache.lucene.search.IndexSearcher

extension [C[_], A](intent: QueryIntent[C, A])
    /** Runs the query and returns the results in the given F. */
    def run[F[_]](searcher: IndexSearcher)(using F: MonadError[F]): F[intent.Output] =
        QueryRun[F](searcher).execute(intent)
