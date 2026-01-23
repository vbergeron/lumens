package lumens.query

import lumens.internal.MonadError
import lumens.query.QueryIntent
import org.apache.lucene.search.IndexSearcher

/** A query run is a context for running a query. It is used to execute a query intent and return the results in the
  * given monad.
  */
sealed trait QueryRun[F[_]](searcher: IndexSearcher):
    def execute(intent: QueryIntent[?, ?])(using F: MonadError[F]): F[intent.Output] =
        F.flatMap(F.blocking(intent.getResults(searcher))) {
            case Right(results) => F.unit(results)
            case Left(err)      => F.error(err)
        }

object QueryRun:
    def apply[F[_]](indexSearcher: IndexSearcher)(using F: MonadError[F]): QueryRun[F] =
        new QueryRun[F](indexSearcher) {}
