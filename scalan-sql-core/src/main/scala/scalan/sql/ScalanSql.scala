package scalan.sql

import scalan.{AnalyzingExp, _}

/**
  * Use this traits for the top level scalan.sql extensions, customizations and overrides
  */
trait ScalanSql extends ScalanDsl with ItersDsl with Analyzing with SqlSlicing {
}
trait ScalanSqlStd extends ScalanDslStd with ItersDslStd with AnalyzingStd with SqlSlicingStd with ScalanSql
trait ScalanSqlExp extends ScalanDslExp with ItersDslExp with AnalyzingExp with SqlSlicingExp with ScalanSql {

}

